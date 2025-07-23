# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(forecast)
library(plotly)

# --- Data Preparation ---
# (Same as before)
african_countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde",
  "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo",
  "Côte d'Ivoire", "Democratic Republic of the Congo", "Djibouti", "Egypt",
  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia",
  "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya",
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco",
  "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe",
  "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
  "Sudan", "Togo", "Tunisia", "Uganda", "United Republic of Tanzania", "Zambia",
  "Zimbabwe"
)

# Assume crop_yield_2000_2023 is loaded
africa_data <- crop_yield_2000_2023 %>%
  filter(Area %in% african_countries) %>%
  select(Area, Item, Year, Value, `Flag Description`)

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = "Africa Crop Yield Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"))
    ),
    # Interactive Controls
    selectInput("selected_item", "Select Crop:",
                choices = c("All Crops" = "All Crops", sort(unique(africa_data$Item))),
                selected = "All Crops"),
    selectInput("selected_country", "Select Country:",
                choices = c("All Countries" = "All Countries", sort(unique(africa_data$Area))),
                selected = "All Countries"),
    sliderInput("year_range", "Year Range:",
                min = 2000, max = 2023, value = c(2000, 2023))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("kpi_avg_yield"),
                valueBoxOutput("kpi_trend") # New KPI for trend
              ),
              fluidRow(
                box(plotOutput("ts_plot"), width = 6),
                box(plotlyOutput("top_crops_plot"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("top_countries_plot"), width = 6),
                box(plotOutput("yield_dist_plot"), width = 6)
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(plotlyOutput("boxplot_crop"), width = 6),
                box(tableOutput("outliers_table"), width = 6)
              )
      ),
      tabItem(tabName = "forecast",
              fluidRow(
                valueBoxOutput("forecast_2024"),
                valueBoxOutput("forecast_2025"),
                valueBoxOutput("forecast_2026")
              ),
              fluidRow(
                box(plotlyOutput("crop_forecast_plot"), width = 12)
              )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # 1. REACTIVE: Filtered data based on BOTH crop and country selection
  filtered_data <- reactive({
    data <- africa_data
    
    # Apply crop filter
    if (input$selected_item != "All Crops") {
      data <- data %>% filter(Item == input$selected_item)
    }
    
    # Apply country filter
    if (input$selected_country != "All Countries") {
      data <- data %>% filter(Area == input$selected_country)
    }
    
    # Apply year filter
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  # 2. REACTIVE: Data for Top 10 Crops (depends on country selection)
  top_crops_data <- reactive({
    data <- africa_data
    # Filter only by country, not by crop
    if (input$selected_country != "All Countries") {
      data <- data %>% filter(Area == input$selected_country)
    }
    # Calculate average yield for each crop
    data %>%
      group_by(Item) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Avg_Yield)) %>%
      slice_head(n = 10)
  })
  
  # 3. REACTIVE: Data for Top 10 Countries (depends on crop selection)
  top_countries_data <- reactive({
    data <- africa_data
    # Filter only by crop, not by country
    if (input$selected_item != "All Crops") {
      data <- data %>% filter(Item == input$selected_item)
    }
    # Calculate average yield for each country
    data %>%
      group_by(Area) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Avg_Yield)) %>%
      slice_head(n = 10)
  })
  
  # --- KPIs ---
  # KPI 1: Average Yield
  output$kpi_avg_yield <- renderValueBox({
    data <- filtered_data()
    avg_yield <- round(mean(data$Value, na.rm = TRUE), 2)
    
    subtitle_text <- if (input$selected_item == "All Crops" && input$selected_country == "All Countries") {
      "Avg Yield (All Crops, All Countries)"
    } else if (input$selected_item != "All Crops" && input$selected_country == "All Countries") {
      paste("Avg Yield for", input$selected_item)
    } else if (input$selected_item == "All Crops" && input$selected_country != "All Countries") {
      paste("Avg Yield in", input$selected_country)
    } else {
      paste("Avg Yield for", input$selected_item, "in", input$selected_country)
    }
    
    valueBox(
      paste0(avg_yield, " kg/ha"),
      subtitle = subtitle_text,
      icon = icon("chart-line"),
      color = "teal"
    )
  })
  
  # KPI 2: Trend (New)
  output$kpi_trend <- renderValueBox({
    data <- filtered_data()
    # Group by Year to get average per year
    yearly_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    # Only calculate trend if we have at least 2 years of data
    if (nrow(yearly_data) < 2) {
      trend_text <- "N/A"
      trend_icon <- icon("question")
      trend_color <- "aqua"
    } else {
      # Fit a simple linear model to get the slope
      model <- lm(Avg_Yield ~ Year, data = yearly_data)
      slope <- coef(model)["Year"]
      
      if (slope > 0.1) {
        trend_text <- "Increasing"
        trend_icon <- icon("arrow-up")
        trend_color <- "green"
      } else if (slope < -0.1) {
        trend_text <- "Decreasing"
        trend_icon <- icon("arrow-down")
        trend_color <- "red"
      } else {
        trend_text <- "Stable"
        trend_icon <- icon("equals")
        trend_color <- "blue"
      }
    }
    
    valueBox(
      trend_text,
      subtitle = "Yield Trend",
      icon = trend_icon,
      color = trend_color
    )
  })
  
  # --- Overview Tab: Time Series Plot ---
  # (Unchanged)
  output$ts_plot <- renderPlot({
    req(input$selected_item, input$selected_country)
    data <- filtered_data()
    
    plot_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    title_text <- if (input$selected_item == "All Crops" && input$selected_country == "All Countries") {
      "Africa-Wide Average Yield (All Crops)"
    } else if (input$selected_item != "All Crops" && input$selected_country == "All Countries") {
      paste("Yield Trend for", input$selected_item, "in Africa")
    } else if (input$selected_item == "All Crops" && input$selected_country != "All Countries") {
      paste("Yield Trend in", input$selected_country, "(All Crops)")
    } else {
      paste("Yield Trend for", input$selected_item, "in", input$selected_country)
    }
    
    ggplot(plot_data, aes(x = Year, y = Avg_Yield)) +
      geom_line(color = "#1f77b4", linewidth = 1) +
      geom_point(color = "#1f77b4", size = 2) +
      labs(title = title_text, x = "Year", y = "Yield (kg/ha)") +
      theme_minimal() +
      ylim(0, max(plot_data$Avg_Yield, na.rm = TRUE) * 1.1)
  })
  
  # --- Overview Tab: Top 10 Crops Plot ---
  # (Unchanged)
  output$top_crops_plot <- renderPlotly({
    req(input$selected_country)
    
    if (input$selected_item != "All Crops") {
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, label = "Select 'All Crops' to see top crops", 
                 size = 8, color = "grey") +
        theme_void()
      return(ggplotly(p))
    }
    
    data <- top_crops_data()
    p <- ggplot(data, aes(x = reorder(Item, Avg_Yield), y = Avg_Yield)) + # Removed 'fill'
      geom_bar(stat = "identity", fill = "#2ca02c") + # Set a fixed fill color
      coord_flip() +
      labs(title = paste("Top 10 Crops in", input$selected_country), 
           x = "Crop", y = "Average Yield (kg/ha)") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # --- Overview Tab: Top 10 Countries Plot ---
  # (Unchanged)
  output$top_countries_plot <- renderPlotly({
    req(input$selected_item)
    
    if (input$selected_country != "All Countries") {
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, label = "Select 'All Countries' to see top countries", 
                 size = 8, color = "grey") +
        theme_void()
      return(ggplotly(p))
    }
    
    data <- top_countries_data()
    p <- ggplot(data, aes(x = reorder(Area, Avg_Yield), y = Avg_Yield)) + # Removed 'fill'
      geom_bar(stat = "identity", fill = "#1f77b4") + # Set a fixed fill color
      coord_flip() +
      labs(title = paste("Top 10 Countries for", input$selected_item), 
           x = "Country", y = "Average Yield (kg/ha)") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # --- Overview Tab: Yield Distribution Histogram ---
  # (Unchanged)
  output$yield_dist_plot <- renderPlot({
    req(input$selected_item, input$selected_country)
    data <- filtered_data()
    
    title_text <- "Yield Distribution"
    if (input$selected_item != "All Crops") title_text <- paste(title_text, "for", input$selected_item)
    if (input$selected_country != "All Countries") title_text <- paste(title_text, "in", input$selected_country)
    
    ggplot(data, aes(x = Value)) +
      geom_histogram(bins = 30, fill = "#1f77b4", color = "black") +
      labs(title = title_text, x = "Yield (kg/ha)", y = "Count") +
      theme_minimal()
  })
  
  # --- Analysis Tab: Boxplot for the Selected Crop ---
  # (Fixed: Removed 'fill' aesthetic)
  output$boxplot_crop <- renderPlotly({
    req(input$selected_item)
    data <- africa_data %>% filter(Item == input$selected_item)
    
    p <- ggplot(data, aes(y = Value)) +
      geom_boxplot(fill = "#1f77b4", width = 0.5) + # Fixed color, no fill=aes()
      labs(title = paste("Yield Distribution for", input$selected_item), 
           y = "Yield (kg/ha)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- Analysis Tab: Outliers Table ---
  # (Unchanged)
  output$outliers_table <- renderTable({
    req(input$selected_item, input$selected_country)
    data <- filtered_data()
    outliers <- data %>%
      arrange(desc(Value)) %>%
      slice_head(n = 5) %>%
      select(Area, Year, Value) %>%
      rename(Country = Area)
    outliers
  })
  
  # --- Forecast Tab: Forecast for the Selected Crop ---
  # (Unchanged from previous version)
  crop_forecast_data <- reactive({
    req(input$selected_item, input$selected_country)
    data <- filtered_data()
    
    ts_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(ts_data) < 3) {
      return(NULL)
    }
    
    model <- lm(Avg_Yield ~ Year, data = ts_data)
    future_years <- data.frame(Year = 2024:2028)
    predictions <- predict(model, newdata = future_years, interval = "prediction")
    
    bind_rows(
      data.frame(Year = ts_data$Year, Yield = ts_data$Avg_Yield, Type = "Historical"),
      data.frame(Year = future_years$Year, Yield = predictions[,"fit"], Type = "Forecast")
    )
  })
  
  # --- Forecast Tab: Forecast KPIs (Fixed: Complete code) ---
  # Forecast 2024
  output$forecast_2024 <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data) || !("Forecast" %in% data$Type)) {
      valueBox("N/A", subtitle = "2024 Forecast", icon = icon("calendar"), color = "aqua")
    } else {
      forecast_val <- data$Yield[data$Year == 2024 & data$Type == "Forecast"]
      valueBox(paste0(round(forecast_val, 0), " kg/ha"), 
               subtitle = "2024 Forecast", 
               icon = icon("calendar"), 
               color = "orange")
    }
  })
  
  # Forecast 2025
  output$forecast_2025 <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data) || !("Forecast" %in% data$Type)) {
      valueBox("N/A", subtitle = "2025 Forecast", icon = icon("calendar"), color = "aqua")
    } else {
      forecast_val <- data$Yield[data$Year == 2025 & data$Type == "Forecast"]
      valueBox(paste0(round(forecast_val, 0), " kg/ha"), 
               subtitle = "2025 Forecast", 
               icon = icon("calendar"), 
               color = "orange")
    }
  })
  
  # Forecast 2026
  output$forecast_2026 <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data) || !("Forecast" %in% data$Type)) {
      valueBox("N/A", subtitle = "2026 Forecast", icon = icon("calendar"), color = "aqua")
    } else {
      forecast_val <- data$Yield[data$Year == 2026 & data$Type == "Forecast"]
      valueBox(paste0(round(forecast_val, 0), " kg/ha"), 
               subtitle = "2026 Forecast", 
               icon = icon("calendar"), 
               color = "orange")
    }
  })
  
  # --- Forecast Tab: Forecast Plot ---
  # (Unchanged)
  output$crop_forecast_plot <- renderPlotly({
    data <- crop_forecast_data()
    if (is.null(data)) {
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, label = "Not enough data for forecasting", 
                 size = 8, color = "red") +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(data, aes(x = Year, y = Yield, color = Type)) +
      geom_line(data = ~ filter(.x, Type == "Historical"), linewidth = 1.2) +
      geom_line(data = ~ filter(.x, Type == "Forecast"), linetype = "dashed", linewidth = 1.2) +
      geom_point(data = ~ filter(.x, Type == "Historical"), size = 2) +
      geom_point(data = ~ filter(.x, Type == "Forecast"), size = 2) +
      scale_color_manual(values = c("Historical" = "darkgreen", "Forecast" = "red")) +
      labs(title = paste("Forecast for", input$selected_item, "in", input$selected_country),
           x = "Year", y = "Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
