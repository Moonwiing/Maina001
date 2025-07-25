library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(forecast)
library(plotly)
library(DT)
library(readr)
library(knitr)
library(rmarkdown)

# Load data
crop_yield_2000_2023 <- tryCatch({
  read_csv("crop_yield_2000_2023.csv")
}, error = function(e) {
  stop("Error loading data file: ", e$message, ". Ensure 'crop_yield_2000_2023.csv' is in the app directory.")
})

# African countries list
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

# --- UI ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Africa Crop Yield Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Crop Risk", tabName = "crop_risk", icon = icon("exclamation-triangle")),
      menuItem("Report", tabName = "report", icon = icon("file-alt"))
    ),
    selectInput("selected_item", "Select Crops:",
                choices = NULL, multiple = TRUE, selected = "All Crops"),
    selectInput("selected_countries", "Select Countries (up to 2):",
                choices = NULL, multiple = TRUE, selected = "All Countries"),
    sliderInput("year_range", "Year Range:",
                min = 2000, max = 2023, value = c(2000, 2023), sep = ""),
    downloadButton("download_report", "Download Report", style = "margin: 10px;")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f5f6fa; }
      .main-sidebar { background-color: #2c3e50; }
      .box { 
        border-radius: 8px; 
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        background-color: white;
        padding: 15px;
      }
      .shiny-plot-output { width: 100% !important; height: 400px !important; }
      .value-box { border-radius: 8px; }
      .sidebar-menu li a { color: white !important; }
      .selectize-input { border-radius: 4px; padding: 5px; }
      .download-button { background-color: #3498db; color: white; border-radius: 4px; }
    "))),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("kpi_avg_yield", width = 3),
                valueBoxOutput("kpi_africa_avg", width = 3),
                valueBoxOutput("kpi_trend", width = 3),
                valueBoxOutput("kpi_percent_increase", width = 3)
              ),
              uiOutput("overview_plots"),
              fluidRow(
                box(plotlyOutput("trend_plot"), width = 12, title = "Yield Trend Over Time")
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(plotlyOutput("boxplot_crop"), width = 6, title = "Yield Distribution"),
                box(tableOutput("outliers_table"), width = 6, title = "Top Outliers")
              ),
              fluidRow(
                box(DTOutput("comparison_table"), width = 12, title = "Crop Performance Comparison")
              )
      ),
      tabItem(tabName = "forecast",
              fluidRow(
                valueBoxOutput("forecast_2024", width = 3),
                valueBoxOutput("forecast_2025", width = 3),
                valueBoxOutput("forecast_2026", width = 3),
                valueBoxOutput("forecast_mape", width = 3)
              ),
              fluidRow(
                box(plotlyOutput("crop_forecast_plot"), width = 12, title = "Yield Forecast")
              )
      ),
      tabItem(tabName = "crop_risk",
              fluidRow(
                box(DTOutput("crops_in_danger_table"), width = 12, title = "Crops at Risk")
              )
      ),
      tabItem(tabName = "report",
              fluidRow(
                box(htmlOutput("report_preview"), width = 12, title = "Performance Report Preview")
              )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive: Filter for African countries
  africa_data <- reactive({
    crop_yield_2000_2023 %>%
      filter(Area %in% african_countries) %>%
      select(Area, Item, Year, Value) %>%
      mutate(
        Area = as.character(Area),
        Item = as.character(Item),
        Year = as.integer(Year),
        Value = as.numeric(Value)
      )
  })
  
  # Update selectInput choices
  observe({
    data <- africa_data()
    updateSelectInput(session, "selected_item",
                      choices = c("All Crops" = "All Crops", sort(unique(data$Item))),
                      selected = "All Crops")
    updateSelectInput(session, "selected_countries",
                      choices = c("All Countries" = "All Countries", sort(unique(data$Area))),
                      selected = "All Countries")
  })
  
  # Reactive: Filtered data
  filtered_data <- reactive({
    data <- africa_data()
    
    if (!("All Crops" %in% input$selected_item)) {
      data <- data %>% filter(Item %in% input$selected_item)
    }
    
    if (!("All Countries" %in% input$selected_countries)) {
      data <- data %>% filter(Area %in% input$selected_countries)
    }
    
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  # Reactive: Africa-wide average yield
  africa_avg_yield <- reactive({
    africa_data() %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2]) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE)) %>%
      pull(Avg_Yield)
  })
  
  # Reactive: Top 10 crops
  top_crops_data <- reactive({
    data <- africa_data()
    if (!("All Countries" %in% input$selected_countries)) {
      data <- data %>% filter(Area %in% input$selected_countries)
    }
    data %>%
      group_by(Item) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Avg_Yield)) %>%
      slice_head(n = 10)
  })
  
  # Reactive: Top 10 countries
  top_countries_data <- reactive({
    data <- africa_data()
    if (!("All Crops" %in% input$selected_item)) {
      data <- data %>% filter(Item %in% input$selected_item)
    }
    data %>%
      group_by(Area) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Avg_Yield)) %>%
      slice_head(n = 10)
  })
  
  # Reactive: Bottom 10 countries
  bottom_countries_data <- reactive({
    data <- africa_data()
    if (!("All Crops" %in% input$selected_item)) {
      data <- data %>% filter(Item %in% input$selected_item)
    }
    data %>%
      group_by(Area) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(Avg_Yield) %>%
      slice_head(n = 10)
  })
  
  # Reactive: Crops in danger
  crops_in_danger <- reactive({
    data <- africa_data()
    if (!("All Countries" %in% input$selected_countries)) {
      data <- data %>% filter(Area %in% input$selected_countries)
    }
    
    yield_quantiles <- quantile(data$Value, c(0.1, 0.25), na.rm = TRUE)
    
    data %>%
      group_by(Area, Item) %>%
      summarise(
        CAGR = ((mean(Value[Year == max(Year)], na.rm = TRUE) / 
                   mean(Value[Year == min(Year)], na.rm = TRUE))^(1/(max(Year) - min(Year))) - 1) * 100,
        Avg_Yield = mean(Value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Risk_Level = case_when(
        CAGR < -1 | Avg_Yield < yield_quantiles[1] ~ "High",
        CAGR < 0 | Avg_Yield < yield_quantiles[2] ~ "Moderate",
        TRUE ~ "Low"
      )) %>%
      filter(Risk_Level %in% c("High", "Moderate")) %>%
      arrange(CAGR, Avg_Yield) %>%
      slice_head(n = 10)
  })
  
  # KPI: Average Yield
  output$kpi_avg_yield <- renderValueBox({
    data <- filtered_data()
    avg_yield <- round(mean(data$Value, na.rm = TRUE), 2)
    
    subtitle_text <- if ("All Crops" %in% input$selected_item && "All Countries" %in% input$selected_countries) {
      "Avg Yield (All Crops, All Countries)"
    } else if (!("All Crops" %in% input$selected_item) && "All Countries" %in% input$selected_countries) {
      paste("Avg Yield for", paste(input$selected_item, collapse = ", "))
    } else if ("All Crops" %in% input$selected_item && !("All Countries" %in% input$selected_countries)) {
      paste("Avg Yield in", paste(input$selected_countries, collapse = ", "))
    } else {
      paste("Avg Yield for", paste(input$selected_item, collapse = ", "), "in", paste(input$selected_countries, collapse = ", "))
    }
    
    valueBox(
      paste0(avg_yield, " kg/ha"),
      subtitle = subtitle_text,
      icon = icon("chart-line"),
      color = "teal"
    )
  })
  
  # KPI: Africa-Wide Average Yield
  output$kpi_africa_avg <- renderValueBox({
    avg_yield <- round(africa_avg_yield(), 2)
    valueBox(
      paste0(avg_yield, " kg/ha"),
      subtitle = "Africa-Wide Avg Yield",
      icon = icon("globe-africa"),
      color = "purple"
    )
  })
  
  # KPI: Trend
  output$kpi_trend <- renderValueBox({
    data <- filtered_data()
    yearly_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(yearly_data) < 2) {
      valueBox("N/A", subtitle = "Yield Trend", icon = icon("question"), color = "aqua")
    } else {
      model <- lm(Avg_Yield ~ Year, data = yearly_data)
      slope <- coef(model)["Year"]
      
      trend_text <- if (slope > 0.1) "Increasing" else if (slope < -0.1) "Decreasing" else "Stable"
      trend_icon <- if (slope > 0.1) icon("arrow-up") else if (slope < -0.1) icon("arrow-down") else icon("equals")
      trend_color <- if (slope > 0.1) "green" else if (slope < -0.1) "red" else "blue"
      
      valueBox(trend_text, subtitle = "Yield Trend", icon = trend_icon, color = trend_color)
    }
  })
  
  # KPI: Percentage Increase (CAGR)
  output$kpi_percent_increase <- renderValueBox({
    data <- filtered_data()
    yearly_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(yearly_data) < 2) {
      valueBox("N/A", subtitle = "CAGR", icon = icon("question"), color = "aqua")
    } else {
      cagr <- ((yearly_data$Avg_Yield[nrow(yearly_data)] / 
                  yearly_data$Avg_Yield[1])^(1/(nrow(yearly_data)-1)) - 1) * 100
      valueBox(
        paste0(round(cagr, 2), "%"),
        subtitle = "CAGR (Selected Period)",
        icon = icon("percentage"),
        color = "yellow"
      )
    }
  })
  
  # Overview: Dynamic Plot Layout
  output$overview_plots <- renderUI({
    show_top_crops <- "All Crops" %in% input$selected_item
    show_countries <- "All Countries" %in% input$selected_countries
    
    if (show_top_crops && show_countries) {
      fluidRow(
        box(plotlyOutput("top_crops_plot"), width = 6, title = "Top 10 Crops"),
        box(plotlyOutput("top_countries_plot"), width = 6, title = "Top 10 Countries"),
        box(plotlyOutput("bottom_countries_plot"), width = 6, title = "Bottom 10 Countries")
      )
    } else if (show_top_crops) {
      fluidRow(
        box(plotlyOutput("top_crops_plot"), width = 12, title = "Top 10 Crops")
      )
    } else if (show_countries) {
      fluidRow(
        box(plotlyOutput("top_countries_plot"), width = 6, title = "Top 10 Countries"),
        box(plotlyOutput("bottom_countries_plot"), width = 6, title = "Bottom 10 Countries")
      )
    } else {
      fluidRow()
    }
  })
  
  # Overview: Trend Plot
  output$trend_plot <- renderPlotly({
    req(input$selected_item, input$selected_countries)
    data <- filtered_data()
    
    plot_data <- data %>%
      group_by(Year, Area) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    title_text <- if ("All Crops" %in% input$selected_item && "All Countries" %in% input$selected_countries) {
      "Africa-Wide Average Yield (All Crops)"
    } else if (!("All Crops" %in% input$selected_item) && "All Countries" %in% input$selected_countries) {
      paste("Yield Trend for", paste(input$selected_item, collapse = ", "), "in Africa")
    } else if ("All Crops" %in% input$selected_item && !("All Countries" %in% input$selected_countries)) {
      paste("Yield Trend in", paste(input$selected_countries, collapse = ", "), "(All Crops)")
    } else {
      paste("Yield Trend for", paste(input$selected_item, collapse = ", "), "in", paste(input$selected_countries, collapse = ", "))
    }
    
    p <- ggplot(plot_data, aes(x = Year, y = Avg_Yield, color = Area)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = title_text, x = "Year", y = "Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      ylim(0, max(plot_data$Avg_Yield, na.rm = TRUE) * 1.1)
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Overview: Top 10 Crops Plot
  output$top_crops_plot <- renderPlotly({
    req(input$selected_countries)
    
    data <- top_crops_data()
    p <- ggplot(data, aes(x = reorder(Item, Avg_Yield), y = Avg_Yield, fill = Item)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Top 10 Crops in", paste(input$selected_countries, collapse = ", ")), 
           x = "Crop", y = "Average Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Overview: Top 10 Countries Plot
  output$top_countries_plot <- renderPlotly({
    req(input$selected_item)
    
    data <- top_countries_data()
    p <- ggplot(data, aes(x = reorder(Area, Avg_Yield), y = Avg_Yield, fill = Area)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Top 10 Countries for", paste(input$selected_item, collapse = ", ")), 
           x = "Country", y = "Average Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Overview: Bottom 10 Countries Plot
  output$bottom_countries_plot <- renderPlotly({
    req(input$selected_item)
    
    data <- bottom_countries_data()
    p <- ggplot(data, aes(x = reorder(Area, -Avg_Yield), y = Avg_Yield, fill = Area)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Bottom 10 Countries for", paste(input$selected_item, collapse = ", ")), 
           x = "Country", y = "Average Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Analysis: Boxplot for Crop
  output$boxplot_crop <- renderPlotly({
    req(input$selected_item)
    data <- filtered_data()
    
    p <- ggplot(data, aes(y = Value, x = Area, fill = Area)) +
      geom_boxplot() +
      labs(title = paste("Yield Distribution for", paste(input$selected_item, collapse = ", ")), 
           x = "Country", y = "Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Analysis: Outliers Table
  output$outliers_table <- renderTable({
    req(input$selected_item, input$selected_countries)
    data <- filtered_data()
    outliers <- data %>%
      arrange(desc(Value)) %>%
      slice_head(n = 5) %>%
      select(Area, Item, Year, Value) %>%
      rename(Country = Area, Crop = Item)
    outliers
  })
  
  # Analysis: Comparison Table
  output$comparison_table <- DT::renderDT({
    req(input$selected_item, input$selected_countries)
    data <- filtered_data()
    
    table_data <- data %>%
      group_by(Area, Item) %>%
      summarise(
        Avg_Yield = mean(Value, na.rm = TRUE),
        CAGR = ((mean(Value[Year == max(Year)], na.rm = TRUE) / 
                   mean(Value[Year == min(Year)], na.rm = TRUE))^(1/(max(Year) - min(Year))) - 1) * 100,
        Volatility = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      arrange(CAGR) %>%
      mutate(
        Avg_Yield = round(Avg_Yield, 2),
        CAGR = round(CAGR, 2),
        Volatility = round(Volatility, 2)
      )
    
    DT::datatable(table_data, 
                  options = list(pageLength = 10, autoWidth = TRUE),
                  rownames = FALSE)
  })
  
  # Forecast: ARIMA Forecast Data
  crop_forecast_data <- reactive({
    req(input$selected_item, input$selected_countries)
    data <- filtered_data()
    
    ts_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      filter(Year <= 2023)
    
    if (nrow(ts_data) < 3) {
      return(NULL)
    }
    
    ts <- ts(ts_data$Avg_Yield, start = min(ts_data$Year), frequency = 1)
    train_ts <- window(ts, start = 2000, end = 2016)
    test_ts <- window(ts, start = 2017, end = 2023)
    
    if (length(train_ts) < 3) {
      return(NULL)
    }
    
    model <- auto.arima(train_ts)
    forecast <- forecast(model, h = 5)
    
    test_forecast <- forecast(model, h = length(test_ts))
    mape <- mean(abs((as.numeric(test_ts) - as.numeric(test_forecast$mean)) / as.numeric(test_ts)) * 100, na.rm = TRUE)
    
    list(
      historical = data.frame(Year = ts_data$Year, Yield = ts_data$Avg_Yield, Type = "Historical"),
      forecast = data.frame(
        Year = 2024:2028,
        Yield = as.numeric(forecast$mean),
        Lower = as.numeric(forecast$lower[, 2]),
        Upper = as.numeric(forecast$upper[, 2]),
        Type = "Forecast"
      ),
      mape = mape
    )
  })
  
  # Forecast: KPIs
  output$forecast_2024 <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data)) {
      valueBox("N/A", subtitle = "2024 Forecast", icon = icon("calendar"), color = "aqua")
    } else {
      forecast_val <- data$forecast$Yield[data$forecast$Year == 2024]
      valueBox(paste0(round(forecast_val, 0), " kg/ha"), 
               subtitle = "2024 Forecast", 
               icon = icon("calendar"), 
               color = "orange")
    }
  })
  
  output$forecast_2025 <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data)) {
      valueBox("N/A", subtitle = "2025 Forecast", icon = icon("calendar"), color = "aqua")
    } else {
      forecast_val <- data$forecast$Yield[data$forecast$Year == 2025]
      valueBox(paste0(round(forecast_val, 0), " kg/ha"), 
               subtitle = "2025 Forecast", 
               icon = icon("calendar"), 
               color = "orange")
    }
  })
  
  output$forecast_2026 <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data)) {
      valueBox("N/A", subtitle = "2026 Forecast", icon = icon("calendar"), color = "aqua")
    } else {
      forecast_val <- data$forecast$Yield[data$forecast$Year == 2026]
      valueBox(paste0(round(forecast_val, 0), " kg/ha"), 
               subtitle = "2026 Forecast", 
               icon = icon("calendar"), 
               color = "orange")
    }
  })
  
  output$forecast_mape <- renderValueBox({
    data <- crop_forecast_data()
    if (is.null(data)) {
      valueBox("N/A", subtitle = "Model MAPE", icon = icon("chart-line"), color = "aqua")
    } else {
      valueBox(paste0(round(data$mape, 2), "%"), 
               subtitle = "Model MAPE", 
               icon = icon("chart-line"), 
               color = "purple")
    }
  })
  
  # Forecast: Plot with Smooth Line
  output$crop_forecast_plot <- renderPlotly({
    data <- crop_forecast_data()
    if (is.null(data)) {
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, label = "Not enough data for forecasting", 
                 size = 8, color = "red") +
        theme_void()
      return(ggplotly(p))
    }
    
    plot_data <- bind_rows(data$historical, data$forecast)
    p <- ggplot(plot_data, aes(x = Year, y = Yield, color = Type)) +
      geom_line(data = ~ filter(.x, Type == "Historical"), size = 1.2) +
      geom_point(data = ~ filter(.x, Type == "Historical"), size = 2) +
      geom_point(data = ~ filter(.x, Type == "Forecast"), size = 2) +
      geom_smooth(data = ~ filter(.x, Type == "Forecast"), method = "loess", se = FALSE, linetype = "dashed", size = 1.2) +
      geom_ribbon(data = ~ filter(.x, Type == "Forecast"), 
                  aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "red") +
      scale_color_manual(values = c("Historical" = "darkgreen", "Forecast" = "red")) +
      labs(title = paste("Forecast for", paste(input$selected_item, collapse = ", "), "in", paste(input$selected_countries, collapse = ", ")),
           x = "Year", y = "Yield (kg/ha)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Crop Risk: Table with Colors
  output$crops_in_danger_table <- DT::renderDT({
    data <- crops_in_danger() %>%
      rename(Country = Area, Crop = Item, `Average Yield (kg/ha)` = Avg_Yield, `CAGR (%)` = CAGR, `Risk Level` = Risk_Level) %>%
      mutate(`Average Yield (kg/ha)` = round(`Average Yield (kg/ha)`, 2), `CAGR (%)` = round(`CAGR (%)`, 2))
    
    DT::datatable(data, 
                  options = list(pageLength = 10, autoWidth = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle(
        "Risk Level",
        target = "cell",
        backgroundColor = DT::styleEqual(
          c("High", "Moderate", "Low"),
          c("red", "yellow", "green")
        )
      ) %>%
      DT::formatStyle(
        "CAGR (%)",
        backgroundColor = DT::styleInterval(c(-1, 0), c("red", "yellow", "green"))
      ) %>%
      DT::formatStyle(
        "Average Yield (kg/ha)",
        backgroundColor = DT::styleInterval(
          quantile(africa_data()$Value, c(0.1, 0.25), na.rm = TRUE),
          c("red", "yellow", "green")
        )
      )
  })
  
  # Report: Generate markdown content
  report_content <- reactive({
    data <- filtered_data()
    avg_yield <- round(mean(data$Value, na.rm = TRUE), 2)
    africa_avg <- round(africa_avg_yield(), 2)
    yearly_data <- data %>%
      group_by(Year) %>%
      summarise(Avg_Yield = mean(Value, na.rm = TRUE), .groups = "drop")
    
    trend_text <- if (nrow(yearly_data) >= 2) {
      model <- lm(Avg_Yield ~ Year, data = yearly_data)
      slope <- coef(model)["Year"]
      if (slope > 0.1) "increasing" else if (slope < -0.1) "decreasing" else "stable"
    } else {
      "insufficient data"
    }
    
    cagr <- if (nrow(yearly_data) >= 2) {
      round(((yearly_data$Avg_Yield[nrow(yearly_data)] / yearly_data$Avg_Yield[1])^(1/(nrow(yearly_data)-1)) - 1) * 100, 2)
    } else {
      "N/A"
    }
    
    top_crops <- top_crops_data() %>%
      mutate(Avg_Yield = round(Avg_Yield, 2)) %>%
      slice_head(n = 3)
    top_countries <- top_countries_data() %>%
      mutate(Avg_Yield = round(Avg_Yield, 2)) %>%
      slice_head(n = 3)
    bottom_countries <- bottom_countries_data() %>%
      mutate(Avg_Yield = round(Avg_Yield, 2)) %>%
      slice_head(n = 3)
    outliers <- filtered_data() %>%
      arrange(desc(Value)) %>%
      slice_head(n = 3) %>%
      select(Country = Area, Crop = Item, Year, Value) %>%
      mutate(Value = round(Value, 2))
    comparison <- filtered_data() %>%
      group_by(Area, Item) %>%
      summarise(
        Avg_Yield = round(mean(Value, na.rm = TRUE), 2),
        CAGR = round(((mean(Value[Year == max(Year)], na.rm = TRUE) / 
                         mean(Value[Year == min(Year)], na.rm = TRUE))^(1/(max(Year) - min(Year))) - 1) * 100, 2),
        .groups = "drop"
      ) %>%
      arrange(CAGR) %>%
      slice_head(n = 3)
    forecast_data <- crop_forecast_data()
    forecast_summary <- if (!is.null(forecast_data)) {
      forecast_vals <- forecast_data$forecast %>%
        filter(Year %in% 2024:2026) %>%
        mutate(Yield = round(Yield, 0))
      mape <- round(forecast_data$mape, 2)
      paste("Forecasted yields: 2024 -", forecast_vals$Yield[1], "kg/ha, 2025 -", forecast_vals$Yield[2], "kg/ha, 2026 -", forecast_vals$Yield[3], "kg/ha. Model MAPE:", mape, "%")
    } else {
      "Insufficient data for forecasting"
    }
    risk_crops <- crops_in_danger() %>%
      mutate(`Average Yield (kg/ha)` = round(Avg_Yield, 2), `CAGR (%)` = round(CAGR, 2)) %>%
      rename(Country = Area, Crop = Item, `Risk Level` = Risk_Level) %>%
      slice_head(n = 3)
    
    paste0(
      "# Crop Yield Performance Report\n\n",
      "## Selection Summary\n",
      "- **Crops**: ", paste(input$selected_item, collapse = ", "), "\n",
      "- **Countries**: ", paste(input$selected_countries, collapse = ", "), "\n",
      "- **Year Range**: ", input$year_range[1], " to ", input$year_range[2], "\n\n",
      "## Key Performance Indicators\n",
      "- **Average Yield**: ", avg_yield, " kg/ha\n",
      "- **Africa-Wide Average Yield**: ", africa_avg, " kg/ha\n",
      "- **Trend**: ", trend_text, "\n",
      "- **CAGR**: ", cagr, "%\n\n",
      "## Trend Analysis\n",
      "The yield trend plot shows the average yield over time for the selected crops and countries. The trend is ", trend_text, ", indicating ", 
      ifelse(trend_text == "increasing", "improving productivity", ifelse(trend_text == "decreasing", "declining productivity", "stable performance")), ".\n\n",
      "## Top Performing Crops\n",
      ifelse(nrow(top_crops) > 0, 
             paste("The top performing crops are:\n", 
                   paste("- ", top_crops$Item, ": ", top_crops$Avg_Yield, " kg/ha", collapse = "\n"), "\n"),
             "No crop data available for the selected filters.\n"), "\n",
      "## Top Performing Countries\n",
      ifelse(nrow(top_countries) > 0, 
             paste("The top performing countries are:\n", 
                   paste("- ", top_countries$Area, ": ", top_countries$Avg_Yield, " kg/ha", collapse = "\n"), "\n"),
             "No country data available for the selected filters.\n"), "\n",
      "## Bottom Performing Countries\n",
      ifelse(nrow(bottom_countries) > 0, 
             paste("The bottom performing countries are:\n", 
                   paste("- ", bottom_countries$Area, ": ", bottom_countries$Avg_Yield, " kg/ha", collapse = "\n"), "\n"),
             "No country data available for the selected filters.\n"), "\n",
      "## Yield Distribution\n",
      "The boxplot shows the yield distribution across selected countries, highlighting variability and potential outliers. The outliers table lists:\n",
      ifelse(nrow(outliers) > 0, 
             paste(paste("- ", outliers$Country, " (", outliers$Crop, ", ", outliers$Year, "): ", outliers$Value, " kg/ha", collapse = "\n"), "\n"),
             "No outliers detected.\n"), "\n",
      "## Performance Comparison\n",
      ifelse(nrow(comparison) > 0, 
             paste("Key performance metrics include:\n", 
                   paste("- ", comparison$Area, " (", comparison$Item, "): Avg Yield = ", comparison$Avg_Yield, " kg/ha, CAGR = ", comparison$CAGR, "%", collapse = "\n"), "\n"),
             "No comparison data available.\n"), "\n",
      "## Forecast\n",
      forecast_summary, "\n\n",
      "## Crops at Risk\n",
      ifelse(nrow(risk_crops) > 0, 
             paste("Crops with high or moderate risk include:\n", 
                   paste("- ", risk_crops$Country, " (", risk_crops$Crop, "): ", risk_crops$`Average Yield (kg/ha)`, " kg/ha, CAGR = ", risk_crops$`CAGR (%)`, "%, Risk = ", risk_crops$`Risk Level`, collapse = "\n"), "\n"),
             "No crops identified as at risk.\n")
    )
  })
  
  # Report: Preview
  output$report_preview <- renderUI({
    HTML(markdown::markdownToHTML(text = report_content(), fragment.only = TRUE))
  })
  
  # Report: Download
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Crop_Yield_Report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(report_content(), temp_file)
      rmarkdown::render(temp_file, output_file = file, output_format = "html_document")
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
