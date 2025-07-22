# Maina001
--------------------------------------
> # 1. Load Libraries
> # ---------------------------------------
> library(shiny)
> library(shinyWidgets)
> library(shinyjs)
> library(dplyr)
> library(dtplyr)
> library(data.table)
> library(ggplot2)
> library(plotly)
> library(reactable)
> library(DT)
> library(forecast)
> library(scales)
> library(thematic)
> library(fontawesome)
> library(shinycssloaders)
> library(glue)
> # Set modern theme
> thematic_shiny(font = "auto")
> # ---------------------------------------
> # 2. Preprocess Data
> # ---------------------------------------
> # Load data from CSV files
> tryCatch({
+   africa_crop_yield_cleaned <- fread("africa_crop_yield_cleaned.csv")
+   africa_average_yield <- fread("africa_average_yield.csv")
+ }, error = function(e) {
+   message("Error loading CSV files: ", e$message, "\nFalling back to sample data.")
+   africa_crop_yield_cleaned <- data.table(
+     Area = rep(c("Algeria", "Angola", "South Africa", "Kenya"), each = 20),
+     Item = rep(c("Maize, grain", "Wheat", "Rice, paddy", "Sorghum"), each = 5),
+     Year = rep(2000:2019, 4),
+     Value = runif(80, 1000, 5000),
+     Flag Description = "Official figure"
Error: unexpected symbol in:
"    Value = runif(80, 1000, 5000),
    Flag Description"
> # Convert to data.table and standardize
> dt_crop_yield <- as.data.table(africa_crop_yield_cleaned)
> dt_avg_yield <- as.data.table(africa_average_yield)
> # Convert factors to characters
> dt_crop_yield[, `:=`(
+   Area = as.character(Area),
+   Item = tolower(as.character(Item))
+ )]
> dt_avg_yield[, `:=`(
+   Area = as.character(Area),
+   Item = tolower(as.character(Item))
+ )]
> # Map maize synonyms to "maize"
> maize_synonyms <- c("maize", "corn", "maize, grain")
> dt_crop_yield[, Item := ifelse(Item %in% maize_synonyms, "maize", Item)]
> dt_avg_yield[, Item := ifelse(Item %in% maize_synonyms, "maize", Item)]
> # Add Source column if missing
> if (!"Source" %in% names(dt_crop_yield)) {
+   message("Source column missing in dt_crop_yield. Adding default 'Country'.")
+   dt_crop_yield[, Source := "Country"]
+ }
Source column missing in dt_crop_yield. Adding default 'Country'.
> if (!"Source" %in% names(dt_avg_yield)) {
+   message("Source column missing in dt_avg_yield. Adding default 'Continent'.")
+   dt_avg_yield[, Source := "Continent"]
+ }
Source column missing in dt_avg_yield. Adding default 'Continent'.
> # Log available crops for debugging
> message("Available crops in dt_crop_yield: ", paste(unique(dt_crop_yield$Item), collapse = ", "))
Available crops in dt_crop_yield: almonds, in shell, apples, apricots, artichokes, bananas, barley, beans, dry, broad beans and horse beans, dry, broad beans and horse beans, green, cabbages, carrots and turnips, cauliflowers and broccoli, cherries, chick peas, dry, chillies and peppers, dry (capsicum spp., pimenta spp.), raw, chillies and peppers, green (capsicum spp. and pimenta spp.), cucumbers and gherkins, dates, eggplants (aubergines), figs, grapes, green garlic, groundnuts, excluding shelled, lemons and limes, lentils, dry, locust beans (carobs), maize (corn), oats, olives, onions and shallots, dry (excluding dehydrated), onions and shallots, green, oranges, other beans, green, other citrus fruit, n.e.c., other fruits, n.e.c., other pulses n.e.c., other stone fruits, other vegetables, fresh n.e.c., peaches and nectarines, pears, peas, dry, peas, green, plums and sloes, pomelos and grapefruits, potatoes, pumpkins, squash and gourds, quinces, rape or colza seed, rice, seed cotton, unginned, sorghum, sunflower seed, tangerines, mandarins, clementines, tomatoes, triticale, unmanufactured tobacco, vetches, watermelons, wheat, avocados, cashew nuts, in shell, cassava, fresh, castor oil seeds, cocoa beans, coffee, green, kenaf, and other textile bast fibres, raw or retted, mangoes, guavas and mangosteens, millet, oil palm fruit, pineapples, sesame seed, sisal, raw, soya beans, sugar cane, sweet potatoes, coconuts, in shell, fonio, karite nuts (sheanuts), kola nuts, okra, other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c., other oil seeds, n.e.c., pepper (piper spp.), raw, taro, yams, cereals n.e.c., edible roots and tubers with high starch or inulin content, n.e.c., fresh, spinach, bambara beans, dry, cow peas, dry, other stimulant, spice and aromatic crops, n.e.c., other tropical fruits, n.e.c., walnuts, in shell, pigeon peas, dry, tea leaves, lettuce and chicory, cantaloupes and other melons, chestnuts, in shell, chicory roots, ginger, raw, hazelnuts, in shell, jute, raw or retted, leeks and other alliaceous vegetables, melonseed, natural rubber in primary forms, papayas, plantains and cooking bananas, cloves (whole stems), raw, vanilla, raw, green corn (maize), string beans, anise, badian, coriander, cumin, caraway, fennel and juniper berries, raw, flax, raw or retted, linseed, lupins, other berries and fruits of the genus vaccinium n.e.c., rye, strawberries, sugar beet, abaca, manila hemp, raw, hop cones, mustard seed, nutmeg, mace, cardamoms, raw, other fibre crops, raw, n.e.c., safflower seed, asparagus, pyrethrum, dried flowers, cashewapple, cinnamon and cinnamon-tree flowers, raw, pistachios, in shell, tung nuts, blueberries, canary seed, peppermint, spearmint, raspberries, buckwheat, cranberries, kiwi fruit
> message("Available crops in dt_avg_yield: ", paste(unique(dt_avg_yield$Item), collapse = ", "))
Available crops in dt_avg_yield: abaca, manila hemp, raw, almonds, in shell, anise, badian, coriander, cumin, caraway, fennel and juniper berries, raw, apples, apricots, artichokes, asparagus, avocados, bambara beans, dry, bananas, barley, beans, dry, blueberries, broad beans and horse beans, dry, broad beans and horse beans, green, buckwheat, cabbages, canary seed, cantaloupes and other melons, carrots and turnips, cashew nuts, in shell, cashewapple, cassava, fresh, castor oil seeds, cauliflowers and broccoli, cereals n.e.c., cherries, chestnuts, in shell, chick peas, dry, chicory roots, chillies and peppers, dry (capsicum spp., pimenta spp.), raw, chillies and peppers, green (capsicum spp. and pimenta spp.), cinnamon and cinnamon-tree flowers, raw, cloves (whole stems), raw, cocoa beans, coconuts, in shell, coffee, green, cow peas, dry, cranberries, cucumbers and gherkins, dates, edible roots and tubers with high starch or inulin content, n.e.c., fresh, eggplants (aubergines), figs, flax, raw or retted, fonio, ginger, raw, grapes, green corn (maize), green garlic, groundnuts, excluding shelled, hazelnuts, in shell, hop cones, jute, raw or retted, karite nuts (sheanuts), kenaf, and other textile bast fibres, raw or retted, kiwi fruit, kola nuts, leeks and other alliaceous vegetables, lemons and limes, lentils, dry, lettuce and chicory, linseed, locust beans (carobs), lupins, maize (corn), mangoes, guavas and mangosteens, melonseed, millet, mustard seed, natural rubber in primary forms, nutmeg, mace, cardamoms, raw, oats, oil palm fruit, okra, olives, onions and shallots, dry (excluding dehydrated), onions and shallots, green, oranges, other beans, green, other berries and fruits of the genus vaccinium n.e.c., other citrus fruit, n.e.c., other fibre crops, raw, n.e.c., other fruits, n.e.c., other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c., other oil seeds, n.e.c., other pulses n.e.c., other stimulant, spice and aromatic crops, n.e.c., other stone fruits, other tropical fruits, n.e.c., other vegetables, fresh n.e.c., papayas, peaches and nectarines, pears, peas, dry, peas, green, pepper (piper spp.), raw, peppermint, spearmint, pigeon peas, dry, pineapples, pistachios, in shell, plantains and cooking bananas, plums and sloes, pomelos and grapefruits, potatoes, pumpkins, squash and gourds, pyrethrum, dried flowers, quinces, rape or colza seed, raspberries, rice, rye, safflower seed, seed cotton, unginned, sesame seed, sisal, raw, sorghum, soya beans, spinach, strawberries, string beans, sugar beet, sugar cane, sunflower seed, sweet potatoes, tangerines, mandarins, clementines, taro, tea leaves, tomatoes, triticale, tung nuts, unmanufactured tobacco, vanilla, raw, vetches, walnuts, in shell, watermelons, wheat, yams
> # Precompute KPIs
> kpi_data <- dt_crop_yield[, .(
+   mean_yield = mean(Value, na.rm = TRUE),
+   sd_yield = sd(Value, na.rm = TRUE),
+   cv_yield = ifelse(mean(Value, na.rm = TRUE) == 0, 0, sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE)),
+   growth_rate = ifelse(
+     first(Value) == 0, 0,
+     (last(Value) - first(Value)) / first(Value) * 100 / (max(Year) - min(Year))
+   )
+ ), by = .(Area, Item)]
> # Custom color palette
> modern_palette <- c(
+   "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
+   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
+ )
> # ---------------------------------------
> # 3. UI
> # ---------------------------------------
> ui <- fluidPage(
+   useShinyjs(),
+   tags$head(
+     tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&display=swap"),
+     tags$style(HTML("
+       body { font-family: 'Inter', sans-serif; }
+       .header { background-color: #1a2526; color: white; padding: 10px; font-size: 18px; font-weight: 600; }
+       .filters { background: rgba(255,255,255,0.05); border-radius: 8px; padding: 15px; margin: 10px; }
+       .box { border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); border-top: 3px solid #1f77b4; padding: 15px; margin-bottom: 20px; }
+       .btn-primary { background-color: #1f77b4; border-color: #1f77b4; }
+       .btn-primary:hover { background-color: #155a8a; border-color: #155a8a; }
+       .btn-danger { background-color: #d62728; border-color: #d62728; }
+       .btn-danger:hover { background-color: #a71e1f; border-color: #a71e1f; }
+     ")),
+     tags$script(HTML("
+       function closeApp() {
+         setTimeout(function() { window.close(); }, 500);
+       }
+     "))
+   ),
+   div(class = "header", 
+       icon("seedling", class = "fa-rotate-180"), 
+       "African Crop Yield Intelligence"
+   ),
+   fluidRow(
+     column(
+       width = 3,
+       div(
+         class = "filters",
+         awesomeRadio(
+           inputId = "analysis_level",
+           label = "Analysis Level",
+           choices = c("Country", "Regional"),
+           selected = "Country",
+           inline = TRUE,
+           status = "primary"
+         ),
+         pickerInput(
+           inputId = "countries",
+           label = "Select Countries",
+           choices = unique(dt_crop_yield$Area),
+           multiple = TRUE,
+           selected = unique(dt_crop_yield$Area)[1:2],
+           options = list(
+             `actions-box` = TRUE,
+             `live-search` = TRUE,
+             `selected-text-format` = "count > 3",
+             `count-selected-text` = "{0} countries selected"
+           )
+         ),
+         pickerInput(
+           inputId = "crops",
+           label = span("Select Crops", actionLink("crop_info", icon("info-circle"))),
+           choices = unique(dt_crop_yield$Item),
+           multiple = TRUE,
+           selected = unique(dt_crop_yield$Item)[1:2],
+           options = list(
+             `actions-box` = TRUE,
+             `live-search` = TRUE,
+             `selected-text-format` = "count > 3",
+             `count-selected-text` = "{0} crops selected"
+           )
+         ),
+         pickerInput(
+           inputId = "arima_crop",
+           label = "Select Crop for ARIMA Forecast",
+           choices = unique(dt_crop_yield$Item),
+           selected = "maize",
+           options = list(`live-search` = TRUE)
+         ),
+         sliderTextInput(
+           inputId = "year_range",
+           label = "Year Range",
+           choices = unique(dt_crop_yield$Year),
+           selected = range(dt_crop_yield$Year),
+           grid = TRUE
+         ),
+         switchInput(
+           inputId = "show_africa_avg",
+           label = "Show Continental Average",
+           value = TRUE,
+           onStatus = "success"
+         ),
+         actionBttn(
+           inputId = "apply_filters",
+           label = "Apply Filters",
+           style = "gradient",
+           color = "primary",
+           block = TRUE,
+           icon = icon("filter")
+         ),
+         actionBttn(
+           inputId = "stop_app",
+           label = "Stop Application",
+           style = "gradient",
+           color = "danger",
+           block = TRUE,
+           icon = icon("power-off")
+         ),
+         h5("Available Crops:"),
+         verbatimTextOutput("available_crops")
+       )
+     ),
+     column(
+       width = 9,
+       tabsetPanel(
+         id = "tabs",
+         tabPanel(
+           "Main Dashboard",
+           fluidRow(
+             column(3, valueBoxOutput("highest_yield_year") %>% withSpinner(type = 6, color = "#1f77b4")),
+             column(3, valueBoxOutput("avg_yield") %>% withSpinner(type = 6, color = "#1f77b4")),
+             column(3, valueBoxOutput("yield_volatility") %>% withSpinner(type = 6, color = "#1f77b4")),
+             column(3, valueBoxOutput("recent_yield") %>% withSpinner(type = 6, color = "#1f77b4"))
+           ),
+           div(class = "box",
+               h4(icon("chart-line"), "Yield Trends with Forecasts"),
+               tabsetPanel(
+                 tabPanel("Interactive Chart", plotlyOutput("trend_plot", height = "500px") %>% withSpinner(type = 6, color = "#1f77b4")),
+                 tabPanel("Data Table", DTOutput("trend_data_table") %>% withSpinner(type = 6, color = "#1f77b4"))
+               ),
+               downloadButton("download_trend_plot", "Download Chart", class = "btn-primary"),
+               downloadButton("download_trend_data", "Download Data", class = "btn-success")
+           ),
+           fluidRow(
+             column(6, 
+                    div(class = "box",
+                        h4(icon("table"), "Summary Statistics"),
+                        reactableOutput("summary_table") %>% withSpinner(type = 6, color = "#1f77b4")
+                    )
+             ),
+             column(6, 
+                    div(class = "box",
+                        h4(icon("map"), "Geospatial Distribution"),
+                        plotlyOutput("yield_map", height = "400px") %>% withSpinner(type = 6, color = "#1f77b4"),
+                        p("Yield values averaged across selected years and crops")
+                    )
+             )
+           )
+         ),
+         tabPanel(
+           "Comparative Analysis",
+           div(class = "box",
+               h4(icon("ranking-star"), "Comparative Analysis"),
+               tabsetPanel(
+                 tabPanel(
+                   "Performance Rankings",
+                   fluidRow(
+                     column(6, h4("Top Performing Countries", style = "text-align: center;"), 
+                            reactableOutput("top_yield_table") %>% withSpinner(type = 6, color = "#1f77b4")),
+                     column(6, h4("Countries Needing Improvement", style = "text-align: center;"), 
+                            reactableOutput("low_yield_table") %>% withSpinner(type = 6, color = "#1f77b4"))
+                   )
+                 ),
+                 tabPanel(
+                   "Benchmarking",
+                   plotlyOutput("comparison_plot", height = "500px") %>% withSpinner(type = 6, color = "#1f77b4"),
+                   radioGroupButtons(
+                     inputId = "benchmark_type",
+                     label = "Comparison Type",
+                     choices = c("Against Continental Average", "Against Top Performer"),
+                     selected = "Against Continental Average",
+                     status = "primary",
+                     justified = TRUE
+                   )
+                 )
+               )
+           )
+         ),
+         tabPanel(
+           "Risk Assessment",
+           div(class = "box",
+               h4(icon("shield-halved"), "Food Security Risk Assessment"),
+               tabsetPanel(
+                 tabPanel(
+                   "Risk Scores",
+                   reactableOutput("risk_table") %>% withSpinner(type = 6, color = "#1f77b4"),
+                   p("Risk is calculated based on yield volatility and trends.")
+                 ),
+                 tabPanel(
+                   "Risk Drivers",
+                   plotlyOutput("risk_drivers_plot", height = "500px") %>% withSpinner(type = 6, color = "#1f77b4")
+                 )
+               )
+           ),
+           div(class = "box",
+               h4(icon("lightbulb"), "Recommendations"),
+               uiOutput("recommendations")
+           )
+         ),
+         tabPanel(
+           "Predictive Trends",
+           div(class = "box",
+               h4(icon("chart-line"), uiOutput("arima_title")),
+               verbatimTextOutput("arima_summary"),
+               reactableOutput("arima_table") %>% withSpinner(type = 6, color = "#1f77b4"),
+               plotlyOutput("arima_plot", height = "400px") %>% withSpinner(type = 6, color = "#1f77b4")
+           )
+         ),
+         tabPanel(
+           "SDG Insights",
+           div(class = "box",
+               h4(icon("seedling"), "SDG 2: Zero Hunger"),
+               reactableOutput("sdg2_table") %>% withSpinner(type = 6, color = "#1f77b4")
+           )
+         ),
+         tabPanel(
+           "Data Explorer",
+           div(class = "box",
+               h4(icon("database"), "Data Explorer"),
+               DTOutput("full_data_table") %>% withSpinner(type = 6, color = "#1f77b4"),
+               downloadButton("download_full_data", "Download Full Dataset", class = "btn-primary")
+           )
+         )
+       )
+     )
+   )
+ )
> # ---------------------------------------
> # 4. Server
> # ---------------------------------------
> server <- function(input, output, session) {
+   # Stop app button
+   observeEvent(input$stop_app, {
+     shinyjs::runjs("closeApp()")
+     stopApp()
+   })
+   
+   # Display available crops
+   output$available_crops <- renderText({
+     paste("Crops:", paste(sort(unique(dt_crop_yield$Item)), collapse = ", "))
+   })
+   
+   # Reactive filtered data
+   filtered_data <- eventReactive(input$apply_filters, {
+     req(input$countries, input$crops)
+     data <- dt_crop_yield[
+       Area %in% input$countries & Item %in% input$crops &
+         Year >= input$year_range[1] & Year <= input$year_range[2]
+     ]
+     if (input$show_africa_avg) {
+       avg_data <- dt_avg_yield[
+         Item %in% input$crops & Year >= input$year_range[1] & Year <= input$year_range[2]
+       ]
+       data <- rbind(
+         data[, .(Area, Item, Year, Value, Source)],
+         avg_data[, .(Area = "Africa Average", Item, Year, Value, Source)], fill = TRUE
+       )
+     } else {
+       data[, Source := "Country"]
+     }
+     data
+   })
+   
+   # ARIMA forecast data (reactive to input$arima_crop)
+   arima_data <- reactive({
+     req(input$arima_crop)
+     crop_ts <- dt_crop_yield[Item == input$arima_crop][
+       , .(mean_yield = mean(Value / 10000, na.rm = TRUE)), by = Year
+     ][
+       order(Year)
+     ]
+     arima_model <- NULL
+     forecast_result <- NULL
+     if (nrow(crop_ts) >= 4) {
+       tryCatch({
+         arima_model <- auto.arima(crop_ts$mean_yield, max.p = 2, max.q = 2, seasonal = FALSE)
+         forecast_result <- forecast(arima_model, h = 3)
+       }, error = function(e) {
+         message("Error in ARIMA model for ", input$arima_crop, ": ", e$message)
+       })
+     } else {
+       message("Insufficient data for ", input$arima_crop, " ARIMA model (", nrow(crop_ts), " observations). Need at least 4.")
+     }
+     list(crop_ts = crop_ts, arima_model = arima_model, forecast_result = forecast_result)
+   })
+   
+   # Forecast data for trends
+   forecast_data <- reactive({
+     req(filtered_data())
+     data <- filtered_data()[Source == "Country"]
+     if (nrow(data) == 0) return(NULL)
+     top_combinations <- data[, .(Value = mean(Value, na.rm = TRUE)), by = .(Area, Item)][order(-Value)][1:min(5, .N)]
+     forecast_list <- lapply(1:nrow(top_combinations), function(i) {
+       combo <- top_combinations[i]
+       subset_data <- data[Area == combo$Area & Item == combo$Item]
+       if (nrow(subset_data) > 3) {
+         tryCatch({
+           ts_data <- ts(subset_data$Value / 10000, start = min(subset_data$Year), frequency = 1)
+           model <- auto.arima(ts_data, max.p = 2, max.q = 2, seasonal = FALSE)
+           fc <- forecast(model, h = 3)
+           data.table(
+             Area = combo$Area,
+             Item = combo$Item,
+             Year = max(subset_data$Year) + 1:3,
+             Value = as.numeric(fc$mean) * 10000,
+             Lower = as.numeric(fc$lower[, 2]) * 10000,
+             Upper = as.numeric(fc$upper[, 2]) * 10000,
+             Source = "Forecast"
+           )
+         }, error = function(e) { NULL })
+       } else {
+         NULL
+       }
+     }) %>% rbindlist()
+     rbind(data[, .(Area, Item, Year, Value, Source)], forecast_list, fill = TRUE)
+   })
+   
+   # KPIs
+   output$highest_yield_year <- renderValueBox({
+     data <- filtered_data()
+     if (nrow(data) == 0) return(NULL)
+     highest <- data[Source == "Country"][which.max(Value), .(Year, Value, Item)]
+     valueBox(
+       value = format(round(highest$Value / 10000, 2), big.mark = ","),
+       subtitle = glue("Peak Yield: {highest$Item} ({highest$Year})"),
+       icon = icon("trophy"),
+       color = "warning"
+     )
+   })
+   
+   output$avg_yield <- renderValueBox({
+     data <- filtered_data()
+     if (nrow(data) == 0) return(NULL)
+     avg <- mean(data[Source == "Country"]$Value, na.rm = TRUE) / 10000
+     trend <- data[Source == "Country"][, .(Mean = mean(Value, na.rm = TRUE)), by = Year][, .(Slope = lm(Mean ~ Year)$coef[2])]$Slope
+     valueBox(
+       value = format(round(avg, 2), big.mark = ","),
+       subtitle = glue("Avg Yield | Trend: {ifelse(trend > 0, '↑', '↓')}"),
+       icon = icon("chart-line"),
+       color = ifelse(trend > 0, "success", "danger")
+     )
+   })
+   
+   output$yield_volatility <- renderValueBox({
+     data <- filtered_data()
+     if (nrow(data) == 0) return(NULL)
+     cv <- sd(data[Source == "Country"]$Value, na.rm = TRUE) / mean(data[Source == "Country"]$Value, na.rm = TRUE) * 100
+     valueBox(
+       value = paste(round(cv, 1), "%"),
+       subtitle = "Yield Volatility (CV)",
+       icon = icon("wave-square"),
+       color = ifelse(cv > 30, "orange", "primary")
+     )
+   })
+   
+   output$recent_yield <- renderValueBox({
+     data <- filtered_data()
+     if (nrow(data) == 0) return(NULL)
+     recent <- data[Source == "Country" & Year == max(Year), .(Value = mean(Value, na.rm = TRUE) / 10000)]
+     valueBox(
+       value = format(round(recent$Value, 2), big.mark = ","),
+       subtitle = glue("Most Recent ({max(data[Source == 'Country']$Year)})"),
+       icon = icon("calendar-check"),
+       color = "purple"
+     )
+   })
+   
+   # Trend Plot
+   output$trend_plot <- renderPlotly({
+     data <- forecast_data()
+     if (is.null(data)) return(NULL)
+     top_crops <- data[, .(Mean = mean(Value, na.rm = TRUE)), by = Item][order(-Mean)][1:min(4, .N)]$Item
+     data <- data[Item %in% top_crops]
+     p <- ggplot(data, aes(x = Year, y = Value / 10000, color = Area, linetype = Source, group = interaction(Area, Source))) +
+       geom_line(size = 1.2, alpha = 0.8) +
+       geom_point(data = data[Source == "Country" & Year == max(Year)], size = 3) +
+       geom_ribbon(data = data[Source == "Forecast"], aes(ymin = Lower / 10000, ymax = Upper / 10000, fill = Area), alpha = 0.2, color = NA) +
+       scale_color_manual(values = modern_palette) +
+       scale_fill_manual(values = modern_palette) +
+       facet_wrap(~ Item, scales = "free_y", ncol = 2) +
+       labs(title = "Yield Trends with 3-Year Forecasts", x = "Year", y = "Yield (tons/ha)") +
+       theme_minimal(base_size = 14, base_family = "Inter")
+     ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 14)), legend = list(orientation = "h", y = 1.1))
+   })
+   
+   # Summary Table
+   output$summary_table <- renderReactable({
+     data <- filtered_data()[Source == "Country"]
+     if (nrow(data) == 0) return(NULL)
+     summary_data <- data[
+       , .(
+         Mean = mean(Value / 10000, na.rm = TRUE),
+         Median = median(Value / 10000, na.rm = TRUE),
+         Volatility = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE) * 100,
+         Min = min(Value / 10000, na.rm = TRUE),
+         Max = max(Value / 10000, na.rm = TRUE)
+       ), by = .(Area, Item)
+     ][, lapply(.SD, round, 2), .SDcols = c("Mean", "Median", "Volatility", "Min", "Max")]
+     reactable(
+       summary_data,
+       columns = list(
+         Volatility = colDef(name = "Volatility (%)", format = colFormat(suffix = "%"), style = function(value) {
+           list(color = ifelse(value > 40, "#ff7f0e", "#2ca02c"), fontWeight = "bold")
+         }),
+         Mean = colDef(name = "Avg Yield (tons/ha)", format = colFormat(suffix = " tons/ha")),
+         Median = colDef(name = "Median Yield (tons/ha)", format = colFormat(suffix = " tons/ha")),
+         Min = colDef(name = "Min Yield (tons/ha)", format = colFormat(suffix = " tons/ha")),
+         Max = colDef(name = "Max Yield (tons/ha)", format = colFormat(suffix = " tons/ha"))
+       ),
+       defaultPageSize = 10,
+       searchable = TRUE,
+       highlight = TRUE
+     )
+   })
+   
+   # Top Yield Table
+   output$top_yield_table <- renderReactable({
+     data <- filtered_data()[Source == "Country" & Year == max(Year)]
+     if (nrow(data) == 0) return(NULL)
+     top_data <- data[, .(Yield = mean(Value / 10000, na.rm = TRUE)), by = Area][order(-Yield)][1:min(10, .N)]
+     top_data[, Rank := 1:.N]
+     top_data[, Yield := round(Yield, 2)]
+     reactable(
+       top_data,
+       columns = list(
+         Rank = colDef(name = "#", width = 60, cell = function(value) {
+           badge <- if (value == 1) "gold" else if (value == 2) "silver" else if (value == 3) "bronze" else "default"
+           div(class = paste0("badge ", badge), value)
+         }),
+         Yield = colDef(name = "Yield (tons/ha)", format = colFormat(suffix = " tons/ha"), style = list(fontWeight = "bold"))
+       ),
+       defaultPageSize = 5
+     )
+   })
+   
+   # Low Yield Table
+   output$low_yield_table <- renderReactable({
+     data <- filtered_data()[Source == "Country" & Year == max(Year)]
+     if (nrow(data) == 0) return(NULL)
+     low_data <- data[, .(Yield = mean(Value / 10000, na.rm = TRUE)), by = Area][order(Yield)][1:min(10, .N)]
+     low_data[, Rank := 1:.N]
+     low_data[, Yield := round(Yield, 2)]
+     reactable(
+       low_data,
+       columns = list(
+         Rank = colDef(name = "#", width = 60),
+         Yield = colDef(name = "Yield (tons/ha)", format = colFormat(suffix = " tons/ha"), style = list(fontWeight = "bold"))
+       ),
+       defaultPageSize = 5
+     )
+   })
+   
+   # Trend Data Table
+   output$trend_data_table <- renderDT({
+     data <- forecast_data()
+     if (is.null(data)) return(NULL)
+     datatable(
+       data[, .(Area, Item, Year, Value = round(Value / 10000, 2), Source)],
+       options = list(pageLength = 10, searching = TRUE, autoWidth = TRUE),
+       rownames = FALSE,
+       style = "bootstrap4",
+       class = "table-bordered table-striped"
+     )
+   })
+   
+   # Full Data Table
+   output$full_data_table <- renderDT({
+     data <- filtered_data()
+     if (is.null(data)) return(NULL)
+     datatable(
+       data[, .(Area, Item, Year, Value = round(Value / 10000, 2), Source)],
+       options = list(pageLength = 10, searching = TRUE, autoWidth = TRUE),
+       rownames = FALSE,
+       style = "bootstrap4",
+       class = "table-bordered table-striped"
+     )
+   })
+   
+   # Yield Map (Placeholder)
+   output$yield_map <- renderPlotly({
+     data <- filtered_data()[Source == "Country"]
+     if (nrow(data) == 0) return(NULL)
+     avg_data <- data[, .(Value = mean(Value / 10000, na.rm = TRUE)), by = .(Area)]
+     p <- ggplot(avg_data, aes(x = Area, y = Value, fill = Area)) +
+       geom_bar(stat = "identity") +
+       scale_fill_manual(values = modern_palette) +
+       labs(title = "Average Yield by Country", x = "Country", y = "Yield (tons/ha)") +
+       theme_minimal(base_size = 14, base_family = "Inter") +
+       theme(axis.text.x = element_text(angle = 45, hjust = 1))
+     ggplotly(p)
+   })
+   
+   # Comparison Plot
+   output$comparison_plot <- renderPlotly({
+     data <- filtered_data()
+     if (nrow(data) == 0) return(NULL)
+     if (input$benchmark_type == "Against Continental Average") {
+       p <- ggplot(data, aes(x = Year, y = Value / 10000, color = Area, group = Area)) +
+         geom_line(size = 1.2) +
+         scale_color_manual(values = modern_palette) +
+         labs(title = "Yield Comparison to Continental Average", x = "Year", y = "Yield (tons/ha)") +
+         theme_minimal(base_size = 14, base_family = "Inter")
+     } else {
+       top_performer <- data[Source == "Country"][, .(Value = mean(Value, na.rm = TRUE)), by = Area][which.max(Value)]$Area
+       p <- ggplot(data[Area %in% c(top_performer, "Africa Average")], aes(x = Year, y = Value / 10000, color = Area, group = Area)) +
+         geom_line(size = 1.2) +
+         scale_color_manual(values = modern_palette) +
+         labs(title = paste("Yield Comparison to Top Performer:", top_performer), x = "Year", y = "Yield (tons/ha)") +
+         theme_minimal(base_size = 14, base_family = "Inter")
+     }
+     ggplotly(p)
+   })
+   
+   # Risk Drivers Plot
+   output$risk_drivers_plot <- renderPlotly({
+     data <- filtered_data()[Source == "Country"]
+     if (nrow(data) == 0) return(NULL)
+     risk_data <- data[
+       , .(
+         Volatility = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE) * 100,
+         Trend_Slope = if (.N > 3) lm(Value ~ Year)$coefficients[2] else NA_real_
+       ), by = .(Area, Item)
+     ]
+     p <- ggplot(risk_data, aes(x = Volatility, y = Trend_Slope, color = Area, text = paste(Area, Item, sep = ": "))) +
+       geom_point(size = 3) +
+       scale_color_manual(values = modern_palette) +
+       labs(title = "Risk Drivers: Volatility vs Trend", x = "Volatility (%)", y = "Trend Slope (kg/ha/yr)") +
+       theme_minimal(base_size = 14, base_family = "Inter")
+     ggplotly(p, tooltip = "text")
+   })
+   
+   # Risk Table
+   output$risk_table <- renderReactable({
+     data <- filtered_data()[Source == "Country"]
+     if (nrow(data) == 0) return(NULL)
+     risk_data <- data[
+       , .(
+         Volatility = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE) * 100,
+         Trend_Slope = if (.N > 3) lm(Value ~ Year)$coefficients[2] else NA_real_
+       ), by = .(Area, Item)
+     ][
+       , Risk_Score := ifelse(is.na(Volatility) | is.na(Trend_Slope), NA,
+                              round(0.6 * Volatility - 0.4 * Trend_Slope / 1000, 2))
+     ]
+     reactable(
+       risk_data,
+       columns = list(
+         Volatility = colDef(name = "Volatility (%)", format = colFormat(suffix = "%")),
+         Trend_Slope = colDef(name = "Trend Slope (kg/ha/yr)", format = colFormat(digits = 2)),
+         Risk_Score = colDef(name = "Risk Score", format = colFormat(digits = 2), style = function(value) {
+           list(color = ifelse(value > 30, "#ff7f0e", "#2ca02c"), fontWeight = "bold")
+         })
+       ),
+       defaultPageSize = 10,
+       searchable = TRUE,
+       highlight = TRUE
+     )
+   })
+   
+   # Recommendations
+   output$recommendations <- renderUI({
+     data <- filtered_data()[Source == "Country"]
+     if (nrow(data) == 0) return(NULL)
+     risk_data <- data[
+       , .(
+         Volatility = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE) * 100,
+         Trend_Slope = if (.N > 3) lm(Value ~ Year)$coefficients[2] else NA_real_
+       ), by = .(Area, Item)
+     ]
+     high_risk <- risk_data[Volatility > 30 | Trend_Slope < 0]$Area
+     if (length(high_risk) > 0) {
+       HTML(paste(
+         "<p>Recommendations for high-risk areas:</p>",
+         "<ul>",
+         paste0("<li>Improve agricultural practices in ", unique(high_risk), " to stabilize yields.</li>", collapse = ""),
+         "</ul>"
+       ))
+     } else {
+       HTML("<p>No high-risk areas identified.</p>")
+     }
+   })
+   
+   # ARIMA Forecast
+   output$arima_title <- renderUI({
+     req(input$arima_crop)
+     h4(icon("chart-line"), glue("{input$arima_crop} Yield Forecast (ARIMA)"))
+   })
+   
+   output$arima_summary <- renderText({
+     ad <- arima_data()
+     if (is.null(ad$forecast_result)) {
+       return(glue("ARIMA model unavailable for {input$arima_crop} due to insufficient data (<4 observations) or model convergence failure."))
+     }
+     paste(glue("Predicted {input$arima_crop} Yield for Next 3 Years (tons/ha):"), 
+           paste(round(as.numeric(ad$forecast_result$mean), 2), collapse = ", "))
+   })
+   
+   output$arima_table <- renderReactable({
+     ad <- arima_data()
+     if (is.null(ad$forecast_result)) return(NULL)
+     historical <- ad$crop_ts[, .(Year, mean_yield = round(mean_yield, 2))]
+     forecast_years <- max(ad$crop_ts$Year) + 1:3
+     forecast <- data.table(
+       Year = forecast_years,
+       mean_yield = round(as.numeric(ad$forecast_result$mean), 2)
+     )
+     reactable(
+       rbind(historical, forecast),
+       columns = list(mean_yield = colDef(name = "Yield (tons/ha)", format = colFormat(suffix = " tons/ha"))),
+       defaultPageSize = 10,
+       searchable = TRUE,
+       highlight = TRUE
+     )
+   })
+   
+   output$arima_plot <- renderPlotly({
+     ad <- arima_data()
+     if (is.null(ad$forecast_result)) return(NULL)
+     historical <- ad$crop_ts[, .(Year, mean_yield, Type = "Historical")]
+     forecast_years <- max(ad$crop_ts$Year) + 1:3
+     forecast <- data.table(
+       Year = forecast_years,
+       mean_yield = as.numeric(ad$forecast_result$mean),
+       Type = "Forecast"
+     )
+     data <- rbind(historical, forecast)
+     p <- ggplot(data, aes(x = Year, y = mean_yield, color = Type)) +
+       geom_line(size = 1) +
+       geom_point(data = data[Type == "Forecast"], size = 3) +
+       scale_color_manual(values = c("Historical" = "#1f77b4", "Forecast" = "#ff7f0e")) +
+       labs(title = glue("{input$arima_crop} Yield Forecast (ARIMA)"), x = "Year", y = "Yield (tons/ha)") +
+       theme_minimal(base_size = 14, base_family = "Inter")
+     ggplotly(p)
+   })
+   
+   # SDG Insights
+   output$sdg2_table <- renderReactable({
+     reactable(
+       rbind(
+         kpi_data[, .(Item, avg_yield = round(mean(mean_yield / 10000, na.rm = TRUE), 2)), by = Item][
+           order(-avg_yield)
+         ][
+           1:5, .(Item, avg_yield, Category = "Top Crops by Yield")
+         ],
+         kpi_data[, .(Area, avg_yield = round(mean(mean_yield / 10000, na.rm = TRUE), 2)), by = Area][
+           order(-avg_yield)
+         ][
+           1:5, .(Item = Area, avg_yield, Category = "Top Countries by Yield")
+         ]
+       ),
+       columns = list(avg_yield = colDef(name = "Yield (tons/ha)", format = colFormat(suffix = " tons/ha"))),
+       defaultPageSize = 10,
+       searchable = TRUE,
+       highlight = TRUE
+     )
+   })
+   
+   # Download Handlers
+   output$download_trend_plot <- downloadHandler(
+     filename = function() paste("yield-trend-", Sys.Date(), ".png", sep = ""),
+     content = function(file) export(plotly::last_plot(), file)
+   )
+   
+   output$download_trend_data <- downloadHandler(
+     filename = function() paste("yield-data-", Sys.Date(), ".csv", sep = ""),
+     content = function(file) write.csv(forecast_data(), file, row.names = FALSE)
+   )
+   
+   output$download_full_data <- downloadHandler(
+     filename = function() paste("full-data-", Sys.Date(), ".csv", sep = ""), # Fixed extension
+     content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
+   )
+ }
> # ---------------------------------------
> # 5. Run App
> # ---------------------------------------
> shinyApp(ui, server)

Listening on http://127.0.0.1:6221
Warning: Error in palette: Insufficient values in manual scale. 13 needed but only 10 provided.
  134: <Anonymous>
  109: layout
  107: renderPlotly [#153]
  106: func
  103: shinyRenderWidget
  102: func
   89: renderFunc
   88: output$trend_plot
    3: runApp
    2: print.shiny.appobj
    1: <Anonymous>
Warning: Error in palette: Insufficient values in manual scale. 13 needed but only 10 provided.
  132: <Anonymous>
  131: signalCondition
  130: signal_abort
  129: rlang::abort
  128: cli::cli_abort
  127: palette
  126: self$palette
  125: map
  124: self$map
  123: FUN
  122: lapply
  121: map_df
  120: scale$map_df
  119: FUN
  118: lapply
  116: map_df
  115: scales$map_df
  114: FUN
  113: lapply
  112: ggplot_build
  111: tns$ggthematic_build
  110: gg2list
  109: ggplotly.ggplot
  107: renderPlotly [#260]
  106: func
  103: shinyRenderWidget
  102: func
   89: renderFunc
   88: output$yield_map
    3: runApp
    2: print.shiny.appobj
    1: <Anonymous>
Warning: Error in palette: Insufficient values in manual scale. 45 needed but only 10 provided.
  134: <Anonymous>
  109: layout
  107: renderPlotly [#153]
  106: func
  103: shinyRenderWidget
  102: func
   89: renderFunc
   88: output$trend_plot
    3: runApp
    2: print.shiny.appobj
    1: <Anonymous>
Warning: Error in palette: Insufficient values in manual scale. 54 needed but only 10 provided.
  132: <Anonymous>
  131: signalCondition
  130: signal_abort
  129: rlang::abort
  128: cli::cli_abort
  127: palette
  126: self$palette
  125: map
  124: self$map
  123: FUN
  122: lapply
  121: map_df
  120: scale$map_df
  119: FUN
  118: lapply
  116: map_df
  115: scales$map_df
  114: FUN
  113: lapply
  112: ggplot_build
  111: tns$ggthematic_build
  110: gg2list
  109: ggplotly.ggplot
  107: renderPlotly [#260]
  106: func
  103: shinyRenderWidget
  102: func
   89: renderFunc
   88: output$yield_map
    3: runApp
    2: print.shiny.appobj
    1: <Anonymous>
Processed 2219 groups out of 2219. 100% done. Time elapsed: 3s. ETA: 0s.
Processed 2219 groups out of 2219. 100% done. Time elapsed: 3s. ETA: 0s.
Processed 2219 groups out of 2219. 100% done. Time elapsed: 3s. ETA: 0s.
Warning: Error in palette: Insufficient values in manual scale. 54 needed but only 10 provided.
  132: <Anonymous>
  131: signalCondition
  130: signal_abort
  129: rlang::abort
  128: cli::cli_abort
  127: palette
  126: self$palette
  125: map
  124: self$map
  123: FUN
  122: lapply
  121: map_df
  120: scale$map_df
  119: FUN
  118: lapply
  116: map_df
  115: scales$map_df
  114: FUN
  113: lapply
  112: ggplot_build
  111: tns$ggthematic_build
  110: gg2list
  109: ggplotly.ggplot
  107: renderPlotly [#299]
  106: func
  103: shinyRenderWidget
  102: func
   89: renderFunc
   88: output$risk_drivers_plot
    3: runApp
    2: print.shiny.appobj
    1: <Anonymous>
