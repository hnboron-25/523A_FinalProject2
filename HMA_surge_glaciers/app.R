library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(dplyr)
library(plotly)

RGIdf <- read.csv("C:/Users/hnbor/Desktop/Environmental Data Science Applications/GitProjects/523A_FinalProject2/HMA_surge_glaciers/data/RGIcleaned.csv")

# ----------------------------------------------------------
# UI
# ----------------------------------------------------------
ui <- page_sidebar(
  title = div(
    h2("Surge-type Glaciers of High Mountain Asia", class = "mt-2 mb-1"),
    p("Explore spatial patterns and glacier attributes across HMA.", class = "text-muted mb-3")
  ),
  
  # ---- THEME ----
  theme = bs_theme(
    version = 5,
    bootswatch = "solar",      # Try: minty, litera, quartz, cyborg, sandstone, etc.
    primary = "#3269a8",
    base_font = font_google("Inter"),
    heading_font = font_google("Montserrat"),
    secondary = "#839dc9"
  ),
  
  # ============================================================
  # LEFT SIDEBAR (changes by TAB)
  # ============================================================
  sidebar = sidebar(
    open = "always",
    width = "320px",
    class = "px-2",
    
    # MAP CONTROLS
    conditionalPanel(
      condition = "input.tabs == 'Map'",
      h4("Map Filters", class = "mt-2"),
      card(
        class = "p-2",
        checkboxGroupInput(
          "o1region_map", "Region",
          choices = list(13, 14, 15),
          selected = c(13, 14, 15)
        ),
        checkboxGroupInput(
          "surge_type_map", "Surge Status",
          choiceNames = list("Not Surge-Type", "Possible", "Probable", "Observed"),
          choiceValues = list(0, 1, 2, 3),
          selected = c(1, 2, 3)
        ),
        sliderInput(
          "zmean_map", "Mean Elevation (m)",
          min = 3000, max = 7500,
          value = c(3000, 7500)
        )
      )
    ),
    
    # BOXPLOT CONTROLS
    conditionalPanel(
      condition = "input.tabs == 'Elevation Distribution'",
      h4("Boxplot Controls", class = "mt-2"),
      card(
        class = "p-2",
        checkboxGroupInput(
          "surge_filter_boxplot",
          "Include glacier types:",
          choiceNames = list("Non-Surge", "Possible", "Probable", "Observed Surge-Type"),
          choiceValues = list(0, 1, 2, 3),
          selected = c(0, 3)
        ),
        
        sliderInput(
          "zmean_box", "Mean Elevation (m)",
          min = 3000, max = 7500,
          value = c(3000, 7500)
        ),
        
      )
    ),
    
    # SCATTERPLOT CONTROLS
    conditionalPanel(
      condition = "input.tabs == 'Slope vs Area'",
      h4("Scatterplot Controls", class = "mt-2"),
      card(
        class = "p-2",
        checkboxGroupInput(
          "surge_filter_scatter",
          "Glacier Types to Show:",
          choiceNames = list(
            "Non-Surge-Type",
            "Possible",
            "Probable",
            "Observed Surge-Type"
          ),
          choiceValues = list(0, 1, 2, 3),
          selected = c(0, 3)
        ),
        checkboxInput("log_x", "Log-scale X-axis (Slope)", FALSE),
        checkboxInput("log_y", "Log-scale Y-axis (Area)", FALSE)
      )
    )
  ),
  
  # ============================================================
  # MAIN CONTENT AREA
  # ============================================================
  layout_columns(
    col_widths = c(12),
    card(
      full_screen = TRUE,
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        
        # --- MAP TAB ---
        tabPanel(
          "Map",
          div(class = "mt-3"),
          leafletOutput("map", height = "650px")
        ),
        
        # --- BOXPLOT TAB ---
        tabPanel(
          "Elevation Distribution",
          div(class = "mt-3"),
          plotlyOutput("elev_compare_plot", height = "550px")
        ),
        
        # --- THIRD TAB ---
        tabPanel(
          "Slope vs Area",
          div(class = "mt-3",
              plotlyOutput("scatter_slope_area", height = "550px"))
        )
      )
    )
  )
)

# ----------------------------------------------------------
# SERVER
# ----------------------------------------------------------
server <- function(input, output) {
  
  surge_labels <- c(
    "0" = "Not Surge-Type",
    "1" = "Possible",
    "2" = "Probable",
    "3" = "Observed"
  )
  
  # -------------------------
  # MAP DATA REACTIVE
  # -------------------------
  RGI_map <- reactive({
    df <- RGIdf %>%
      filter(
        o1region %in% input$o1region_map,
        surge_type %in% input$surge_type_map,
        zmean_m >= input$zmean_map[1],
        zmean_m <= input$zmean_map[2]
      )
    df$surge_text <- surge_labels[as.character(df$surge_type)]
    df
  })
  
  pal <- colorFactor(palette = "Dark2", domain = RGIdf$o1region)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = RGI_map(),
        lng = ~cenlon,
        lat = ~cenlat,
        radius = 4,
        color = ~pal(o1region),
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>RGI ID:</b> ", rgi_id, "<br>",
          "<b>Name:</b> ", glac_name, "<br>",
          "<b>Region:</b> ", o1region, "<br>",
          "<b>Surge Type:</b> ", surge_text, "<br>",
          "<b>Mean Elevation:</b> ", zmean_m
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = RGIdf$o1region, title = "Region")
  })
  
  
  # -------------------------
  # BOXPLOT REACTIVE
  # -------------------------
  RGI_boxplot <- reactive({
    RGIdf %>%
      filter(
        surge_type %in% input$surge_filter_boxplot,
        zmean_m >= input$zmean_box[1],       # <-- FILTER ADDED
        zmean_m <= input$zmean_box[2]
      ) %>%
      mutate(surge_label = factor(
        surge_type,
        levels = c(0, 1, 2, 3),
        labels = c("Non-Surge", "Possible Surge", "Probable Surge", "Observed Surge")
      ))
  })
  
  # -------------------------
  # BOXPLOT PLOTLY
  # -------------------------
  output$elev_compare_plot <- renderPlotly({
    df <- RGI_boxplot()
    
    plot_ly(
      data = df,
      x = ~surge_label,
      y = ~zmean_m,
      type = "box",
      color = ~surge_label
    ) %>%
      layout(
        title = list(
          text = "Elevation Comparison",
          x = 0.05,
          font = list(size = 20)
        ),
        xaxis = list(title = "Glacier Type"),
        yaxis = list(title = "Mean Elevation (m)"),
        margin = list(l = 70, r = 30, t = 80, b = 70),
        showlegend = FALSE
      )
  })
  
  # ============================
  # 3rd Tab: SCATTERPLOT
  # ============================
  RGI_scatter <- reactive({
    RGIdf %>%
      filter(surge_type %in% input$surge_filter_scatter) %>%
      mutate(
        surge_label = surge_labels[as.character(surge_type)]
      )
  })
  
  # -------------------------
  # NEW: SCATTERPLOT OUTPUT
  # -------------------------
  output$scatter_slope_area <- renderPlotly({
    df <- RGI_scatter()
    
    plot_ly(
      data = df,
      x = ~slope_deg,
      y = ~area_km2,
      type = "scatter",
      mode = "markers",
      color = ~surge_label,
      marker = list(size = 8, opacity = 0.7)
    ) %>%
      layout(
        title = list(text = "Slope vs Area by Glacier Surge Type", x = 0.05),
        xaxis = list(title = "Slope (degrees)",
                     type = ifelse(input$log_x, "log", "linear")
                     ),
        yaxis = list(title = "Area (km²)",
                     type = ifelse(input$log_y, "log", "linear"))
      )
  })
}

shinyApp(ui, server)
