#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Set up for Shiny App
library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(dplyr)
library(plotly)

# Read in cleaned RGI data
RGIdf <- read.csv("C:/Users/hnbor/Desktop/Environmental Data Science Applications/GitProjects/523A_FinalProject2/HMA_surge_glaciers/data/RGIcleaned.csv")
#RGIdf <- read.csv("data/RGIcleaned.csv")


# Define UI
ui <- fluidPage(
  # App Title
  titlePanel("Surge-type Glaciers of High Mountain Asia (HMA)"),
  
  # Info text
  h4("View spatial distribution of surging glaciers and their attributes across HMA"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for widgets that users can interact with
    sidebarPanel(
      # Input: select region
      checkboxGroupInput(
        inputId = "o1region",
        label = "Region",
        choices = list(13, 14, 15),
        # selected = sets which are selected by default
        selected = c(13, 14, 15)
      ),
      
      # Input: Filter points by observation type
      checkboxGroupInput(
        inputId = "surge_type",
        label = "Surge Status",
        choiceNames = list("Not Surge-Type", "Possible", "Probable", "Observed"),
        choiceValues = list(0, 1, 2, 3),
        selected = c(1, 2, 3)
      ),
      
      # Input: Filter by Elevation
      sliderInput(
        inputId = "zmean_m",
        label = "Mean Elevation (m above sea level)",
        min = 3000,
        max = 7500,
        value = c(3000, 7500)
      )
    ),
    
    # Main panel for displaying map
    mainPanel(
      tabsetPanel(
      tabPanel("Map",
               br(),
               leafletOutput("map", height = "600px")),
      tabPanel("Elevation Distribution",
               br(),
               plotlyOutput("elev_compare_plot", height = "500px")),
      tabPanel("Summary Table",
               br(),
               "summary_table"))
      )
    )
  )




# Define Server
server <- function(input, output) {
  # Make reactive object for the RGI data by calling RGI IDs to extract the values the user chose
  RGI_react <- reactive(
    RGIdf %>%
      filter(o1region %in% input$o1region) %>%
      filter(surge_type %in% input$surge_type) %>%
      filter(zmean_m >= input$zmean_m[1] &
               zmean_m <= input$zmean_m[2])
  )
  
  # Render the map based on our reactive occurrence dataset
  
  # Create color palette for Regions
  pal <- colorFactor(palette = "Dark2", domain = RGIdf$o1region)
  output$map <- renderLeaflet({
    # Create leaflet map
    leaflet() %>%
      addTiles() %>%
      # Add glacier points
      addCircleMarkers(
        data = RGI_react(),
        lng = ~ cenlon,
        lat = ~ cenlat,# Note the () after occ_react!
        radius = 4,
        color = ~ pal(o1region),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~ paste0(
          "<b>RGI ID:</b> ", rgi_id, "<br>",
          "<b>Glacier Name:</b>", glac_name, "<br>",
          "<b>Region:</b> ", o1region, "<br>",
          "<b>Surge Type:</b> ", surge_type, "<br>",
          "<b>Mean Elevation (m):</b> ", zmean_m
          
        )
      ) %>%
      # Add legend
      addLegend(
        position = "bottomright",
        pal = pal,
        values = RGIdf$o1region,
        title = "Region"
      )
    
  })

# Create elevation plot
  output$elev_compare_plot <- renderPlotly({
    
    df <- RGI_react()
    
    # Filter for non-surge (0) and observed surge-type (3)
    df_sub <- df %>%
      filter(surge_type %in% c(0, 3)) %>%
      mutate(surge_label = factor(surge_type,
                                  levels = c(0, 3),
                                  labels = c("Non-Surge", "Observed Surge")))
    
    # Create interactive boxplot
    plot_ly(
      data = df_sub,
      x = ~surge_label,
      y = ~zmean_m,
      type = "box",
      color = ~surge_label
    ) %>%
      layout(
        title = "Elevation Comparison: Non-Surge vs Observed Surge-Type Glaciers",
        xaxis = list(title = "Glacier Type"),
        yaxis = list(title = "Mean Elevation (m above sea level)"),
        showlegend = FALSE
      )
  })
  
  
}
# Run the app
shinyApp(ui = ui, server = server)






