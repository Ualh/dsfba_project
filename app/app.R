preprocessed_data <- read.csv("../data/app_data.csv")
swiss_cantons <- st_read("../data/CH_map/swissBOUNDARIES3D_1_4_TLM_KANTONSGEBIET.shp")

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map of Vehicle Data in Swiss Cantons"),
  selectInput("fuelType", "Choose a Fuel Type:", choices = unique(preprocessed_data$Fuel)),
  selectInput("vehicleType", "Choose a Vehicle Type:", choices = unique(preprocessed_data$VehicleType)),
  sliderInput("year", "Select Year:", 
              min = min(preprocessed_data$Year), 
              max = max(preprocessed_data$Year), 
              value = min(preprocessed_data$Year),
              step = 1),
  plotlyOutput("map")
)

# Define Server Logic
server <- function(input, output) {
  output$map <- renderPlotly({
    # Filter the preprocessed data based on input
    filtered_data <- preprocessed_data %>%
      filter(Fuel == input$fuelType, 
             VehicleType == input$vehicleType,
             Year == input$year)
    
    # Merge with shapefile data
    map_data <- merge(swiss_cantons, filtered_data, by = "KANTONSNUM")
    
    # Create the plot
    p <- ggplot() +
      geom_sf(data = map_data, aes(fill = Total), color = NA) +
      scale_fill_viridis_c() +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the App
shinyApp(ui, server)
#rsconnect::deployApp(here("app"))