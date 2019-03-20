# Load packages ----
library(shiny)
library(readxl)
library(tidyverse)
library(xts)
library(DT)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(rsconnect)
library(lubridate)
library(highcharter)
library(geojsonio)

# Load data ----
flights_domesc <- read.csv("data/flights_domesc.csv", header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
flights_int <- read.csv("data/flights_int.csv", header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
pax_domesc <- read.csv("data/pax_domesc.csv", header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
pax_int <- read.csv("data/pax_int.csv", header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
pax_cons <- rbind(pax_domesc,pax_int)
airport_codes <- read.csv("data/airport_codes.csv", header = T, stringsAsFactors = F)
mx_map <- geojson_read(x = "countries/mx-all.geo.json")
#mx_map <- geojsonio::as.json(mx_map)


# Source helper functions -----
source("convert_ts.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Mexican Air Monitor"),
  helpText("The following interface allows you to visualize a variety of key metrics
           of the Mexican air transportation industry."),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h1("Traffic:"),
      radioButtons(inputId = "type1",
                   label = "Select the variable type",
                   choices = c("Airport",
                               "Airport group")),
      
      conditionalPanel(
        condition = "input.type1 == 'Airport group'",
        selectInput(inputId = "airport2", 
                    label = "Choose an airport group",
                    choices = sort(unique(pax_cons$Origin_Airport_Group)),
                    selected = "ASUR")
      ),
      
      conditionalPanel(
        condition = "input.type1 == 'Airport'",
        selectInput(inputId = "airport", 
                    label = "Choose an airport",
                    choices = sort(unique(c(pax_domesc$Origin,pax_domesc$Destination))),
                    selected = "")),
      
      radioButtons(inputId = "type2",
                   label = "Select the type of traffic",
                   choices = c("Domestic",
                               "International",
                               "Consolidated")),
      
      radioButtons(inputId = "type3",
                   label = "Select the operational variable",
                   choices = c("Passengers",
                               "Flights",
                               "Passengers per flight"))

      #conditionalPanel(
        #condition = "input.type1 == 'Airport' && (type2 == 'International' || type2 == 'Consolidated')",
        #selectInput(inputId = "airport_int", 
                    #label = "Choose an airport",
                    #choices = sort(unique(c(pax_cons$Origin,pax_cons$Destination))),
                    #selected = "")),

    ),
    mainPanel(width = 8,
              highchartOutput(outputId = "ts_plot"),
              br(),
              highchartOutput(outputId = "mx_map")
    )
  )
)


# Server logic ----
server <- function(input, output) {
  
  
  output$ts_plot <- renderHighchart({
    
    
    #Use of convert_ts function
    if(input$type1 == "Airport"){
    if(input$type3 == "Passengers per flight"){
      #Pax data
      pax_selection <- convert_ts(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                  type.1 = tolower(input$type1),
                                  type.2 = tolower(input$type2),
                                  type.3 = "passengers"
                                  )
      #Flight data
      flight_selection <- convert_ts(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                     type.1 = tolower(input$type1),
                                     type.2 = tolower(input$type2),
                                     type.3 = "flights"
                                     )
      #Data
      data <- round(pax_selection/flight_selection, digits = 1)
    } else{
      data <- convert_ts(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                         type.1 = tolower(input$type1),
                         type.2 = tolower(input$type2),
                         type.3 = tolower(input$type3)
                         )
    }} else{
      if(input$type3 == "Passengers per flight"){
        #Pax data
        pax_selection <- convert_ts(input$airport2,
                                    type.1 = tolower(input$type1),
                                    type.2 = tolower(input$type2),
                                    type.3 = "passengers"
                                    )
        #Flight data
        flight_selection <- convert_ts(input$airport2,
                                       type.1 = tolower(input$type1),
                                       type.2 = tolower(input$type2),
                                       type.3 = "flights")
        #Data
        data <- round(pax_selection/flight_selection, digits = 1)
      } else{
        data <- convert_ts(input$airport2,
                           type.1 = tolower(input$type1),
                           type.2 = tolower(input$type2),
                           type.3 = tolower(input$type3)
                           )}
    }
    
    growth <- round(diff(data, lag = 12)/lag.xts(data, k = 12), digits = 2)
    plot_data <- fortify(data)
    plot_data$growth <- growth
    plot_data$type <- plot_data$growth > 0
    #plot_data$tooltip <- paste0(input$type3, ": ", plot_data$data, "\n Date: ", plot_data$Index) 

    #Plot ts object
    highchart(type = "stock") %>%
      hc_yAxis_multiples(
        create_yaxis(2, height = c(4,4), turnopposite = T)
      ) %>%
      hc_add_series(plot_data, hcaes(x = Index, y = data), yAxis = 0, type = "line", color = "#E2A206", name = input$type3) %>%
      hc_add_series(plot_data, hcaes(x = Index, y = growth*100, color = type), yAxis = 1, type = 'column', 
                    name = "YoY change (%)")
      #hc_title(text = paste0(input$type3, " YoY% growth ", paste(as.character(input$dateRange), collapse = " to ")))
      
    
  })
  
  output$mx_map <- renderHighchart({
    
    if(input$type1 == "Airport"){
      #Find airport coordinates:
      airport_locations <- data.frame(
        name = input$airport,
        lon = airport_codes$long[match(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)], airport_codes$iata_code)],
        lat = airport_codes$lat[match(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)], airport_codes$iata_code)]
      )
      
      highchart(type = "map") %>%
        #hcmap("countries/mx/mx-all", showInLegend = F) %>%
        hc_add_series(mapData = mx_map, showInLegend = F) %>%
        hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#E2A206",
                      tooltip = list(pointFormat = "{point.properties.z}"))
    } else{
      if(input$airport2 == "ASUR"){
        #Airports operated by ASUR
        names <- c("CUN", "CZM", "HUX", "MID", "MTT", "OAX", "TAP", "VER", "VSA")
        
        airport_locations <- data.frame(
                     name = airport_codes$iata_code[airport_codes$iata_code %in% names],
                     lon = airport_codes$lon[airport_codes$iata_code %in% names],
                     lat = airport_codes$lat[airport_codes$iata_code %in% names],
                     z = 1
                  )
        
        
        highchart(type = "map") %>%
          #hcmap("countries/mx/mx-all", showInLegend = F) %>%
          hc_add_series(mapData = mx_map, showInLegend = F) %>%
          hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#44EBD1",
                        tooltip = list(pointFormat = "{point.properties.z}"))
        
      } else if (input$airport2 == "GAP"){
        #Airports operated by GAP
        names <- c("AGU", "GDL", "HMO", "LAP", "BJX", "SJD", "LMM", "ZLO", "MXL", "MLM", "PVR", "TIJ")
        
        airport_locations <- data.frame(
          name = airport_codes$iata_code[airport_codes$iata_code %in% names],
          lon = airport_codes$lon[airport_codes$iata_code %in% names],
          lat = airport_codes$lat[airport_codes$iata_code %in% names],
          z = 1
        )
        
        
        highchart(type = "map") %>%
          #hcmap("countries/mx/mx-all", showInLegend = F) %>%
          hc_add_series(mapData = mx_map, showInLegend = F) %>%
          hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#AE61EA",
                        tooltip = list(pointFormat = "{point.properties.z}"))
      } else if (input$airport2 == "OMA"){
        #Airports operated by OMA
        names <- c("ACA","CUU","CJS","CUL","DGO","ZIH","MZT","MTY","REX","SLP","TAM","TRC","ZCL")
        
        airport_locations <- data.frame(
          name = airport_codes$iata_code[airport_codes$iata_code %in% names],
          lon = airport_codes$lon[airport_codes$iata_code %in% names],
          lat = airport_codes$lat[airport_codes$iata_code %in% names],
          z = 1
        )
        
        
        highchart(type = "map") %>%
          #hcmap("countries/mx/mx-all", showInLegend = F) %>%
          hc_add_series(mapData = mx_map, showInLegend = F) %>%
          hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "red",
                        tooltip = list(pointFormat = "{point.properties.z}"))
      } else if (input$airport2 == "Mexico City"){
        #Airports operated by MEX
        names <- c("MEX")
        
        airport_locations <- data.frame(
          name = airport_codes$iata_code[airport_codes$iata_code %in% names],
          lon = airport_codes$lon[airport_codes$iata_code %in% names],
          lat = airport_codes$lat[airport_codes$iata_code %in% names],
          z = 1
        )
        
        
        highchart(type = "map") %>%
          #hcmap("countries/mx/mx-all", showInLegend = F) %>%
          hc_add_series(mapData = mx_map, showInLegend = F) %>%
          hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#197C07",
                        tooltip = list(pointFormat = "{point.properties.z}"))
      } else if (input$airport2 == "Other"){
        #Airports operated by Others
        names <- unique(c(pax_domesc$Origin_IATA,pax_domesc$Destination))
        names <- names[!(names %in% c("ACA","CUU","CJS","CUL","DGO","ZIH","MZT","MTY","REX","SLP","TAM","TRC","ZCL",
                                      "AGU", "GDL", "HMO", "LAP", "BJX", "SJD", "LMM", "ZLO", "MXL", "MLM", "PVR", "TIJ",
                                      "MEX", "CUN", "CZM", "HUX", "MID", "MTT", "OAX", "TAP", "VER", "VSA"))]
        
        airport_locations <- data.frame(
          name = airport_codes$iata_code[airport_codes$iata_code %in% names],
          lon = airport_codes$lon[airport_codes$iata_code %in% names],
          lat = airport_codes$lat[airport_codes$iata_code %in% names],
          z = 1
        )
        
        
        highchart(type = "map") %>%
          #hcmap("countries/mx/mx-all", showInLegend = F) %>%
          hc_add_series(mapData = mx_map, showInLegend = F) %>%
          hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#CA6207",
                        tooltip = list(pointFormat = "{point.properties.z}"))
      }
    }

  })
  
  
  
}

# Run app ----
shinyApp(ui, server)

