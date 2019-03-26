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
library(shinythemes)
library(rjson)

# Load data ----
flights_domesc <- read.csv("data/flights_domesc.csv", header = T, stringsAsFactors = F)
flights_int <- read.csv("data/flights_int.csv", header = T, stringsAsFactors = F)
pax_domesc <- read.csv("data/pax_domesc.csv", header = T, stringsAsFactors = F)
pax_int <- read.csv("data/pax_int.csv", header = T, stringsAsFactors = F)
pax_cons <- rbind(pax_domesc,pax_int)

flights_domesc[is.na(flights_domesc)] <- 0
flights_int[is.na(flights_int)] <- 0
pax_domesc[is.na(pax_domesc)] <- 0
pax_int[is.na(pax_int)] <- 0
pax_cons[is.na(pax_cons)] <- 0


airport_codes <- read.csv("data/airport_codes_2.csv", header = T, stringsAsFactors = F)
mx_map <- fromJSON(file = "countries/mx-all.geo.json")
world_map <- fromJSON(file = "countries/world-palestine-highres.geo.json")

# Source helper functions -----
source("convert_ts.R")
source("find_destinations.R")

#Design UI ---

ui <- navbarPage(
    theme = shinytheme("cosmo"),
    title = strong(div("Mexican Air Monitor", 
                style = "font-family:frutiger;font-size:125%; text-align:center; vertical-align:midlle")),
    tabPanel(
      HTML('<p style="font-family:frutiger;
           font-size:125%; text-align:center; 
           vertical-align:midlle"> 
           <b>Statistics by airport</b></p>'),
      fluidRow(
        column(3,
               selectInput(inputId = "airport", 
                           label = "Airport",
                           choices = sort(unique(c(pax_domesc$Origin,pax_domesc$Destination))),
                           selected = "", multiple = F)),
        column(3,
               selectInput(inputId = "type2",
                           label = "Traffic type",
                           choices = c("Domestic", "International", "Consolidated"),
                           selected = "", multiple = F)),
        column(3,
               selectInput(inputId = "type3",
                           label = "Variable type",
                           choices = c("Passengers", "Flights", "Passengers per flight"),
                           selected = "", multiple = F)),
        column(3,
               dateRangeInput(inputId = "date_range", 
                              label = "Period (YYYY-MM-DD)", 
                              start = as.Date("2001-01-01", format = "%Y-%m-%d"), 
                              end = as.Date("2019-01-01", format = "%Y-%m-%d"), 
                              min = as.Date("2001-01-01", format = "%Y-%m-%d"),
                              max = as.Date("2019-01-01", format = "%Y-%m-%d"), 
                              format = "yyyy-mm-dd", startview = "month",
                              weekstart = 0, language = "en", separator = " to ", width = NULL,
                              autoclose = TRUE))
      ),
      fluidRow(
        column(12,
               highchartOutput(outputId = "destination_map"))
      ),
      fluidRow(
        column(12,
               highchartOutput(outputId = "traffic_ts"))
      ),
      fluidRow(
        column(12,
               dataTableOutput(outputId = "traffic_table"))
      )

    )
)







# Server logic ----
server <- function(input, output){
  #We begin by creating the destination map:
  output$destination_map <- renderHighchart({
    #Retreive destinations for selected airport.
    destinations_df <- find_destinations(airport = pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                         start_date = input$date_range[1],
                                         end_date = input$date_range[2],
                                         type.2 = tolower(input$type2),
                                         type.3 = tolower(input$type3),
                                         mkt_share = T)
    #Make a data.frame with airport locations and variable.
    airport_destinations <- merge(destinations_df,airport_codes[,c(4,6,10,12,13)], 
                               by.x = "destinations", by.y = "iata_code", all = F)
    data <- data.frame(
      name = airport_destinations$destinations,
      lon = airport_destinations$long,
      lat = airport_destinations$lat,
      z = airport_destinations$mkt_share
    )
    
    airport_locations <- data.frame(
      name = pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
      lon = airport_codes$long[match(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)], airport_codes$iata_code)],
      lat = airport_codes$lat[match(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)], airport_codes$iata_code)]
    )
    
    
    if(input$type3 == "Passengers per flight"){aux.label <- "<b>Avg. pax per flight:</b> {point.z}"}
    else {aux.label <- "<b>Mkt. share:</b> {point.z}%"}
    
    highchart(type = "map") %>%
      hc_add_series(mapData = world_map, showInLegend = F) %>%
      #hc_title(text = paste0(substr(input$type3,1,nchar(input$type3)-1), " market share per destination (2018)"), align = "left") %>%
      hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#E2A206",
                    tooltip = list(pointFormat = "{point.properties.z}")) %>%
      hc_add_series(data = data,
                    type = "mappoint", name = "Destinations", maxSize = "10%",
                    marker = list(fillColor = "white", lineColor = "black", lineWidth = 2, radius = 3, symbol = "circle"),
                    tooltip = list(headerFormat = "",
                                   pointFormat = paste0("<b>Airport: </b> {point.name}<br>",
                                                        aux.label))) %>%
      hc_mapNavigation(enabled = TRUE, buttonOptions = list(verticalAlign = "bottom")) 
  })
  
  output$traffic_table <- DT::renderDataTable({
    
    data <- as.data.frame(find_destinations(airport = pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                            start_date = input$date_range[1],
                                            end_date = input$date_range[2],
                                            type.2 = tolower(input$type2),
                                            type.3 = tolower(input$type3),
                                            mkt_share = F))
    
    order_vector <- find_destinations(airport = pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                      start_date = input$date_range[1],
                                      end_date = input$date_range[2],
                                      type.2 = tolower(input$type2),
                                      type.3 = tolower(input$type3),
                                      mkt_share = T) 
    
    order_vector <- as.character(order_vector[order(order_vector$mkt_share, decreasing = T),1])
    
    data <- data[, order_vector]
    
    datatable(data,
              extensions = 'Buttons',
              options = list(rownames = T,
                             dom = 'Brtlp',
                             #buttons = list(
                             #   list(
                             #    extend = 'colvis',
                             #     columns = seq(1, ncol(data))
                             #  )),
                             #columnDefs = list(
                             #   list(
                             #    visible = F,
                             #     targets = seq(6, ncol(data))
                             #  )
                             #),
                             scrollX = T,
                             scrollCollapse = TRUE)
              )
  
  })
  
  output$traffic_ts <- renderHighchart({
    
    if(input$type3 == "Passengers per flight"){
      #Pax data
      pax_selection <- convert_ts(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                  type.1 = "airport",
                                  type.2 = tolower(input$type2),
                                  type.3 = "passengers"
      )
      #Flight data
      flight_selection <- convert_ts(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                                     type.1 = "airport",
                                     type.2 = tolower(input$type2),
                                     type.3 = "flights",
                                     start.date = input$date_range[1],
                                     end.date = input$date_range[2]
      )
      #Data
      data <- round(pax_selection/flight_selection, digits = 1)
    } else{
      data <- convert_ts(pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)],
                         type.1 = "airport",
                         type.2 = tolower(input$type2),
                         type.3 = tolower(input$type3),
                         start.date = input$date_range[1],
                         end.date = input$date_range[2]
      )
    }
    
    growth <- round(diff(data, lag = 12)/lag.xts(data, k = 12), digits = 2)
    plot_data <- fortify(data)
    plot_data <- cbind(plot_data, growth = coredata(growth))
    plot_data$type <- plot_data$growth > 0
    
    highchart(type = "stock") %>%
      hc_yAxis_multiples(
        create_yaxis(2, height = c(4,4), turnopposite = T)
      ) %>%
      hc_add_series(plot_data, hcaes(x = Index, y = data), yAxis = 0, type = "line", color = "#E2A206", name = input$type3) %>%
      hc_add_series(plot_data, hcaes(x = Index, y = growth*100, color = type), yAxis = 1, type = 'column', 
                    name = "YoY change (%)")
      #hc_title(text = paste0(input$type3, " YoY% growth ", paste(as.character(input$dateRange), collapse = " to ")))
    
  })
}

shinyApp(ui, server)






