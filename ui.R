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
library(markdown)
library(kableExtra)

# Load data ----
flights_domesc <- read.csv("data/flights_domesc.csv", header = T, stringsAsFactors = F)
flights_int <- read.csv("data/flights_int.csv", header = T, stringsAsFactors = F)
pax_domesc <- read.csv("data/pax_domesc.csv", header = T, stringsAsFactors = F)
pax_int <- read.csv("data/pax_int.csv", header = T, stringsAsFactors = F)
tua_data <- read.csv("data/tua_data.csv", header = T, stringsAsFactors = F)
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
source("simple_cap.R")
source("mkt_exposure.R")

#Design UI ---

ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = strong(div("")),
  tabPanel(
    title = strong("Intro"),
    includeMarkdown("mam_intro.Rmd")
  ),
  tabPanel(
    title = strong(div("Statistics by airport")),
    fluidRow(
      column(3,
             selectInput(inputId = "airport", 
                         label = "Airport",
                         choices = sort(unique(c(pax_domesc$Origin,pax_domesc$Destination))),
                         selected = "", multiple = F),
             selectInput(inputId = "type2",
                         label = "Traffic type",
                         choices = c("Domestic", "International", "Consolidated"),
                         selected = "Consolidated", multiple = F),
             selectInput(inputId = "type3",
                         label = "Variable type",
                         choices = c("Passengers", "Flights", "Passengers per flight"),
                         selected = "", multiple = F),
             dateRangeInput(inputId = "date_range", 
                            label = "Period (YYYY-MM-DD)", 
                            start = as.Date("2018-01-01", format = "%Y-%m-%d"), 
                            end = as.Date("2019-01-01", format = "%Y-%m-%d"), 
                            min = as.Date("2001-01-01", format = "%Y-%m-%d"),
                            max = as.Date("2019-01-01", format = "%Y-%m-%d"), 
                            format = "yyyy-mm-dd", startview = "month",
                            weekstart = 0, language = "en", separator = " to ", width = NULL,
                            autoclose = TRUE),
             style = "background-color:#FCFDFF;"),
      column(9,
             br(),
             br(),
             htmlOutput(outputId = "airport_info"))
    ),
    fluidRow(
      column(12,
             h4(strong(textOutput(outputId = "ts_header"))),
             highchartOutput(outputId = "traffic_ts"))
    ),
    fluidRow(
      column(6,
             h4(strong(textOutput(outputId = "mkt_header2"))),
             highchartOutput(outputId = "mkt_exposure2")),
      column(6,
             h4(strong(textOutput(outputId = "mkt_header"))),
             highchartOutput(outputId = "mkt_exposure")
      )
    )
  ),
  
  
  
  tabPanel(
    title = strong("Statistics by company"),
    fluidRow(
      column(3,
             selectInput(
               inputId = "company",
               label = "Airport Group",
               choices = c("ASUR","GAP","OMA","Mexico City","Other"),
               selected = "", multiple = F)),
      column(3,
             selectInput(inputId = "type2.c",
                         label = "Traffic type",
                         choices = c("Domestic", "International", "Consolidated"),
                         selected = "", multiple = F)),
      column(3,
             selectInput(inputId = "type3.c",
                         label = "Variable type",
                         choices = c("Passengers", "Flights", "Passengers per flight"),
                         selected = "", multiple = F)),
      column(3,
             dateRangeInput(inputId = "date_range.c", 
                            label = "Period (YYYY-MM-DD)", 
                            start = as.Date("2018-01-01", format = "%Y-%m-%d"), 
                            end = as.Date("2019-01-01", format = "%Y-%m-%d"), 
                            min = as.Date("2001-01-01", format = "%Y-%m-%d"),
                            max = as.Date("2019-01-01", format = "%Y-%m-%d"), 
                            format = "yyyy-mm-dd", startview = "month",
                            weekstart = 0, language = "en", separator = " to ", width = NULL,
                            autoclose = TRUE))
      
    )
  )
)







# Server logic ----
server <- function(input, output){
  #We begin by creating the destination map:
  output$destination_map <- renderHighchart({
    
    input_code <- pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)]
    
    #Retreive destinations for selected airport.
    
    if(input$type2 == "Consolidated"){type2 = "domestic"}else{type2 = tolower(input$type2)}
    
    test <- convert_ts(input_code,
                       type.1 = "airport",
                       type.2 = type2,
                       type.3 = "flights",
                       start.date = input$date_range[1] %m-% months(12),
                       end.date = input$date_range[2] 
    )
    
    validate(
      if(length(test[paste0(input$date_range[1],"/",input$date_range[2])]) == 0){
        paste0("Ooops, looks like ",simpleCap(input$airport)," doesn't have ",tolower(input$type2)," ",tolower(input$type3)," for the selected dates.\n",
               "Please change your selection.")
      } else {NULL}
    )
    
    destinations_df <- find_destinations(airport = input_code,
                                         start_date = input$date_range[1],
                                         end_date = input$date_range[2],
                                         type.2 = tolower(input$type2),
                                         type.3 = tolower(input$type3),
                                         mkt_share = T)
    #Make a data.frame with airport locations and variable.
    airport_destinations <- merge(destinations_df,airport_codes[,c(4,6,10,12,13)], 
                                  by.x = "destinations", by.y = "iata_code", all = F)
    
    
    main_destinations <- top_n(airport_destinations, min(length(airport_destinations$destinations),5), mkt_share)
    secondary_destinations <- top_n(airport_destinations, 
                                    5 - length(airport_destinations$destinations), 
                                    mkt_share)
    
    data_main <- data.frame(
      name = main_destinations$destinations,
      lon = main_destinations$long,
      lat = main_destinations$lat,
      z = main_destinations$mkt_share
    )
    
    data_secondary <- data.frame(
      name = secondary_destinations$destinations,
      lon = secondary_destinations$long,
      lat = secondary_destinations$lat,
      z = secondary_destinations$mkt_share
    )
    
    airport_locations <- data.frame(
      name = input_code,
      lon = airport_codes$long[match(input_code, airport_codes$iata_code)],
      lat = airport_codes$lat[match(input_code, airport_codes$iata_code)]
    )
    
    country_mkt <- airport_destinations[,c("mkt_share","iso_country")] 
    
    
    
    if(input$type3 == "Passengers per flight"){
      aux.label <- "<b>Avg. pax per flight:</b> {point.z}"
      country_mkt <- country_mkt %>%
        group_by(iso_country) %>%
        summarise(value = mean(mkt_share))
    } else {
      data_main$z <- data_main$z*100
      data_secondary$z <- data_secondary$z*100
      aux.label <- "<b>Mkt. share:</b> {point.z}%"
      country_mkt <- country_mkt %>%
        group_by(iso_country) %>%
        summarise(value = sum(mkt_share)*100)
    }
    
    highchart(type = "map") %>%
      hc_add_series(mapData = world_map,
                    showInLegend = F,
                    data = country_mkt,
                    joinBy = c("hc-a2", "iso_country"),
                    value = "value",
                    name = "Market share",
                    dataLabels = list(enabled = TRUE, format = "{point.name}"),
                    borderColor = "#FAFAFA", borderWidth = 0.1,
                    tooltip = list(valueDecimals = 2, valueSuffix = "%")) %>%
      hc_colorAxis(minColor = "#C5C5C5", maxColor = "#1C1C1C", showInLegend = F) %>%
      hc_add_series(airport_locations, type = "mappoint", name = "Airport location", maxSize = "10%", color = "#E2A206",
                    tooltip = list(pointFormat = "{point.properties.z}")) %>%
      hc_add_series(data = data_main,
                    type = "mappoint", name = "Top 5 destinations", maxSize = "10%",
                    marker = list(fillColor = "white", lineColor = "red", lineWidth = 2, radius = 3, symbol = "circle",
                                  style = list(color = "White")),
                    tooltip = list(headerFormat = "",
                                   pointFormat = paste0("<b>Airport: </b> {point.name}<br>",
                                                        aux.label))) %>%
      hc_add_series(data = data_secondary,
                    type = "mappoint", name = "Other destinations", maxSize = "10%",
                    enabled = F,
                    marker = list(fillColor = "white", lineColor = "black", lineWidth = 2, radius = 3, symbol = "circle"),
                    tooltip = list(headerFormat = "",
                                   pointFormat = paste0("<b>Airport: </b> {point.name}<br>",
                                                        aux.label)),
                    visible = F) %>%
      hc_mapNavigation(enabled = TRUE, buttonOptions = list(verticalAlign = "top", align = "right")) %>%
      hc_legend(verticalAlign = "top")
  })
  
  #Airport info table:
  output$airport_info <- renderText({
    
    input_code <- pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)]
    operated_by <- pax_cons$Origin_Airport_Group[match(input$airport,pax_domesc$Origin)]
    dom_tua <- tua_data[192,input_code]
    int_tua <- tua_data[96,input_code]
    
    airport_table <- data.frame(c("Airport: ", "Operated by: ", "Estimated domestic aeronautical tariff: ","Estimated international aeronautical tariff: "),
                                c(simpleCap(input$airport), operated_by, paste0("$",dom_tua), paste0("$", int_tua)))
    
    kable(airport_table, col.names = NULL) %>%
      kable_styling(c("striped", "bordered"), full_width = F) %>%
      column_spec(1, bold = T)
    
  })
  
  #Destination exposure
  output$mkt_exposure2 <- renderHighchart({
    
    
    input_code <- pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)]
    
    
    if(input$type2 == "Consolidated"){type2 = "domestic"}else{type2 = tolower(input$type2)}
    
    test <- convert_ts(input_code,
                       type.1 = "airport",
                       type.2 = type2,
                       type.3 = "flights",
                       start.date = input$date_range[1] %m-% months(12),
                       end.date = input$date_range[2] 
    )
    
    validate(
      if(length(test[paste0(input$date_range[1],"/",input$date_range[2])]) == 0){
        ""
      } else {NULL}
    )
    
    
    if(input$type3 == "Passengers per flight"){
      
      market <- market_exposure(input_code, start.date = input$date_range[1],
                                end.date = input$date_range[2],
                                type.2 = tolower(input$type2),
                                type.3 = tolower(input$type3),
                                n = 200)
      
      
      aux.txt <- "Avg. pax per flight"
      
      hchart(market, "bar", hcaes(x = destinations, y = mkt_share, color = coloract), name = aux.txt,
             tooltip = list(pointFormat = paste0("Airport: {point.destinations} <br>",
                                                 "Operated by: {point.Origin_Airport_Group} <br>",
                                                 aux.txt, ": {point.mkt_share}"))) %>%
        hc_chart(polar = T) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_subtitle(text = "Source: SCT")
      
    } else {
      
    
    market <- market_exposure(input_code, start.date = input$date_range[1],
                                end.date = input$date_range[2],
                                type.2 = tolower(input$type2),
                                type.3 = tolower(input$type3),
                                n = 10)  
        
    aux.txt <- "Exposure"
    
    hchart(market, "waterfall", hcaes(x = destinations, y = mkt_share, color = coloract), name = aux.txt,
           tooltip = list(pointFormat = paste0("Airport: {point.destinations} <br>",
                                               "Operated by: {point.Origin_Airport_Group} <br>",
                                               aux.txt, ": {point.mkt_share} %"))) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = aux.txt)) %>%
      hc_subtitle(text = "Source: SCT")
    }
    
  })
  
  #Header text output:
  output$ts_header <- renderText({
    paste0(simpleCap(input$airport),"'s ", tolower(input$type3), " time series: ")
  })
  
  #Header Mkt.share output:
  output$mkt_header2 <- renderText({
    if(input$type3 == "Passengers per flight"){
      "Avg. pax per flight for top destinations:"
    }else{"Exposure to main destinations:"}
  })
  
  output$mkt_header <- renderText({
    if(input$type3 == "Passengers per flight"){
      "Avg. pax per flight for each airport group:"
    }else{"Exposure to each airport group:"}
  })
  
  #Routes under stress 
  output$mkt_exposure <- renderHighchart({
    
    input_code <- pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)]
    
    if(input$type2 == "Consolidated"){type2 = "domestic"}else{type2 = tolower(input$type2)}
    
    test <- convert_ts(input_code,
                       type.1 = "airport",
                       type.2 = type2,
                       type.3 = "flights",
                       start.date = input$date_range[1] %m-% months(12),
                       end.date = input$date_range[2] 
    )
    
    validate(
      if(length(test[paste0(input$date_range[1],"/",input$date_range[2])]) == 0){
        ""
      } else {NULL}
    )
    
    
    destinations <- find_destinations(input_code,
                                      start_date = input$date_range[1],
                                      end_date = input$date_range[2],
                                      type.2 = tolower(input$type2),
                                      type.3 = tolower(input$type3),
                                      mkt_share = F)
    
    if(input$type3 != "Passengers per flight"){
      destinations <- colSums(destinations)
      destinations <- data.frame(airport = names(destinations), pax = destinations)
      destinations <- unique(merge(destinations, pax_cons[,c(1,5)],by.x = "airport", by.y = "Origin_IATA", all.x = F, all.y = F))
      

      destinations <- destinations %>%
          group_by(Origin_Airport_Group) %>%
          summarise(pax = sum(pax))
        
      destinations$mkt_share <- round(destinations$pax/sum(destinations$pax), digits = 2)*100
        
      airport_colors <- data.frame(Origin_Airport_Group = c("ASUR", "GAP", "OMA", "Mexico City", "International", "Other"),
                                     coloract = c("#376B7F", "#58436E", "#AB503C", "#538059", "#D3C771", "#948F8F"))
        
      destinations <- merge(destinations, airport_colors)
      names(destinations) <- c("variable", "pax", "mkt_share", "coloract")
      
      
      if(input$type3 == "Flights"){aux.txt <- "Flights operated:"} else{aux.txt <- "PAX transported:"}
      
      hchart(destinations, "waterfall", hcaes(x = variable, y = mkt_share, color = coloract),
             tooltip = list(pointFormat = paste0("Exposure: {point.mkt_share} % <br>",
                                                 paste0(aux.txt," {point.pax}")))) %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = "Exposure")) %>%
          hc_subtitle(text = "Source: SCT")
    } else{
      destinations <- colMeans(destinations, na.rm = T)
      destinations <- data.frame(airport = names(destinations), pax = destinations)
      destinations <- unique(merge(destinations, pax_cons[,c(1,5)],by.x = "airport", by.y = "Origin_IATA", all.x = F, all.y = F))
      
      
      destinations <- destinations %>%
        group_by(Origin_Airport_Group) %>%
        summarise(pax = round(mean(pax), digits = 1))
      
      airport_colors <- data.frame(Origin_Airport_Group = c("ASUR", "GAP", "OMA", "Mexico City", "International", "Other"),
                                   coloract = c("#376B7F", "#58436E", "#AB503C", "#538059", "#D3C771", "#948F8F"))
      
      destinations <- merge(destinations, airport_colors)
      names(destinations) <- c("variable", "pax", "coloract")
      
      
      hchart(destinations, "bar", hcaes(x = variable, y = pax, color = coloract),
             tooltip = list(pointFormat = paste0("Avg. pax per flight: {point.pax} "))) %>%
        hc_chart(polar = T) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_subtitle(text = "Source: SCT")
    }
    

    
    
  })
  
  #Traffic time series
  output$traffic_ts <- renderHighchart({
    
    input_code <- pax_cons$Origin_IATA[match(input$airport,pax_domesc$Origin)]
    
    if(input$type3 == "Passengers per flight"){
      #Pax data
      pax_selection <- convert_ts(input_code,
                                  type.1 = "airport",
                                  type.2 = tolower(input$type2),
                                  type.3 = "passengers",
                                  start.date = input$date_range[1] %m-% months(12),
                                  end.date = input$date_range[2]
      )
      #Flight data
      flight_selection <- convert_ts(input_code,
                                     type.1 = "airport",
                                     type.2 = tolower(input$type2),
                                     type.3 = "flights",
                                     start.date = input$date_range[1] %m-% months(12),
                                     end.date = input$date_range[2] 
      )
      #Data
      data <- round(pax_selection/flight_selection, digits = 1)
    } else{
      data <- convert_ts(input_code,
                         type.1 = "airport",
                         type.2 = tolower(input$type2),
                         type.3 = tolower(input$type3),
                         start.date = input$date_range[1] %m-% months(12),
                         end.date = input$date_range[2] 
      )
    }
    
    validate(
      if(length(data[paste0(input$date_range[1],"/",input$date_range[2])]) == 0){
        paste0("Ooops, looks like ",simpleCap(input$airport)," doesn't have ",tolower(input$type2)," ",tolower(input$type3)," for the selected dates.\n",
               "Please change your selection.")
      } else {NULL}
    )
    
    growth <- round(diff(data, lag = 12)/lag.xts(data, k = 12), digits = 2)
    growth[is.na(growth)] <- 0
    plot_data <- fortify(data[paste0(input$date_range[1],"/",input$date_range[2])])
    names(plot_data) <- c("Index", "data")
    plot_data <- cbind(plot_data, growth = coredata(growth[paste0(input$date_range[1],"/",input$date_range[2])]))
    plot_data$type <- plot_data$growth > 0
    plot_data <- mutate(plot_data, coloract = colorize(type, c("#AEB0B3","#464749")))
    
    highchart(type = "stock") %>%
      hc_yAxis_multiples(create_yaxis(2, height = c(4,4), turnopposite = T)) %>%
      hc_add_series(plot_data, hcaes(x = Index, y = data), yAxis = 0, type = "line", color = "red", name = input$type3) %>%
      hc_add_series(plot_data, hcaes(x = Index, y = growth*100, color = coloract), 
                    yAxis = 1, type = 'column', 
                    name = "YoY change (%)") %>%
      #hc_title(text = paste0(input$type3, " YoY% growth (", paste(as.character(input$date_range), collapse = " to "),")"), 
      #         style = list(fontWeight = "bold")) %>%
      hc_subtitle(text = "Source: SCT") %>%
      hc_legend(verticalAlign = "top") 
    
  })
  
}


shinyApp(ui, server)
