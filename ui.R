# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(readxl)
library(tidyverse)
library(xts)
library(DT)

# Load data ----
flights_domesc <- read_excel(path = paste0("data/Base_File_2.xlsx"), sheet = 1)
flights_int <- read_excel(path = paste0("data/Base_File_2.xlsx"), sheet = 2)
pax_domesc <- read_excel(path = paste0("data/Base_File_2.xlsx"), sheet = 3)
pax_int <- read_excel(path = paste0("data/Base_File_2.xlsx"), sheet = 4)

# Source helper functions -----
source("convert_ts.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Mexican Air Monitor"),
  helpText("The following interface allows you to visualize a variety of key metrics
           of the Mexican air transportation industry."),
  sidebarLayout(
    sidebarPanel(
      h3("Origin:"),
      radioButtons(inputId = "type_origin",
                    label = "Please select the origin",
                    choices = c("By airport",
                                "By airport group")),
      conditionalPanel(
        condition = "input.type_origin == 'By airport'",
        selectInput(inputId = "origin_a", 
                    label = "Choose an airport",
                    choices = sort(unique(pax_domesc$`Origin (IATA)`)),
                    selected = "")),
      conditionalPanel(
        condition = "input.type_origin == 'By airport group'",
        selectInput(inputId = "origin_a", 
                    label = "Choose an airport group",
                    choices = sort(unique(pax_domesc$`Origin Airport Group`)),
                    selected = ""))
      ),
  mainPanel(dataTableOutput(outputId = "table"))
  )
)


# Server logic ----
server <- function(input, output) {
  output$table <- renderDataTable({
    pax_from_selected_airport <- pax_domesc %>%
      filter(`Origin (IATA)` == input$origin_a) %>%
      select(`Destination (IATA)`:`Origin (IATA)`)
    DT::datatable(data=pax_from_selected_airport,
                  options = list(pageLength = 10),
                  rownames = F)
    })
}

# Run app ----
shinyApp(ui, server)
