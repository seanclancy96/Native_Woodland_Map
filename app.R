library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

# Reading data 
df_clean <- readRDS(file = "df_clean.rds")

# Setting provinces of counties
munster <- c("Kerry", "Limerick", "Cork", "Tipperary", "Waterford", "Clare")
connacht <- c("Galway", "Leitrim", "Mayo", "Roscommon", "Sligo")
leinster <- c("Kildare", "Carlow", "Kilkenny", "Laois", "Longford", "Louth", 
              "Meath", "Offaly", "Wexford", "Wicklow", "Westmeath", "Dublin")
ulster <- c("Monaghan", "Cavan", "Donegal")

# User Interface
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map",width="100%", height ="100%"),
  
  
  absolutePanel(top = 10, right = 10,
                radioButtons("owner", label = "Ownership", choices = c("All", "Public", "NPWS"), width = '180px'),
                selectInput("county", label = "County", choices = c("All Counties", "Munster", "Leinster", "Connacht", "Ulster",
                                                                    df_clean$county %>% 
                                                                      unique() %>% # Taking unique county vals
                                                                      sort(), # Sorting counties alphabetically to improve navigation
                                                                    ), width = '180px'))
)

# Server
server <- function(input, output, session) {

  output$map <- renderLeaflet({leaflet(df_clean %>% 
                                         filter(if (input$county %in% "All Counties") county %in% county 
                                                else if (input$county %in% "Munster") county %in% munster
                                                else if (input$county %in% "Leinster") county %in% leinster
                                                else if (input$county %in% "Connacht") county %in% connacht
                                                else if (input$county %in% "Ulster") county %in% ulster
                                                else county %in% input$county) %>% 
                                         filter(if (input$owner %in% "All") ownership0 %in% ownership0
                                                else if (input$owner %in% "Public") ownership0 %in% c("Public", "NPWS")
                                                else ownership0 %in% input$owner)
  ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
                 popup = ~paste0("<font size=3>", '<strong>', woodland_name, '</strong>',
                                 "<font size=2>", 
                                 "<br/>Conservation status: ", cons_rate, 
                                 "<br/>Threat status: ", threat_rate, 
                                 "<br/>Area (ha): ", area, 
                                 "<br/>Ownership: ", ownership ))}
  )

}

shinyApp(ui, server)
