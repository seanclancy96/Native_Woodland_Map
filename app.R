# install.packages('rsconnect')
# 
# rsconnect::setAccountInfo(name='johnmullane',
#                           token='E14FFDCBCE45C038FA23130DF0DDC722',
#                           secret='xL+g04RX63I/sfIygdAPwkTH/yq9lucSiDoQRdxh')
# 
# library(rsconnect)
# rsconnect::deployApp('~/Native_Woodland_Map')

library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
# install.packages("DT")
library(DT)
library(rgdal)
# for metadata/attributes- vectors or rasters
library(raster)

# Reading data 
df_clean <- readRDS(file = "df_clean.rds")

# Setting provinces of counties
munster <- c("Kerry", "Limerick", "Cork", "Tipperary", "Waterford", "Clare")
connacht <- c("Galway", "Leitrim", "Mayo", "Roscommon", "Sligo")
leinster <- c("Kildare", "Carlow", "Kilkenny", "Laois", "Longford", "Louth", 
              "Meath", "Offaly", "Wexford", "Wicklow", "Westmeath", "Dublin")
ulster <- c("Monaghan", "Cavan", "Donegal")
sort(leinster)

# Taking county vals from NPWS filtering to use an error message
NPWS_vals <- df_clean %>% 
  filter(ownership0 %in% "NPWS") 
NPWS_counties <- unique(NPWS_vals$county)

# Shape file creation
# woods <- readOGR("NSNW_Woodland_Habitats_2010.shp")
# shapeData <- spTransform(woods, CRS("+proj=longlat +ellps=GRS80")) # Transform coordinates
# ?absolutePanel
# ?actionButton
# icon()
# ?checkboxInput

# User interface
ui <- navbarPage("Native Woodlands", # id="main",
                 tabPanel("Map", leafletOutput("map", height=950),
                          absolutePanel(top = 80, right = 25,
                                        radioButtons("owner", label = "Ownership", choices = c("All", "Public", "NPWS"), width = '180px'),
                                        selectInput("county", label = "County", choices = list("All Counties", 
                                                                                               'Leinster'= sort(leinster), 
                                                                                               'Munster' = sort(munster), 
                                                                                               'Connacht'= sort(connacht), 
                                                                                               'Ulster'  = sort(ulster)), width = '180px'), 
                                        # selectInput("notes", label = "Field Notes", choices = c("Disabled", "Enabled"), width = '180px'),  
                                        width = '180px')),                                        
                 tabPanel("Data", DT::dataTableOutput("data")),
                 tabPanel("Information", includeMarkdown("README.md"))
                 #,
                 # tabPanel("Shape Map", leafletOutput("shape", height=950))
)


server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(
      if (input$owner %in% "NPWS" && !input$county %in% c(NPWS_counties, "All Counties")) {
        df_clean
      }
      else
        df_clean %>% 
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
      
        addMarkers(
          # reactive(if (x() %in% "Disabled")
          ~lon, ~lat, label = ~htmlEscape(woodland_name), clusterOptions = markerClusterOptions(),
                   popup = ~paste0("<font size=3>", '<strong>', woodland_name, '</strong>',
                                   "<font size=2>", 
                                   "<br/>Conservation status: ", cons_rate, 
                                   "<br/>Threat status: ", threat_rate, 
                                   "<br/>Area (ha): ", area, 
                                   "<br/>Ownership: ", ownership,
                                   "<br/><br/>Field Notes: <br/>", "<font size=1>", field_notes)
                                   #if (input$notes %in% "Enabled"){
                                    # ,
                                     #"<br/>Field Notes: <br/>", field_notes})  
      #}
      
    # else
        
      # ~lon, ~lat, label = ~htmlEscape(woodland_name), clusterOptions = markerClusterOptions(),
      #            popup = ~paste0("<font size=3>", '<strong>', woodland_name, '</strong>',
      #                            "<font size=2>",
      #                            "<br/>Conservation status: ", cons_rate,
      #                            "<br/>Threat status: ", threat_rate,
      #                            "<br/>Area (ha): ", area,
      #                            "<br/>Ownership: ", ownership ,
      #                            "<br/>Field Notes: <br/>", field_notes)
)
  }
)
  
  output$data <- DT::renderDataTable(datatable(
    df_clean[,-c(13,14)], filter = 'top',
    colnames = c("Side ID", "Latitude", "Longitude", "Woodland Name", "Area", "Conservation Rating", "Conservation Score", 
                 "Threat Rating", "Threat Score", "Ownership", "Townland Name", "County")
  )
)
  
  # output$shape <- renderLeaflet(
  #   leaflet() %>% 
  #     addTiles() %>%
  #     addPolygons(data = shapeData, weight = 5, col = 'red')
  # )
  
}

shinyApp(ui, server)
