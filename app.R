library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
# Reading data 
df <- readRDS(file = "df.rds")

# Setting St. Johns Wood Latitude and Longitude to correct coordinates
johns_name <- "St John's Wood"
lat_johns <- 53.556947
lon_johns <- -8.00471653

# Changing details for St. John's wood
johns <- df %>% 
  filter(woodland_name %in% johns_name) %>% 
  mutate(lat = lat_johns, lon = lon_johns) 

df_clean <- df %>% 
  filter(!woodland_name %in% johns_name) %>% 
  add_row(johns)

df_clean %>% filter(woodland_name %in% johns_name) # Checking new data frame contains wood of interest

# Changing empty ownership names to Not Stated
df_clean$ownership[df_clean$ownership==""] <- "Not Stated"

unique(df_clean["ownership"])

df_clean$ownership0 <- ifelse(grepl("private",df_clean$ownership,ignore.case=T),"Private",
                              ifelse(grepl("coillte|other state body|local authority|Bórd na Móna", df_clean$ownership,ignore.case=T),"Public",
                              ifelse(grepl("npws",df_clean$ownership,ignore.case=T),"NPWS","Unknown")))


# Taking non-private woods only
df_clean_non_private <- df_clean %>% 
  filter(!grepl("Private", ownership)) 

# Taking definite non-private woods
df_clean_public_confirmed <- df_clean_non_private %>% 
  filter(!ownership %in% "Not Stated")

ui <- fluidPage(
  navbarPage("NPWS Forests",
             tabPanel("Map",
                      textOutput("text"),
                      selectInput("county", label = "County", choices = c("All Counties",unique(df_clean$county))),
                      radioButtons("owner", label = "Ownership", choices = c("All","Public","NPWS")),
                      leafletOutput("map")
                      )
  )
  
)

server <- function(input, output, session) {
  
  output$text <- renderText({
    "John is cool"
  })
  
 
    
  
  output$map <- renderLeaflet({leaflet(df_clean %>% 
                filter(if (input$county %in% "All Counties") county %in% county 
                       else county %in% input$county) %>% 
                filter(if (input$owner %in% "All") ownership0 %in% ownership0
                       else if (input$owner %in% "Public") ownership0 %in% c("Public","NPWS")
                       else ownership0 %in% input$owner)
    ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
                 popup = ~paste0("<font size=3>", '<strong>', woodland_name, '</strong>',
                                 "<font size=2>", 
                                 "<br/>Conservation status: ", cons_rate, 
                                 "<br/>Threat status: ", threat_rate, 
                                 "<br/>Area (ha): ", area, 
                                 "<br/>Ownership: ", ownership ))})
  
}

shinyApp(ui, server)

