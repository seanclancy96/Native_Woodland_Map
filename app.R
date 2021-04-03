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
for (i in 1:nrow(df_clean)){  # Looping through rows
  if (df_clean$ownership[i] == ""){ # If ownership is empty
    df_clean$ownership[i]<- "Not Stated" # Assign not stated to entry ownership
  }
}

# Set a new variable which denotes if it is a public or private wood
pub_priv <- c("a")

df_clean2 <- df_clean %>%
  add_column(pub_priv) 

# Using a loop to set a pub/priv indicator for map
for (i in 1:nrow(df_clean2)){
  if (!grepl("Private", df_clean2$ownership[i]) & !df_clean2$ownership[i] %in% "Not Stated"){
    df_clean2$pub_priv[i] <- "Public"
  }
  else {
    df_clean2$pub_priv[i] <- "Private"
  }
}

# User Interface
ui <- fluidPage(
  navbarPage("Native Woodlands of Ireland",
             tabPanel("Map",
                      textOutput("text"),
                      selectInput(inputId = "county", label = "County", choices = c("All counties", unique(df_clean2$county))),
                      radioButtons(inputId = "type", label = "Map type", choices = c("Public", "All")),
                      leafletOutput("map", width = "80%", height = 600),
                      textOutput("Description")    
                      )
                            
  )
  
)

server <- function(input, output, session) {
  
    output$map <- renderLeaflet({leaflet(df_clean2 %>% 
                                         filter(if (input$county %in% "All counties") county %in% county 
                                                else county %in% input$county) %>% 
                                            filter(if (input$type %in% c("Public")) pub_priv %in% input$type
                                                   else pub_priv %in% c("Public", "Private"))
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


