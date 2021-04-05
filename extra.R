library(tidyverse)
library(leaflet)
library(htmltools)

# Reading data 
df <- readRDS(file = "df.rds")

# Setting St. Johns Wood Latitude and Longitude to correct coordinates
johns_name <- "St John's Wood"
lat_johns <- 53.556947
lon_johns <- -8.00471653

# Changing details for some specific woods (St. John's wood, Phoenix park wood and more)
johns <- df %>% 
  filter(woodland_name %in% johns_name) %>% 
  mutate(lat = lat_johns, lon = lon_johns) 

borough <- df %>% 
  filter(county %in% "Borough") %>% 
  mutate(county = "Dublin")

meath_louth <- df %>% 
  filter(county %in% "Meath/Louth") %>% 
  mutate(county = "Meath")

cork_waterford <- df %>% 
  filter(county %in% "Cork/Waterford") %>% 
  mutate(county = "Cork")

df_clean <- df %>% 
  filter(!woodland_name %in% johns_name) %>% 
  filter(!county %in% "Borough") %>% 
  filter(!county %in% "Meath/Louth") %>% 
  filter(!county %in% "Cork/Waterford") %>% 
  add_row(johns)  %>% 
  add_row(borough) %>% 
  add_row(meath_louth) %>% 
  add_row(cork_waterford)

# Checking new data frame contains wood of interest
df_clean %>% 
  filter(woodland_name %in% johns_name | woodland_name %in% "Phoenix Park") 

# Changing empty ownership names to Not Stated
df_clean$ownership[df_clean$ownership==""] <- "Not Stated"

# Setting ownership variable for Private/Public/NPWS
df_clean$ownership0 <- ifelse(grepl("private", df_clean$ownership, ignore.case=T), "Private",
                              ifelse(grepl("coillte|other state body|local authority|Bórd na Móna", df_clean$ownership,ignore.case=T), "Public",
                                     ifelse(grepl("npws", df_clean$ownership, ignore.case=T), "NPWS", "Unknown")))

saveRDS(df_clean,"df_clean.rds")

# User Interface
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map",width="100%", height ="100%"),
  
  
  absolutePanel(top = 10, right = 10,
                radioButtons("owner", label = "Ownership", choices = c("All", "Public", "NPWS"), width = '180px'),
                selectInput("county", label = "County", choices = c("All Counties", "Munster", "Leinster", "Connacht", "Ulster",
                                                                    df_clean$county %>% 
                                                                      unique() %>% # Taking unique county vals
                                                                      sort() # Sorting counties alphabetically to improve navigation
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

