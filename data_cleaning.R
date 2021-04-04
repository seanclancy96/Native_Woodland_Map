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
