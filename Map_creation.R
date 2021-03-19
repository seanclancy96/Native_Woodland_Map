# Reading data 
readRDS(file = "df.rds")

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
levels(df_clean$ownership)[levels(df_clean$ownership)==""] <- "Not Stated"

# Taking non-private woods only
df_clean_non_private <- df_clean %>% 
  filter(!grepl("Private", ownership)) 

# Taking definite non-private woods
df_clean_public_confirmed <- df_clean_non_private %>% 
  filter(!ownership %in% "Not Stated")

# simple plot
ggplot(df_clean) +
  geom_point(aes(area, cons_score, shape = cons_rate, color = threat_score)) +
  facet_wrap(~threat_rate) +
  scale_color_gradientn(colors = terrain.colors(7)) +
  scale_x_log10()

# Map of all woods
m <- leaflet(df_clean) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
             popup = ~paste0(woodland_name, 
                             "<br/>Conservation status: ", cons_rate, 
                             "<br/>Threat status: ", threat_rate, 
                             "<br/>Area (ha): ", area, 
                             "<br/>Ownership: ", ownership))

# Map of non private woods
m_nonpriv <- leaflet(df_clean_non_private) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
             popup = ~paste0(woodland_name, 
                             "<br/>Conservation status: ", cons_rate, 
                             "<br/>Threat status: ", threat_rate, 
                             "<br/>Area (ha): ", area, 
                             "<br/>Ownership: ", ownership ))

# Map of all non-private woods excluding not stated ownership
m_public_confirmed <- leaflet(df_clean_public_confirmed) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
             popup = ~paste0(woodland_name, 
                             "<br/>Conservation status: ", cons_rate, 
                             "<br/>Threat status: ", threat_rate, 
                             "<br/>Area (ha): ", area, 
                             "<br/>Ownership: ", ownership ))

m  # Print the full map
m_nonpriv  # Print the non-private woods map
m_public_confirmed  # Print the confirmed public woods map

