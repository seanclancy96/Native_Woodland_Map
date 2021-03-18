# Necessary packages 
library(tidyverse)
library(pdftools)
library(stringi)
library(rnrfa)
library(leaflet)
library(htmltools)

# Extract directly from URLS
urls <- c("https://www.npws.ie/sites/default/files/publications/pdf/Perrin_et_al_2008_NSNW_V3a.pdf",
          "https://www.npws.ie/sites/default/files/publications/pdf/Perrin_et_al_2008_NSNW_V3b.pdf",
          "https://www.npws.ie/sites/default/files/publications/pdf/Perrin_et_al_2008_NSNW_V3c.pdf",
          "https://www.npws.ie/sites/default/files/publications/pdf/Perrin_et_al_2008_NSNW_V3d.pdf")

txt <- lapply(urls, pdf_text) # Changing pdf to text files
pages <- c() # Initialising pages vector

# Fill vector with each pdf's page length 
i<-1 
while (i < 5){
  pages[i] <- length(txt[[i]])
  i <- i+1
}

pages # Checking vector

txt[[1]]

# Extracting information of interest
ls_out <- function(pdf_id){
  lapply(2:pages[pdf_id], function(x) {
    
    # x <- 2 # Use this to test
    plines <- strsplit(txt[[pdf_id]][x], "\\n")[[1]]
    
    # Extract site id
    site_line <- grep("Site no.", plines)
    site_id <- stri_extract_first_regex(plines[site_line], "[0-9]+")
    
    # Extract woodland name
    wl_line <- grep("Woodland name", plines)
    wl <- str_squish(plines[wl_line])
    woodland_name <- gsub('^.*Woodland name\\s*|\\s*Townland.*$', '', wl)
    
    # Extract townland name
    twn_line <- grep("Townland name", plines)
    twn <- str_squish(plines[twn_line])
    townland_name <- gsub('^.*Townland name\\s*|\\s*Conservation.*$', '', twn)
    
    # Extract whether it is a public or private wood
    own_line <- grep("Ownership", plines)
    own <- str_squish(plines[own_line])
    ownership <- gsub('^.*Ownership\\s*|\\s*Area.*$', '', own)
    
    # Extract Area
    area_line <- grep("Area", plines)
    area <- as.numeric(stri_extract_first_regex(plines[area_line], "[0-9]+"))
    
    # conservation & threat
    cons_line <- grep("Conservation", plines)
    cl <- str_squish(plines[cons_line])
    scores <- as.numeric(stri_extract_all(cl, regex = "[0-9]+")[[1]])
    cons_rate <- gsub('^.*Conservation rating and score\\s*|\\s*[0-9]+.*$', '', cl)
    cons_score <- scores[1]
    threat_score <- scores[2]
    threat_rate <- gsub('^.*Threat rating and score\\s*|\\s*[0-9]+.*$', '', cl)
    
    # Get grid ref & convert to lat-long
    grid_line <- grep("Grid ref.", plines)
    gl <- str_squish(plines[grid_line])
    gref <- gsub('^.*Grid ref. \\s*|\\s*6 inch sheet.*$', '', gl)
    coord <- rnrfa::osg_parse(grid_refs = paste0("I", gref), coord_system = "WGS")
    lat <- coord$lat
    lon <- coord$lon
    
    # Format to datafram
    df <- data.frame(site_id, lat = lat, lon = lon, woodland_name, area, cons_rate, cons_score, threat_rate, threat_score, ownership, townland_name)
    
    print(x)
    
    return(df[1, ])
    
  })
}

# Applying function to create data frames with variables of interest
df1 <- do.call(rbind, ls_out(1))
df2 <- do.call(rbind, ls_out(2))
df3 <- do.call(rbind, ls_out(3))
df4 <- do.call(rbind, ls_out(4))

df <- rbind(df1,df2,df3,df4) # Binding these data together
df
str(df)

# Changing empty ownership names to Not Stated
levels(df$ownership)[levels(df$ownership)==""] <- "Not Stated"

# Taking non-private woods only
df_non_private <- df %>% 
  filter(!grepl("Private", ownership)) 

# Taking definite non-private woods
df_public_confirmed <- df_non_private %>% 
  filter(!ownership %in% "Not Stated")

# simple plot
ggplot(df) +
  geom_point(aes(area, cons_score, shape = cons_rate, color = threat_score)) +
  facet_wrap(~threat_rate) +
  scale_color_gradientn(colors = terrain.colors(7)) +
  scale_x_log10()

# Map of all woods
m <- leaflet(df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
             popup = ~paste0(woodland_name, 
                             "<br/>Conservation status: ", cons_rate, 
                             "<br/>Threat status: ", threat_rate, 
                             "<br/>Area (ha): ", area, 
                             "<br/>Ownership: ", ownership))
m  # Print the map

# Map of non private woods
m_nonpriv <- leaflet(df_non_private) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
             popup = ~paste0(woodland_name, 
                             "<br/>Conservation status: ", cons_rate, 
                             "<br/>Threat status: ", threat_rate, 
                             "<br/>Area (ha): ", area, 
                             "<br/>Ownership: ", ownership ))
m_nonpriv  # Print the map

# Map of all non-private woods excluding not stated ownership
m_public_confirmed <- leaflet(df_public_confirmed) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lon, ~lat, label = ~htmlEscape(woodland_name), 
             popup = ~paste0(woodland_name, 
                             "<br/>Conservation status: ", cons_rate, 
                             "<br/>Threat status: ", threat_rate, 
                             "<br/>Area (ha): ", area, 
                             "<br/>Ownership: ", ownership ))
m_public_confirmed  # Print the map

