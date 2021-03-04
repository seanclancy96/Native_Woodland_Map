# Necessary packages 
library(tidyverse)
library(pdftools)
library(stringi)
library(rnrfa)
library(leaflet)

# Creating a vector of PDF files names using the list.files function. The pattern argument says to only grab those files ending with "pdf"
# Extract direct from URLS
urls <- c("https://www.npws.ie/sites/default/files/publications/pdf/Perrin_et_al_2008_NSNW_V3a.pdf")

opinions <- lapply(urls, pdf_text)
pages <- length(opinions[[1]])

# loop through pages & exrtact info
ls_out <- lapply(2:pages, function(x) {
  
  # x <- 2 # Use this to test
  
  plines <- strsplit(opinions[[1]][x], "\\n")[[1]]
  
  # Extract site id
  site_line <- grep("Site no.", plines)
  site_id <- stri_extract_first_regex(plines[site_line], "[0-9]+")
  
  # Extract woodland name
  wl_line <- grep("Woodland name", plines)
  wl <- str_squish(plines[wl_line])
  woodland_name <- gsub('^.*Woodland name\\s*|\\s*Townland.*$', '', wl)
  
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
  df <- data.frame(site_id, lat = lat, lon = lon, woodland_name, area, cons_rate, cons_score, threat_rate, threat_score)
  
  print(x)
  
  return(df[1, ])
  
})

# Bind all elements in the list by rows to make a dataframe
df <- do.call(rbind, ls_out)
df
str(df)

# simple plot
ggplot(df) +
  geom_point(aes(area, cons_score, shape = cons_rate, color = threat_score)) +
  facet_wrap(~threat_rate) +
  scale_color_gradientn(colors = terrain.colors(7)) +
  scale_x_log10()

# testing leaflet maps
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=df$lon, lat=df$lat)
m  # Print the map

