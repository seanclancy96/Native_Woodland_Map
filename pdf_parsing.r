# Necessary packages 
library(tidyverse)
library(pdftools)
library(leaflet)
library(tm)

# testing leaflet maps
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

# Creating a vector of PDF files names using the list.files function. The pattern argument says to only grab those files ending with "pdf"
files <- list.files(pattern = "pdf$")
files

opinions <- lapply(files, pdf_text)
# opinions

lapply(opinions, length)

opinions[1] # first pdf 

# Set up text mining package to extract data from these pdf files now in R


