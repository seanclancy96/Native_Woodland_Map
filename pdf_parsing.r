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


# setting working directory
setwd("~/R/Native_Woodland_Project")
# Creating a vector of PDF files names using the list.files function. The pattern argument says to only grab those files ending with "pdf"
files <- list.files( pattern = "pdf$")
files

# Extract text from pdfs
pdfs_R <- lapply(files, pdf_text)

lapply(pdfs_R, length)

pdfs_R[1] # first pdf 

?pdftools

pdfs_R[1] %>% readLines()

# Cleaning a text document 
gsub(pattern = "\\W", replacement=" ", pdfs_R[1]) # removing punctuation

word_count <- count(pdfs_R[[1]], word, sort = TRUE)
typeof(pdfs_R)
typeof(pdfs_R[[1]])
?count

length(pdfs_R)
write(pdfs_R[1],"test.txt")

length(pdfs_R[[1]])

text_test <- pdf_text("Perrin_et_al_2008_NSNW_V3a.pdf")
typeof(text_test)
# count(text_test, "Townland name", sort = T)
