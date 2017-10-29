library(RMongo)
library(mongolite)
library(readr)
library(dplyr)

# General functions -------------------------------------------------------

path <- function(input) {
  filePath <- "D:/Vakken/Data Storage and Processing/"
  return(paste(filePath, input, sep = ""))
}

trim <- function(string) {
  gsub("^\\s+|\\s+$", "", string)
}

# Importing data ----------------------------------------------------------

moviesFromKaggle <- read.csv2(path("movie_metadata.csv"), sep = ",")

# Insert into mongoDb -----------------------------------------------------

mcon <- mongo(collection="DataKaggle", db="Practicum", url="mongodb://localhost")

mcon$insert(moviesFromKaggle)

# mcon2 <- mongo(collection="DataMovielens", db="Practicum", url="mongodb://localhost")
# 
# mcon2$insert(moviesRatingsLinks)
