# install.packages("RMongo")
library(RMongo)
library(mongolite)
library(readr)
library(dplyr)
# library(config)
# config <- config::get(file = "config.yml")

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

# Cleaning kaggle data ----------------------------------------------------

moviesFromKaggle <- moviesFromKaggle %>%
  mutate(movie_title = gsub("Â", "", movie_title)) %>%
  mutate(movie_title = sapply(movie_title, trim))

# Creating same title format as movielens
moviesFromKaggle$title <- paste(moviesFromKaggle$movie_title, "(",moviesFromKaggle$title_year,")")
moviesFromKaggle <- moviesFromKaggle %>%
  mutate(title = gsub("\\( ", "\\(", title)) %>%
  mutate(title = gsub("\\ )", "\\)", title))


# Insert into mongoDb -----------------------------------------------------

mcon <- mongo(collection="DataKaggle", db="Practicum", url="mongodb://localhost")

mcon$insert(moviesFromKaggle)

# mcon2 <- mongo(collection="DataMovielens", db="Practicum", url="mongodb://localhost")
# 
# mcon2$insert(moviesRatingsLinks)
