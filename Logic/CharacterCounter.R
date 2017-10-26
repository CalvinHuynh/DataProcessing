library(RMongo)
library(dplyr)
library(stringr)
#library(stringi)

# Genersl functions and variables -----------------------------------------

path <- function(input) {
  filePath <- "D:/Vakken/Data Storage and Processing/"
  return(paste(filePath, input, sep = ""))
}

trim <- function(string) {
  gsub("^\\s+|\\s+$", "", string)
}

vowels <- c("a","e","i","o","u")

vowels_count <- function(string) {
  sum(str_count(tolower(string), vowels))
}

letters_count <- function(string) {
  sum(str_count(tolower(string), "[:alpha:]"))
}

# Retrieve data from mongoDb ----------------------------------------------

mcon <- mongoDbConnect("Practicum",port=27017)
moviesCollection <- "DataKaggle"
kaggleData<- dbGetQuery(mcon, moviesCollection,"{}",skip=0,limit=100000)

# Reading local data from movielens ---------------------------------------

movies <- read.csv2(path("movies.csv"), sep = ",")
ratings <- read.csv2(path("ratings.csv"), sep = ",")

# Cleaning movielens data -------------------------------------------------

moviesRatings <- merge(movies, ratings, by = "movieId")

numberOfPeopleVoted <- moviesRatings %>%
  group_by(title) %>%
  summarise(no_of_votes = n())

summedUpRating <- moviesRatings %>%
  group_by(title) %>%
  summarise(total_rating = sum(as.numeric(as.character(rating))) * 2)

mergedMovieRatings <- merge(numberOfPeopleVoted, summedUpRating)
mergedMovieRatings <- mergedMovieRatings %>%
  mutate(avg_rating = sapply(total_rating / no_of_votes, function(x) {
    round(x, digits = 2)
  }))

mergedMovieRatings <- mergedMovieRatings[!(mergedMovieRatings$avg_rating == 0),]

mergedMovieRatings <- mergedMovieRatings %>%
  mutate(no_of_vowels = sapply(title, vowels_count))

mergedMovieRatings <- mergedMovieRatings %>%
  mutate(no_of_letters = sapply(title, letters_count)) %>%
  mutate(no_of_consonants = no_of_letters - no_of_vowels)

cleanedMovieLensDf <- subset(mergedMovieRatings,
                       select = 
                         c(title, no_of_votes,
                           avg_rating, no_of_vowels,
                           no_of_letters, no_of_consonants))
