library(RMongo)
library(dplyr)
library(stringr)
library(tidyr)
#library(stringi)

# Genersl functions and variables -----------------------------------------

path <- function(input) {
  filePath <- "D:/Vakken/Data Storage and Processing/"
  return(paste(filePath, input, sep = ""))
}

# trims the whitespaces
trim <- function(string) {
  gsub("^\\s+|\\s+$", "", string)
}

# splits the year
extractYear <- function(string) {
  substr(string, nchar(string) - 6 + 1, nchar(string))
}

vowels <- c("a", "e", "i", "o", "u")

vowels_count <- function(string) {
  sum(str_count(tolower(string), vowels))
}

letters_count <- function(string) {
  sum(str_count(tolower(string), "[:alpha:]"))
}

# Retrieve data from mongoDb ----------------------------------------------

mcon <- mongoDbConnect("Practicum", port = 27017)
moviesCollection <- "DataKaggle"
kaggleData <-
  dbGetQuery(mcon,
             moviesCollection,
             "{}",
             skip = 0,
             limit = 100000)

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
    round(x, digits = 1)
  }))

mergedMovieRatings <-
  mergedMovieRatings[!(mergedMovieRatings$avg_rating == 0), ]

mergedMovieRatings <- mergedMovieRatings %>%
  mutate(no_of_vowels = sapply(title, vowels_count))

mergedMovieRatings <- mergedMovieRatings %>%
  mutate(no_of_letters = sapply(title, letters_count)) %>%
  mutate(no_of_consonants = no_of_letters - no_of_vowels) %>%
  mutate(title_year = sapply(as.character(title), extractYear))

# Retrieves all the abnormal rows
rowsToFix <- subset(mergedMovieRatings,!grepl("^\\(", title_year))

# Filter rows without any digits
rowsToFix <- subset(rowsToFix, grepl("^\\d", title_year))

# Specific fixes
rowsToFix$title_year <- sub("^", "\\(", rowsToFix$title_year)
rowsToFix$title_year <-
  gsub(rowsToFix$title_year, pattern = "-", replacement = "")

# Remove abnormal rows from df
mergedMovieRatings <-
  subset(mergedMovieRatings, grepl("^\\(", title_year))

# Merge fixedRows with df with a full join
mergedMovieRatings <- merge(mergedMovieRatings, rowsToFix, key = "title", all = TRUE)
mergedMovieRatings$title_year <- gsub(mergedMovieRatings$title_year, pattern = "\\(", replacement = "")
mergedMovieRatings$title_year <- gsub(mergedMovieRatings$title_year, pattern = "\\)", replacement = "")

# Final dataframes --------------------------------------------------------

cleanedMovieLensDf <- subset(
  mergedMovieRatings,
  select =
    c(
      title,
      title_year,
      no_of_votes,
      avg_rating,
      no_of_vowels,
      no_of_letters,
      no_of_consonants
    )
)

filteredKaggleDf<- subset(
  kaggleData,
  select =
    c(
      title,
      title_year,
      imdb_score,
      content_rating,
      budget,
      gross
    )
)

# Combining movielens data with kaggle data -------------------------------

totalDf <- merge(filteredKaggleDf, cleanedMovieLensDf, key = "title", all.x = TRUE)

dfToComplement <- totalDf %>%
  filter(is.na(no_of_vowels)) %>%
  mutate(no_of_vowels = sapply(title, vowels_count)) %>%
  mutate(no_of_letters = sapply(title, letters_count)) %>%
  mutate(no_of_consonants = no_of_letters - no_of_vowels)
