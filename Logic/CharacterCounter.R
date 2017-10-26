library(RMongo)
library(dplyr)
library(stringr)
library(tidyr)

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

getMoviesRatings <- function(){
  movies <- read.csv2(path("movies.csv"), sep = ",")
  ratings <- read.csv2(path("ratings.csv"), sep = ",")
  moviesRatings <- merge(movies, ratings, by = "movieId")
  return(moviesRatings)
}

# Cleaning kaggle data ----------------------------------------------------

cleanKaggleData <- function() {
  kaggleData <- kaggleData %>%
    mutate(movie_title = gsub("\U00C2", "", movie_title)) %>%
    mutate(movie_title = gsub("\U00C3", "", movie_title)) %>%
    mutate(movie_title = sapply(movie_title, trim)) %>%
    # mutate(movie_title = gsub("\\,$", "", movie_title))
    mutate(movie_title= gsub("\u201A", "", movie_title))
  
  # Creating same title format as movielens
  kaggleData$title <-
    paste(kaggleData$movie_title, "(", kaggleData$title_year, ")")
  kaggleData <- kaggleData %>%
    mutate(title = gsub("\\( ", "\\(", title)) %>%
    mutate(title = gsub("\\ )", "\\)", title))
  return(kaggleData)
}

# Cleaning movielens data -------------------------------------------------

cleanMovieLens <- function() {
  numberOfPeopleVoted <- getMoviesRatings() %>%
    group_by(title) %>%
    summarise(no_of_votes = n())
  
  summedUpRating <- getMoviesRatings() %>%
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
  titleFix <- subset(mergedMovieRatings, grepl(", The", title))
  titleFix$title <- gsub(titleFix$title, pattern = ", The", replacement = "")
  titleFix$title <- sub("^", "The ", titleFix$title)
  
  subMergedMovieRatings <- subset(mergedMovieRatings, !grepl(", The", title))
  
  fixedMergeMovieRatings <- merge(subMergedMovieRatings, titleFix, key = "title", all = TRUE)
  return(fixedMergeMovieRatings)
}

# Final dataframes --------------------------------------------------------

cleanedMovieLensDf <- subset(
  cleanMovieLens(),
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
  cleanKaggleData(),
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

getTotalMergedDf <- function() {
  totalDf <- merge(filteredKaggleDf, cleanedMovieLensDf, key = "title", all.x = TRUE)
  
  # Complement the missing data
  dfToComplement <- totalDf %>%
    filter(is.na(no_of_vowels)) %>%
    mutate(no_of_vowels = sapply(title, vowels_count)) %>%
    mutate(no_of_letters = sapply(title, letters_count)) %>%
    mutate(no_of_consonants = no_of_letters - no_of_vowels)
  
  orginDf <- totalDf %>%
    filter(!is.na(no_of_vowels))
  
  # Merge the complemented data into the total dataframe
  totalDf <- merge(orginDf, dfToComplement, key = "title", all = TRUE)
  return(totalDf)
}
