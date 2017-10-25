install.packages("RMongo")
library(RMongo)
library(mongolite)

getwd()
setwd ("D:/Vakken/Data Storage and Processing")
moviesFromKaggle <- read.csv2("movie_metadata.csv", sep=",")
head(moviesFromKaggle)


movies <- read.csv2("movies.csv", sep=",")
ratings <- read.csv2("ratings.csv", sep=",")
links <- read.csv2("links.csv", sep=",")
tags <- read.csv2("tags.csv", sep=",")

moviesRatings <- merge(movies, ratings, by="movieId")
head(moviesRatings)

moviesRatingsLinks <- merge(moviesRatings, links, by="movieId")
head(moviesRatingsLinks)


mcon <- mongo(collection="Data", db="Practicum", url="mongodb://localhost")

mcon$insert(moviesFromKaggle)

mcon2 <- mongo(collection="DataMovielens", db="Practicum", url="mongodb://localhost")

mcon2$insert(moviesRatingsLinks)
