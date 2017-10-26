library(ggplot2)
library(readr)
library(dplyr)

source("Logic/CharacterCounter.R")
df <- getTotalMergedDf()

# Table functions ---------------------------------------------------------

getData <- function(){
  return(df)
}

getVowels <- function() {
  df %>%
    group_by(no_of_vowels) %>%
    summarise(average_score = round(mean(imdb_score), digits = 1))
}

getConsonants <- function() {
  df %>%
    group_by(no_of_consonants) %>%
    summarise(average_score = round(mean(imdb_score), digits = 1))
}

getLetters <- function() {
  df %>%
    group_by(no_of_letters) %>%
    summarise(average_score = round(mean(imdb_score), digits = 1))
}

# Plot functions ----------------------------------------------------------

barPlotVowels<- function() {
  p <- ggplot(getVowels(), aes(x = factor(no_of_vowels), y = average_score)) + 
    geom_bar(stat = "identity") +
    labs(x = "number of vowels", y = "imdb score") +
    geom_text(data = getVowels(), aes(label = average_score), position = "stack", hjust = -0.5) +
    ggtitle("Average score per number of used vowels in title") +
    coord_flip() +
    theme_light()
  return(p)
}

barPlotConsonants<- function() {
  p <- ggplot(getConsonants(), aes(x = factor(no_of_consonants), y = average_score)) + 
    geom_bar(stat = "identity") +
    labs(x = "number of consonants", y = "imdb score") +
    geom_text(data = getConsonants(), aes(label = average_score), position = "stack", hjust = -0.5) +
    ggtitle("Average score per number of used consonants in title") +
    coord_flip() +
    theme_light()
  return(p)
}


barPlotLetters<- function() {
  p <- ggplot(getLetters(), aes(x = factor(no_of_letters), y = average_score)) + 
    geom_bar(stat = "identity") +
    labs(x = "number of consonants", y = "imdb score") +
    geom_text(data = getLetters(), aes(label = average_score), position = "stack", hjust = -0.5) +
    ggtitle("Average score per title length") +
    coord_flip() +
    theme_light()
  return(p)
}

barPlotVowels()
barPlotConsonants()
barPlotLetters()
