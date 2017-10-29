library(ggplot2)
library(readr)
library(dplyr)

source("Logic/CharacterCounter.R")
df <- getTotalMergedDf()


# General functions -------------------------------------------------------

minusOne <- function(x) {
  x <- x - 1
}

# # var1 = no_of_letters, var2 = occurance_total_letters
# fillDfToSameSize <- function(size ,var1, var2, df1, df2, mergedBy) {
#   
#   fillZeroDf <- data.frame(var1 = integer(),
#                            average_score = numeric(),
#                            var2 = integer())
#   
#   temp <- merge(df1, df2, by = mergedBy)
#   
#   size <- size + 1
#   
#   for (i in 1:size) {
#     fillZeroDf[i,1] <- i
#     fillZeroDf[i,2] <- 0
#     fillZeroDf[i,3] <- 0
#   }
#   
#   fillZeroDf <- fillZeroDf %>%
#     mutate(noquote(var1) = sapply(noquote(var1), minusOne))
#   
#   totaldf <- merge(temp, fillZeroDf, by = var1, all.y = TRUE)
#   totaldf <- totaldf[,c(var1, "average_score.x", paste0(var2, ".x"))]
#   colnames(totaldf) <- c(var1, "average_score", var2)
#   totaldf[is.na(totaldf)] <- 0
# }

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

getOccurancesVowels <- function(){
  summary_vowels_df <- getVowels()
  
  summary_occurance_vowels_df <- df %>%
    group_by(no_of_vowels) %>%
    summarise(occurance_vowels = n())
  
  size <- tail(summary_occurance_vowels_df$no_of_vowels, n = 1)
  
  temp <- merge(summary_vowels_df, summary_occurance_vowels_df, by = "no_of_vowels")
  
  fillZeroDf <- data.frame(no_of_vowels = integer(),
                           average_score = numeric(),
                           occurance_vowels = integer())
  
  size <- size + 1
  
  for (i in 1:size) {
    fillZeroDf[i,1] <- i
    fillZeroDf[i,2] <- 0
    fillZeroDf[i,3] <- 0
  }
  
  fillZeroDf <- fillZeroDf %>%
    mutate(no_of_vowels = sapply(no_of_vowels, minusOne))
  
  totaldf <- merge(temp, fillZeroDf, by = "no_of_vowels", all.y = TRUE)
  totaldf <- totaldf[,c("no_of_vowels","average_score.x", "occurance_vowels.x")]
  colnames(totaldf) <- c("no_of_vowels", "average_score", "occurance_vowels")
  totaldf[is.na(totaldf)] <- 0
  
  return(totaldf)
}

getOccurancesConsonants <- function(){
  summary_consonants_df <- getConsonants()
  
  summary_occurance_consonants_df <- df %>%
    group_by(no_of_consonants) %>%
    summarise(occurance_consonants = n())
  
  size <- tail(summary_consonants_df$no_of_consonants, n = 1)
  
  temp <- merge(summary_consonants_df, summary_occurance_consonants_df, by = "no_of_consonants")
  
  fillZeroDf <- data.frame(no_of_consonants = integer(),
                           average_score = numeric(),
                           occurance_consonants = integer())
  
  size <- size + 1
  
  for (i in 1:size) {
    fillZeroDf[i,1] <- i
    fillZeroDf[i,2] <- 0
    fillZeroDf[i,3] <- 0
  }
  
  fillZeroDf <- fillZeroDf %>%
    mutate(no_of_consonants = sapply(no_of_consonants, minusOne))
  
  totaldf <- merge(temp, fillZeroDf, by = "no_of_consonants", all.y = TRUE)
  totaldf <- totaldf[,c("no_of_consonants","average_score.x", "occurance_consonants.x")]
  colnames(totaldf) <- c("no_of_consonants", "average_score", "occurance_consonants")
  totaldf[is.na(totaldf)] <- 0
  
  return(totaldf)
}

getTotalLetters <- function(){
  summary_total_letters <- getLetters()
  
  summary_total_letters_df <- df %>%
    group_by(no_of_letters) %>%
    summarise(occurance_total_letters = n())
  
  size <- tail(summary_total_letters$no_of_letters, n = 1)
  
  temp <- merge(summary_total_letters, summary_total_letters_df, by = "no_of_letters")
  
  fillZeroDf <- data.frame(no_of_letters = integer(),
                           average_score = numeric(),
                           occurance_total_letters = integer())
  
  size <- size + 1
  
  for (i in 1:size) {
    fillZeroDf[i,1] <- i
    fillZeroDf[i,2] <- 0
    fillZeroDf[i,3] <- 0
  }
  
  fillZeroDf <- fillZeroDf %>%
    mutate(no_of_letters = sapply(no_of_letters, minusOne))
  
  totaldf <- merge(temp, fillZeroDf, by = "no_of_letters", all.y = TRUE)
  totaldf <- totaldf[,c("no_of_letters","average_score.x", "occurance_total_letters.x")]
  colnames(totaldf) <- c("no_of_letters", "average_score", "occurance_total_letters")
  totaldf[is.na(totaldf)] <- 0
  
  return(totaldf)
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
