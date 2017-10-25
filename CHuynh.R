install.packages("dplyr")
library(dplyr)

getwd()
setwd ( "D:/Downloads")
randomFile <- read.csv2("File-A_BD3.csv", sep=";")

filteredData <- select(randomFile,1:8)
colnames(filteredData)

names(filteredData) <-c("CRD_ID","StartDatetime","EndDatetime","Duration"
                        ,"Volume","ChargePointStreet","ChargePointZIP","ChargePointCity")
colnames(filteredData)



removeSpaces <- function(text){
  for(i in filteredData$ChargePointZIP)
    if(i == text){
      newValue <- gsub(" ", "",  filteredData$ChargePointZIP[i])
      filteredData$ChargePointZIP[i] <- newValue
    } 
  }
?apply
apply(filteredData,ChargePointZIP,removeSpaces("3086 PR"))


