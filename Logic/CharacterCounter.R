library(RMongo)
mcon <- mongoDbConnect("Practicum",port=27017)
moviesCollection <- "Data"
kaggleData<- dbGetQuery(mcon, moviesCollection,"{}",skip=0,limit=100000)
head(kaggleData)

kaggleData$movie_title <- apply(kaggleData,2,function(x) gsub("Ã‚Â","",kaggleData$movie_title))

movielensCollection <- "DataMovielens"
movielensData <- dbGetQuery(mcon, movielensCollection,"{}",skip=0,limit=100000)
head(movielensData)