# open files
moviesCSV <- "movies.csv"
ratingsCSV <- "ratings.csv"

# work
moviesCSV <- "C:/Users/figinma1/Documents/Studies/00_Active_Projects/MoviesPrediction/movies.csv"
ratingsCSV <- "C:/Users/figinma1/Documents/Studies/00_Active_Projects/MoviesPrediction/ratings.csv"


movies <- read.delim(moviesCSV, sep = ",", fill = TRUE)
ratingsfull <- read.delim(ratingsCSV, sep = ",", fill = TRUE)
ratingsfull <- subset(ratingsfull, select = c(userId, movieId, rating))

# load libraries
library(dplyr)

# convert some columns
movies <- mutate(movies, title = as.character(title))
movies <- mutate(movies, title = as.character(title))

# remove non UTF8 characters
Encoding(movies$title) <- "UTF-8"
movies$title <- iconv(movies$title, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''

# 270.896 users with 26.024.289 is too much! take 5.000 users (784k ratings, 21k movies)
set.seed(26587)
n <- sample(unique(ratingsfull$userId),5000)
ratings <- ratingsfull[ratingsfull$userId %in% n, ]
# now not all movies have a rating, select only movies with at least 1 rating
movies <- summarize(group_by(inner_join(movies,ratings, by="movieId"), movieId, title, genres)) 
  
# order alphabetically
movies <- arrange(movies,title)


# save dataframes
save(movies,file="C:/Users/figinma1/Documents/Studies/00_Active_Projects/MoviesPrediction/app/movies.Rdata")
save(ratings,file="C:/Users/figinma1/Documents/Studies/00_Active_Projects/MoviesPrediction/app/ratings.Rdata")

# genres
genres <- strsplit(as.character(movies$genres),"|",fixed=TRUE)  # list
genres <- unlist(genres)   # character
genres <- unique(genres)   # remove duplicates
genres <- genres[!genres %in% c("(no genres listed)","IMAX")]   # remove NULL value
genres <- sort(genres)
save(genres,file="C:/Users/figinma1/Documents/Studies/00_Active_Projects/MoviesPrediction/app/genres.Rdata")


