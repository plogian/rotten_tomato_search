# setwd("~/R/Rotten Tomato")
library(XML)

file_name <- "Best Scifi Movies.csv"
error_file <- "error.txt"

df <- read.csv(file_name, stringsAsFactors = FALSE)
movie_list <- unlist(df$title)
movie_list <- sapply(movie_list, 
                     function(x) {
                       x <- gsub("&", "and", x)
                       x <- gsub("[[:punct:]]", "", x)
                       x <- gsub(" ", "_", x)
                       x <- tolower(x)
                       return(x)
                       })

tomatometer <- function(movie) {
  url <- paste0("https://www.rottentomatoes.com/m/", movie)
  tryCatch ({
    html <- readLines(url)
    meter_location <- grep("meter-value", html)[1]
    score <- html[meter_location]
    score <- gsub("[^0-9]", "", score)
    return(score)
  }, error = function (e) {
    cat(movie, file=error_file, sep="\n", append=TRUE)
    return("failed")
  })
} 

year <- function(movie) {
  url <- paste0("https://www.rottentomatoes.com/m/", movie)
  tryCatch ({
    html <- readLines(url)
    meter_location <- grep('class="h3 year"', html)[1]
    score <- html[meter_location]
    score <- gsub("[^0-9]", "", score)
    score <- gsub("^3", "", score)
    score <- gsub("1$", "", score)
    return(score)
  }, error = function (e) {
    cat(movie, file=error_file, sep="\n", append=TRUE)
    return("failed")
  })
} 

df$Tomatometer <- sapply(movie_list, tomatometer)
df$tomto_url <- sapply(movie_list, function(x) {paste0("https://www.rottentomatoes.com/m/", x)})
df$year <- sapply(movie_list, year)
  
write.csv(df, paste0("updated-", file_name))