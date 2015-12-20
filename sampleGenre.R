setwd("C:/Users/Magdalena/Desktop/MachineLearning")


# dobrze to widać przy restauracjach polska a niemiecja a tajska
u_cols <- c('user_id', 'age', 'sex', 'occupation', 'zip_code')
users <- read.csv('u.user', sep='|', col.names=u_cols, header=FALSE)


r_cols <- c('user_id', 'movie_id', 'rating', 'unix_timestamp')
ratings <- read.csv('u.data', sep='\t', col.names=r_cols,header = FALSE)


m_colsR <- c('movie_id', 'title', 'release_date', 'video_release_date', 'imdb_url', 'unknown','Action','Adventure','Animation','Children\'s','Comedy','Crime','Documentary','Drama','Fantasy','Film-Noir','Horror','Musical','Mystery','Romance','Sci-Fi','Thriller','War','Western')
moviesR <- read.csv('u.item', sep='|',header=FALSE, col.names=m_colsR )

m_cols <- c('movie_id', 'title', 'release_date')
movies <- read.csv('u.item', sep='|',header=FALSE )

colnames(movies) <- m_cols
mlens <- merge(movies, ratings, all = TRUE)


movieGenres <- merge(moviesR, ratings, all = TRUE)
# średnia ocen filmów
movieAdvRatings <- aggregate( movieGenres$rating, list(title = movieGenres$title),  mean)

movieAdvGenres <- merge(moviesR,movieAdvRatings,all =TRUE)
movieAdvGenres
mlens[1] <- NULL
mlens[2] <- NULL
mlens[4] <- NULL

mlens <- mlens[c("title", "user_id", "rating")]

library('reshape')
user.title.matrix.NA <- cast(mlens, title ~ user_id, value = 'rating', fun.aggregate=mean)

# najczesciej ocenione filmy
mostRatingUsers <- 0
for(j in 2:ncol(user.title.matrix.NA)){
  mostRatingUsers[j-1] <- sum(!is.na(user.title.matrix.NA[j]))
}

mostRatingUsers <-(order(mostRatingUsers,decreasing = TRUE))[1:20]
mostRatingUsers
colnames(user.title.matrix.NA[406])

mostRatedMovies <- 0
for(i in 1:nrow(user.title.matrix.NA)){
  mostRatedMovies[i] <- sum(!is.na(user.title.matrix.NA[i,2:ncol(user.title.matrix.NA)]))
}
sort(mostRatedMovies,decreasing = TRUE)[1:100]
order(mostRatedMovies,decreasing = TRUE)[1:100]

mostRatedMovies <- order(mostRatedMovies,decreasing = TRUE)[1:100]
mostRated <- 0
mostRated <- user.title.matrix.NA[mostRatedMovies,mostRatingUsers+1]

bestRatings405 <- row.names(mostRated[mostRated$"405" == "5",colnames(mostRated)])

bestGernres405 <- moviesR[bestRatings405,6:24]
colSums(bestGernres405)
usersLikes <- c(1,3,4)

meanRated <- cbind(movie_id=c(rownames(mostRated)),mean=c(rowMeans(mostRated,na.rm = TRUE)))


rated

nrow(mostRated)
rated <- subset(movieAdvGenres, title %in% user.title.matrix.NA[rownames(mostRated),1])


newdata <- 0
for(j in 1:nrow(mostRated)){
  newdata[j] <- movieAdvGenres[ which(movieAdvGenres$movie_id==row.names(mostRated[j,])),]
}
newdata
movieAdvGenres[3,2] 


colSums(rated[6:24]) #

chosenGenres <- c(2,3,6,9,15,16,17) 
# Action Adventure Comedy Drama Romance Sci.Fi Thriller
genre <- rated[6:24]
genre <- genre[chosenGenres]
correlation <- cor(genre)



R_p <- function(id_movie,usersLikes, correlation, rated,genre){
  bsum <- 0
  for(i in 1:length(usersLikes)) {
    for(j in 1:ncol(genre)) {
      if(genre[id_movie,j] == 1){
        sum <- correlation[usersLikes[i],j] *rated[id_movie,25]
        bsum <- sum + bsum
      }
    }
  }
  return (bsum / length(usersLikes))
}
R_pAll <- 0
for(i in 1:nrow(rated)){
  R_pAll[i] <- R_p(i,usersLikes, correlation, rated,genre)
}


R_pAllOrder <- order(R_pAll,decreasing = TRUE)

rated[R_pAllOrder[1:10],1]



