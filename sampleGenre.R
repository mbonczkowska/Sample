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
mlensID <- mlens

mlensID[2] <- NULL
mlensID[3] <- NULL
mlensID[4] <- NULL
mlensID <- mlens[c("movie_id", "user_id", "rating")]
# średnia ocen filmów
#movieAdvRatings <- aggregate( movieGenres$rating, list(title = movieGenres$title),  mean)




mlens[1] <- NULL
mlens[2] <- NULL
mlens[4] <- NULL

mlens <- mlens[c("title", "user_id", "rating")]

library('reshape')
user.movie_id.matrix.NA <- cast(mlensID, movie_id ~ user_id, value = 'rating', fun.aggregate=mean)
# najczesciej ocenione filmy
mostRatingUsers <- 0
for(j in 2:ncol(user.movie_id.matrix.NA)){
  mostRatingUsers[j-1] <- sum(!is.na(user.movie_id.matrix.NA[j]))
}

mostRatingUsers <-(order(mostRatingUsers,decreasing = TRUE))[1:20]


movieGenres <- merge(moviesR, ratings, all = TRUE)
movieGenres <- subset(movieGenres, user_id %in% mostRatingUsers)


# srednia najczesciej oceniajacych uzytkownikow
movieAdvRatings <- aggregate( movieGenres$rating, list(title = movieGenres$title),  mean)

movieAdvGenres <- merge(moviesR,movieAdvRatings,all =TRUE)

m <- subset(movieGenres, title=="Star Wars (1977)")
sum(m[26])/nrow(m)


mostRatedMovies <- 0
for(i in 1:nrow(user.movie_id.matrix.NA)){
  mostRatedMovies[i] <- sum(!is.na(user.movie_id.matrix.NA[i,2:ncol(user.movie_id.matrix.NA)]))
}
sort(mostRatedMovies,decreasing = TRUE)[1:100]
order(mostRatedMovies,decreasing = TRUE)[1:100]




mostRatedMovies <- order(mostRatedMovies,decreasing = TRUE)[1:100]

mostRated <- 0
mostRated <- user.movie_id.matrix.NA[mostRatedMovies,mostRatingUsers+1]
rownames(!is.na(mostRated["92"]))

most
bestRatings92 <- row.names(mostRated[mostRated$"92" == "5",colnames(mostRated)])

bestGernres92 <- moviesR[bestRatings92,6:24]
mostRatedMovies

colSums(bestGernres92)
usersLikes <- c(1,2,4)

meanRated <- cbind(movie_id=c(rownames(mostRated)),mean=c(rowMeans(mostRated,na.rm = TRUE)))


new <-0

new <-subset(movieAdvGenres, movie_id %in% mostRatedMovies)

z <- is.na(user.movie_id.matrix.NA[mostRatedMovies,93])
newzzz <- 0
newzzz <-subset(new, movie_id %in% mostRatedMovies[z])
newzzz

colSums(rated[6:24]) 

chosenGenres <- c(2,3,6,9,15,16,17) 
# Action Adventure Comedy Drama Romance Sci.Fi Thriller
genre <- newzzz[6:24]
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
for(i in 1:nrow(newzzz)){
  R_pAll[i] <- R_p(i,usersLikes, correlation, newzzz,genre)
}
sort(R_pAll,decreasing = TRUE)

R_pAllOrder <- order(R_pAll,decreasing = TRUE)

newzzz[R_pAllOrder,1]
newzzz[1]
is.na(user.movie_id.matrix.NA["92"])
mostRatedMovies
user.movie_id.matrix.NA[,93]

z <- is.na(user.movie_id.matrix.NA[mostRatedMovies,93])
user.movie_id.matrix.NA[mostRatedMovies,93]
mostRatedMovies[z]
user.movie_id.matrix.NA[row.names(new),93]
TR <- R_pAllOrder[z==TRUE]
TR
new[mostRatedMovies[z],1]
newzz <-subset(user.movie_id.matrix.NA, movie_id %in% mostRatedMovies[z])

new[newzzz[z==TRUE],1]
new[TR,1]
newzzz[R_pAll[1:10]]
user.movie_id.matrix.NA[R_pAllOrder[z==1],1]
R_pAllOrder
