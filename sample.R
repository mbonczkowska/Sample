setwd("C:/Users/Magdalena/Desktop/MachineLearning")



u_cols <- c('user_id', 'age', 'sex', 'occupation', 'zip_code')
users <- read.csv('u.user', sep='|', col.names=u_cols, header=FALSE)

r_cols <- c('user_id', 'movie_id', 'rating', 'unix_timestamp')
ratings <- read.csv('u.data', sep='\t', col.names=r_cols,header = FALSE)

m_cols <- c('movie_id', 'title', 'release_date')
#m_cols <- c('movie_id', 'title', 'release_date', 'video_release_date', 'imdb_url', 'unknown','Action','Adventure','Animation','Children\'s','Comedy','Crime','Documentary','Drama','Fantasy','Film-Noir','Horror','Musical','Mystery','Romance','Sci-Fi','Thriller','War','Western')
movies <- read.csv('u.item', sep='|',header=FALSE )


movies <- movies[1:3]
colnames(movies) <- m_cols

mlens <- merge(movies, ratings, all = TRUE)
#lens <- merge(users, ratings, all = TRUE)
#ml <- merge(mlens, lens, all = TRUE)

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

correlation <- 0
correlation <- cor(mostRated,use="pairwise.complete.obs")



mostRatedNotNA <- mostRated
for (i in 1:nrow(mostRatedNotNA)) {
  for (j in 1:ncol(mostRatedNotNA)) {
    if(is.na(mostRatedNotNA[i,j]))
    {
      mostRatedNotNA[i,j] <- 0;
    }
  }
}


corr405 <- 0 
for (i in 1:nrow(correlation)) {
  corr405[i] <- correlation[1,i]
}
corr405
# osoby z najwyższą korelacją do 405
colnames(mostRated[order(corr405,decreasing = TRUE)])

sort405 <- sort(corr405,decreasing = TRUE)
n405 <- colnames(mostRated[order(corr405,decreasing = TRUE)])[2:6]
w_n405 <- sort405[2:6]

corr405
n405
w_n405

mostRatU <- 0
for(j in 1:ncol(mostRated)){
  mostRatU[j] <- sum(!is.na(mostRated[j]))
}
sort(mostRatU)

mostRatM <- 0
for(i in 1:nrow(mostRated)){
  mostRatM[i] <- sum(!is.na(mostRated[i,1:ncol(mostRated)]))
}
order(mostRatM)
mostRated[77,]

countRating <- 0
sumRating <- 0
for (i in 1:nrow(mostRatedNotNA)) {
  for(j in 1:ncol(mostRatedNotNA)){
    if(colnames(mostRatedNotNA[j]) == "405"  ){
      sumRating <- mostRatedNotNA[i,j] + sumRating
      
      if(mostRatedNotNA[i,j] != 0){
        countRating <- 1 + countRating
      }
    }
  }
}
mean405 <- sumRating/countRating

colnames(mostRatedNotNA)
n406
countRating <- 0
sumRating <- 0
for(k in 1:5){
  for (i in 1:nrow(mostRatedNotNA)) {
    for(j in 1:ncol(mostRatedNotNA)){
      if(colnames(mostRatedNotNA[j]) == n405[k]  ){
        
        if(i == 1){
          sumRating[k] <- 0
          countRating[k] <- 0
        }
        
        sumRating[k] <- mostRatedNotNA[i,j] + sumRating[k]
        
        if(mostRatedNotNA[i,j] != 0){
          countRating[k] <- 1 + countRating[k]
        }
      }
    }
  }
}
meanN405 <- sumRating/countRating


sumu <- 0
sumd <- 0

for (k in 1:5) {
  
  for (j in 1:nrow(mostRated)) {
    
    for (i in 2:ncol(mostRated)) {
      
      if(colnames(mostRated[i]) == n405[k] ){
        
        if(k == 1){
          sumu[j] <- 0
          sumd[j] <- 0
        }
        
        if(mostRatedNotNA[j,i] != 0){
          sumu[j] <- (mostRatedNotNA[j,i] - meanN405[k]) * w_n405[k] + sumu[j]
          sumd[j] <- w_n405[k] + sumd[j]
        }
        
      }
      
    }
    
  }
  
}

normalized <- mean405 + (sumu/sumd)
order(normalized,decreasing = TRUE)
score <- rownames(mostRated[order(normalized,decreasing = TRUE),])

score[1:10]
user.title.matrix.NA[score,1][1:10]

s<- 0
for (k in 1:5) {
  s[k] <- mostRated["180",n405[k]]
}
s
s<- 0
for (i in 1:length(score)) {
  s[i] <- mostRated[score[i],1]
}
s
rownames(mostRated)





