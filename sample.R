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


corr92 <- 0 
for (i in 1:nrow(correlation)) {
  corr92[i] <- correlation[20,i]
}
corr92
# osoby z najwyższą korelacją do 92
colnames(mostRated[order(corr92,decreasing = TRUE)])

sort92 <- sort(corr92,decreasing = TRUE)
n92 <- colnames(mostRated[order(corr92,decreasing = TRUE)])[2:6]
w_n92 <- sort92[2:6]

corr92
n92
w_n92

user.title.matrix.NA["614",n92]

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
    if(colnames(mostRatedNotNA[j]) == "92"  ){
      sumRating <- mostRatedNotNA[i,j] + sumRating
      
      if(mostRatedNotNA[i,j] != 0){
        countRating <- 1 + countRating
      }
    }
  }
}
mean92 <- sumRating/countRating

colnames(mostRatedNotNA)
n406
countRating <- 0
sumRating <- 0
for(k in 1:5){
  for (i in 1:nrow(mostRatedNotNA)) {
    for(j in 1:ncol(mostRatedNotNA)){
      if(colnames(mostRatedNotNA[j]) == n92[k]  ){
        
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
meanN92 <- sumRating/countRating
meanN92

sumu <- 0
sumd <- 0

for (k in 1:5) {
  
  for (j in 1:nrow(mostRated)) {
    
    for (i in 2:ncol(mostRated)) {
      
      if(colnames(mostRated[i]) == n92[k] ){
        
        if(k == 1){
          sumu[j] <- 0
          sumd[j] <- 0
        }
        
        if(mostRatedNotNA[j,i] != 0){
          sumu[j] <- (mostRatedNotNA[j,i] - meanN92[k]) * w_n92[k] + sumu[j]
          sumd[j] <- w_n92[k] + sumd[j]
        }
        
      }
      
    }
    
  }
  
}

normalized <- mean92 + (sumu/sumd)
sortnor <- sort(normalized,decreasing = TRUE)
score <- rownames(mostRated[order(normalized,decreasing = TRUE),])

mostRated

user.title.matrix.NA[180,1]
row.names(mostRated["1104",])
score[35]
score
mostRated["",]
sortnor[35]
score[1:10]
user.title.matrix.NA[score,1][1:10]
sum(!is.na((normalized - user.title.matrix.NA[score,93])) > 1)
normalized - user.title.matrix.NA[score,93]
k <-(normalized - user.title.matrix.NA[score,93]) < 0.5

sum(!is.na(k[k==TRUE]))
sum(!is.na(k[k==FALSE]))
z <-is.na(user.title.matrix.NA[score,93])
is.na(user.title.matrix.NA[score,93])
TR
score
TR
sort(normalized[z],decreasing = TRUE)
user.title.matrix.NA[score,1]
TR <- score[z==TRUE]

user.title.matrix.NA[TR,1]
user.title.matrix.NA[score[z==TRUE],1]

for (k in 1:5) {
  s[k] <- mostRated["180",n92[k]]
}
s
s<- 0
for (i in 1:length(score)) {
  s[i] <- mostRated[score[i],1]
}
s
rownames(mostRated)
