data <- data.frame(Age = numeric(0), Income=numeric(0))
data1 <- edit (data)

View(data1)
data1$Age <- scale(data1$Age)
data1$Income <- scale(data1$Income)

cluster <- kmeans(data1,2)
data1$cluster <- cluster$cluster


### Import the movie.data
movie <-