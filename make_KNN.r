## knn

library(ggplot2)
library(dplyr)

#------------------------------------ load iris data
data <- iris
names(data)

#------------------------------------ draw plot with only two features
data %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point() + theme(legend.position = "bottom")

#------------------------------------ draw plot with pca data
pca_df <- prcomp(data[,-5])
plot(pca_df, type = "l")
summary(pca_df)

pca_data <- data.frame(predict(pca_df))
pca_data$Species <- data$Species
names(pca_data)

pca_data %>% ggplot(aes(x = PC1, y = PC2, colour = Species)) + geom_point() + theme(legend.position = "bottom")

#------------------------------------- knn classfication
library(class)

index <- sample(1:150, 100, replace = F)

train <- data[index,-5]
test <- data[-index,-5]
label <- data$Species[index]
k <- 10

knn_pred <- knn(train, test, label, k)

table(data$Species[-index], knn_pred)

#------------------------------------- knn 구현


distance <- function(x, y){
  return(sqrt(sum((x-y)^2)))
}

find_class <- function(train, test, label, k){
  dis <- apply(train, 1, distance, y = test) 
  dis_sort <- sort(dis)
  tb <- table(label[which(names(dis) %in% names(dis_sort[1:k]))])
  return(names(which(tb == max(tb))))
}

knn_algorithm <- function(train, test, label, k){
  result <- unname(apply(test, 1, find_class, train = train, label = label, k = k))
  return(as.factor(result))
}

knn_pred2 <- knn_algorithm(data[index,-5], data[-index,-5], data$Species[index], k=10)
table(data$Species[-index], knn_pred2)



