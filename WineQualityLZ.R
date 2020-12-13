library(ggplot2)
library(reshape2)
library(class)
library(lattice)
library(caret)
library(ISLR)
library(corrplot)

#import dataset and data cleaning
WineQuality <- read.csv(file = 'winequality-red.csv',sep=";",na.strings = "")
#check if dataset has NA
sum(is.na(WineQuality)) 
#check the number of rows and columns of the dataset
dim(WineQuality)
summary(WineQuality)
#determine if wine is of high quality
WineQuality$high <-ifelse(WineQuality$quality>=7,1,0)
#heat map
WineQuality_matrix <- as.matrix(WineQuality)
cormat <- round(cor(WineQuality_matrix),2)
head(cormat)
melted_wine <- melt(cormat)
head(melted_wine)
#heatmap in default color
#ggplot(data = melted_wine, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

ggplot(data = melted_wine, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+
  scale_fill_gradient2(low = "red",mid = "purple",high = "blue", guide = "colourbar")

#KNN
set.seed(400)
wine_train <- createDataPartition(y = WineQuality$quality, p = 0.7, list = FALSE)
training <- WineQuality[wine_train,]
testing <- WineQuality[-wine_train,]

#distribution in original data
prop.table(table(training$quality)) * 100
#distribution in testing data
prop.table(table(testing$quality)) * 100

#normalize KNN variables
trainx <- training[,names(training) != "quality"]
preProcessValue <- preProcess(x = trainx, method = c("center","scale"))

control <- trainControl(method = "repeatedcv", repeats = 3)
knnFit <- train(quality ~., data = training, method = "knn", trControl = control, preProcess = c("center", "scale"),tuneLength = 20)
knnFit
plot(knnFit)

#correlation plot
corrplot(cor(WineQuality_matrix))
#visualization relationship of the y variable
