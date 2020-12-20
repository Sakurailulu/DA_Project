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
#WineQuality$high <-ifelse(WineQuality$quality>=7,1,0)
#heat map
WineQuality_matrix <- as.matrix(WineQuality)
cormat <- round(cor(WineQuality_matrix),2)
head(cormat)
melted_wine <- melt(cormat)
head(melted_wine)
ggplot(data = melted_wine, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+
  scale_fill_gradient2(low = "red",mid = "purple",high = "blue", guide = "colourbar")

#correlation plot
corrplot(cor(WineQuality_matrix))

#KNN 
set.seed(400)
#categorize the wine data by quality rating, low, med and high
WineQuality$high <- ifelse(WineQuality$quality>=7,1,0)
WineQuality$med <- ifelse(WineQuality$quality>=5 & WineQuality$quality<7 , 1,0)
WineQuality$low <- ifelse(WineQuality$quality<5,1,0)

train_wine = sample(1:nrow(WineQuality),nrow(WineQuality)/2)
test_wine = (-train_wine)

WineQuality.train_wine = WineQuality[train_wine,-1]
low.train_wine = WineQuality$low[train_wine]
WineQuality.test_wine = WineQuality[test_wine,-1]
low.test_wine = WineQuality$low[test_wine]
knn.pred_low_5 = knn(WineQuality.train_wine,WineQuality.test_wine,low.train_wine,k=5)
# display a proportion table that shows predicted low vs. actual low quality
prop.table(table(knn.pred_low_5,low.test_wine))
# calculate prediction accuracy
mean(knn.pred_low_5==low.test_wine)

WineQuality.train_wine = WineQuality[train_wine,-2]
med.train_wine = WineQuality$med[train_wine]
WineQuality.test_wine = WineQuality[test_wine,-2]
med.test_wine = WineQuality$med[test_wine]
# display a proportion table that shows predicted med vs. actual med quality
knn.pred_med_5 = knn(WineQuality.train_wine,WineQuality.test_wine,med.train_wine,k=5)
prop.table(table(knn.pred_med_5,med.test_wine))
# calculate prediction accuracy
mean(knn.pred_med_5==med.test_wine)

WineQuality.train_wine = WineQuality[train_wine,-3]
high.train_wine = WineQuality$high[train_wine]
WineQuality.test_wine = WineQuality[test_wine,-3]
high.test_wine = WineQuality$high[test_wine]
knn.pred_high_5 = knn(WineQuality.train_wine,WineQuality.test_wine,high.train_wine,k=5)
# display a proportion table that shows predicted high vs. actual high quality
prop.table(table(knn.pred_high_5,high.test_wine))
# calculate prediction accuracy
mean(knn.pred_high_5==high.test_wine)







