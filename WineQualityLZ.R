library(ggplot2)
library(reshape2)

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
#correlation plot
#visualization relationship of the y variable