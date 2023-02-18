# Advaith Cheruvu
# November 14th 2021
# glassknn.r
# Data analysis on forensic glass data data from the UCI
# machine learning repository
#
# clean up and set up
rm(list=ls())
setwd("/Users/advaithc/Desktop/RProjects/DSFS Labs")
#
# libraries
library(class)
library(ggvis)
library(gmodels)
library(tidyverse)
library(caret)
library(GGally)
library(gridExtra)
#
# read in glass data
glass <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data"),  na.strings = "?", header = FALSE)
#
# look at data
head(glass)
names(glass)
summary(glass)
dim(glass)
#
# column names
names(glass) <- c("ID", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "source")
#
# check the glass data names
names(glass)
# using only half the columns so that the pairwise graph is readable
short1 <- glass[, c(2:6,11)]
short2 <- glass[, c(7:10,11)]
ggpairs(short1, aes(color = as.character(source)))
ggpairs(short2, aes(color = as.character(source)))
#
# Correlation (Barium and Aluminum have high correlation with source)
cor(as.numeric(glass$Al), as.numeric(glass$source))
cor(as.numeric(glass$Ba), as.numeric(glass$source))
# Aluminum and Silicon have high negative correlation with Refractive Index
cor(as.numeric(glass$Al), as.numeric(glass$RI))
cor(as.numeric(glass$Si), as.numeric(glass$RI))
#
# scatter plots
# Al vs RI
glass %>% ggvis(x= ~Al, y=~RI, fill = ~source) %>% layer_points()
# Si vs RI
glass %>% ggvis(x= ~Si, y=~RI, fill = ~source) %>% layer_points() 
# Mg vs Al
glass %>% ggvis(x= ~Mg, y=~Al, fill = ~source) %>% layer_points()
#
# random seed
set.seed(237894)
index <- sample(2, nrow(glass), replace=TRUE, prob=c(0.7, 0.3))
index
#
# training set
glass.training <- glass[index==1, 1:11]
#
# test set
glass.test <- glass[index==2, 1:11]
#
# Test and training sets take up 30% and 70% of the original data, respectively.
dim(glass.test)
dim(glass.training)
dim(glass)
#
# create glass training labels
glass.trainLabels <- glass[index==1,11]
#
# glass test labels
glass.testLabels <- glass[index==2, 11]
#
# build model
glass_pred <- knn(train = glass.training, test = glass.test, cl = glass.trainLabels, k=7)
#
# Inspect `glass_pred`
glass_pred
#
# Put glass.testLabels in a data frame
glassTestLabels <- data.frame(glass.testLabels)
dim(glassTestLabels)
#
# merge glass_pred and glass.testLabels
merge <- data.frame(glass.testLabels, glass_pred)
dim(merge)
#
# specify column names for final data
names <- colnames(glass.test)
finaldata <- cbind(glass.test, merge)
dim(finaldata)
names(finaldata) <- c(names, "Observed Glass Source", "Predicted Glass Source")
# look at final data
head(finaldata)
#
# Checking model accuracy
x <- 0
ifelse(finaldata$`Observed Glass Source` != finaldata$`Predicted Glass Source`, x <- x+1, x <- x)
paste0("This model is ", round((1-(x/68))*100, 2), "% accurate over the testing set.")
#
# Checking model accuracy
x <- 0
ifelse(finaldata$`Observed Glass Source` != finaldata$`Predicted Glass Source`, x <- x+1, x <- x)
paste0("This model is ", round((1-(x/68))*100, 2), "% accurate over the testing set.")
#
# EOF (End of File)