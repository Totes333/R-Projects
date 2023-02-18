# Advaith Cheruvu
# September 27th, 2021
# drugsPCA.r
# This script conducts PCA for drug data
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
#
# install and load libraries
library(ggplot2)
source("myfunctions.R")
#
# load drugs data
drugs <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\drugs.csv",header=TRUE)
#
# looking at the data
names(drugs)
summary(drugs)
str(drugs)
dim(drugs)
class(drugs)
glimpse(drugs)
head(drugs)
#
# converting this data to a numeric matrix
drugs.matrix <- data.matrix(drugs[2:15])
rownames(drugs.matrix) <- drugs$X
#
# prcomp for single value decomposition
pca <- prcomp(drugs.matrix, scale=TRUE)
summary(pca)
pca$x[,1]
#
# Plot of PCA 
plot(pca$x[,1],pca$x[,2])
#
# Scree Plot
screeplot(pca, main="Scree Plot")
#
# Better Scree Plot
pca.variance <- pca$sdev^2
pca.variance.per <-round(pca.variance/sum(pca.variance)*100,1)
pca.variance.per
barplot(pca.variance.per, main="Scree plot", xlab="Principal Components", ylab="Percent Variable", ylim=c(0,100),names.arg=pca.variance.per)
#
# Putting pca data into a data frame
pca.data <- data.frame(Sample = rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
pca.data
#
# ggplot of pca.data
ggplot(data=pca.data,aes(x=X, y=Y, label=Sample)) + geom_text() + xlab(paste("PC1 - ", pca.variance.per[1], " %", sep="")) + ylab(paste("PC2 - ",
pca.variance.per[2], " %", sep="")) + theme_bw() + ggtitle("PCA Plot")
#
# loading scores (which factors contribute to PC1)
loading_scores <- pca$rotation[,1]
loading_scores
drug_scores <- abs(loading_scores)
drug_scores_ranked <- sort(drug_scores, decreasing = TRUE)
top5 <- names(drug_scores_ranked[1:5])
top5
# loading scores (which factors contribute toPC2)
loading_scores2 <- pca$rotation[,2]
loading_scores2
drug_scores2 <- abs(loading_scores2)
drug_scores_ranked2 <- sort(drug_scores2, decreasing = TRUE)
top5 <- names(drug_scores_ranked2[1:5])
top5
#
# Biplot of PCA
biplot(pca)
# EOF (End of File)