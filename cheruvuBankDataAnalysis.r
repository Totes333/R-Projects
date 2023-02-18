# Advaith Cheruvu
# November 14th 2021
# bankDataAnalysis.r
# Data analysis on Bank client data from the UCI
# machine learning repository
#
# clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
#
# libraries
library(tidyverse)
library(caret)
library(ggplot2)
# functions
source("myfunctions.R")
#
# read in bank data I obtained this data from the 
# UCI machine learning repository
bank <- read.csv("bank-additional.csv", sep=';', header = TRUE)
# I obtained this data from the UCI machine 
# learning repository (https://archive.ics.uci.edu/ml/datasets/Bank+Marketing)
#
# look at data
head(bank)
names(bank)
summary(bank)
dim(bank)
#
# the data is half numeric and half categorical
# the output variable "y" shows if the client
# subscribed to a term deposit


# the output variable is a character, but I will be
# working with it as a numeric
bank$ynum <- ifelse(bank$y=="yes",1,0)
# pairwise plots
pairs(~bank$age + bank$ynum + bank$campaign + bank$pdays + bank$previous, upper.panel = panel.cor, diag.panel = panel.hist)
pairs(~bank$emp.var.rate + bank$cons.price.idx + bank$cons.conf.idx + bank$nr.employed, upper.panel = panel.cor, diag.panel = panel.hist)
# pdays (days passed after client was last contacted)
# is somewhat negatively correlated with previous 
# (number of contacts performed during this campaign and for this client)
# 
# emp.var.rate (employment variation rate) is highly 
# correlated with cons.price.idx (consumer price index)
# and nr.employed (number of employees)
#

#
# population by emp.var.rate and cons.price.idx scatterplot

EVR.CPI.sp <- ggplot(bank, aes(x=emp.var.rate,       # sets the x 
                          y=cons.price.idx))+        # sets the y 
                          geom_point() +             # tells R to create a scatter plot
  xlab("Employment Variation Rate") +                  # labels the x axis
  ylab("Consumer Price Index") +                     # labels the y axis
  ggtitle("Scatterplot of EVR by CPI") +             # labels the plot
  theme(plot.title = element_text(hjust = 0.5))      # centers the title


EVR.CPI.sp

#
# box plot of EVR and CPI grouped by jobs

EVR.CPI.bp <- ggplot(bank, aes(grouping= job,        # groups the data
                          x=cons.price.idx,          # sets the x 
                          y=emp.var.rate,            # sets the y
                          fill = job))+              # fill based on group 
  geom_boxplot() +                                   # tells R to create a boxplot
  xlab("Consumer Price Index") +                     # labels the x axis
  ylab("Employee Variation Rate") +                  # labels the y axis
  ggtitle("Boxplot of EVR by CPI Grouped by Job") +  # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+     # centers the title
  scale_fill_hue() +                                 # colors the boxplots
  guides(fill = guide_legend(title="Job"))           # renames the legend
  

EVR.CPI.bp

# BIC score analysis
#
# BIC modeling for duration -> ynum
BIC(lm(bank$ynum~1)) # 2217.398
BIC(lm(bank$ynum~bank$duration)) # 1332.358
# Supports Causation (this is expected since
# a client with a call duration of 0 cannot subscribe to a 
# term deposit)
#
# BIC modeling for EVR -> ynum
BIC(lm(bank$ynum~1)) # 2217.398
BIC(lm(bank$ynum~bank$emp.var.rate)) # 2101.804
# Supports Causation (the higher employment variation
# rate, the more people can afford to start a term deposit)
#
# BIC modeling for CPI -> ynum
BIC(lm(bank$ynum~1)) # 2217.398
BIC(lm(bank$ynum~bank$cons.price.idx)) # 2085.705
# Supports Causation (when consumer prices are cheaper
# people are more inclined to open a term deposit)
#
# BIC modeling for CPI -> ynum <- EVR
BIC(lm(bank$ynum~1)) # 2217.398
BIC(lm(bank$ynum~bank$cons.price.idx+bank$emp.var.rate)) # 1648.122
# Supports Causation (since both CPI and EVR are related
# to the financial well-being of potential customers,
# it makes sense that they support causation together)
# 
# K-Nearest Neighbors Algorithm
# random seed
set.seed(473829)
index <- sample(2, nrow(bank), replace=TRUE, prob=c(0.7, 0.3))
index
#
# training set
bank.training <- bank[index==1, c(1,11:14,16:20,22)]
#
# test set
bank.test <- bank[index==2, c(1,11:14,16:20,22)]
#
# Test and training sets take up 30% and 70% of the original data, respectively.
dim(bank.test)
dim(bank.training)
dim(bank)
#
# create bank training labels
bank.trainLabels <- bank[index==1,22]
#
# bank test labels
bank.testLabels <- bank[index==2,22]
#
# finding the best value for K
accu <-as.data.frame(matrix(nrow=1,ncol=2))
colnames(accu)<-c("rate","k")
for (i in 1:100){
#
# build model
bank_pred <- knn(train = bank.training, test = bank.test, cl = bank.trainLabels, k=i)
#
# Inspect `bank_pred`
#bank_pred
#
# Put bank.testLabels in a data frame
bankTestLabels <- data.frame(bank.testLabels)
#dim(bankTestLabels)
#
# merge bank_pred and bank.testLabels
merge <- data.frame(bank.testLabels, bank_pred)
#dim(merge)
#
# specify column names for final data
names <- colnames(bank.test)
finaldata <- cbind(bank.test, merge)
#dim(finaldata)
names(finaldata) <- c(names, "Observed Bank Call", "Predicted Bank Call")
# look at final data
#head(finaldata)
#
# Checking model accuracy
x <- sum(finaldata$`Observed Bank Call`!=finaldata$`Predicted Bank Call`)
x
y <-round((1-(x/1241))*100, 2)
paste0("This model is ", y, "% accurate over the testing set.")
accu[nrow(accu)+1,] = c(y, i)
}
# Fixing rownames
accu <- accu[-c(1),]
rownames(accu) <- 1:nrow(accu)
accu
#
# plotting accuracy by k
accuPlot <- ggplot(data=accu, aes(x=k, y=rate)) +
  geom_line() +
  geom_point()
accuPlot + ggtitle("Plot of Accuracy by K Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("K Value") + ylab("Accuracy of Model (%)") +
  ylim(89,92)
#
# optimal K value
paste0("The optimal K value is ", which.max(accu$rate), 
       " which yields an accuracy rate of ", max(accu$rate), "%.")

# EOF (End of File)