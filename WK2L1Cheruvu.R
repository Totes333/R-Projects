# Advaith Cheruvu
# August 29th, 2021
# WK2L1Cheruvu.R
# this script shows the process of cleaning a dataset
# for passengers of the titanic
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
source("myfunctions.R")
#
# install and load libraries
library(tidyverse)
library(dplyr)
#
# load titanic data
passengers <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\titanic.csv",header=T)
#
# overview of the data set
names(passengers)
summary(passengers)
str(passengers)
dim(passengers)
class(passengers)
glimpse(passengers)
#
# removing passenger ID column
passengers = select(passengers, -1)
#
# replacing 1s and 0s with "yes" and "no"
passengers$Survived<-ifelse(passengers$Survived=="1","Yes","No")
#
# replacing the passenger class data with "Upper", "Middle", and "Lower". 
passengers$Pclass<-ifelse(passengers$Pclass==1,"Upper",ifelse(passengers$Pclass==2,"Middle","Lower"))
#
# replacing the embarkation point with the full names
passengers$Embarked<-ifelse(passengers$Embarked=="C","Cherbourg",
ifelse(passengers$Embarked=="Q","Queenstown","Southampton"))
#
# replacing the missing age data with the mean of the known age data
passengers$Age <- ifelse(is.na(passengers$Age), round(mean(passengers$Age, na.rm=TRUE),3), passengers$Age)
#
# changing column names of "Sibsp" and "Parch" to "Siblings" and "ParentsChildren"
colnames(passengers)<-c("Survived", "Pclass","Name","Sex","Age","Siblings","ParentsChildren","Ticket","Fare","Cabin","Embarked")
#
# creating a histogram plot by creating a collection
# of some numerical values (Age, Siblings, ParentsChildren, Fare)
numerics <- passengers[,c(5:7,9)]
summary(numerics)
#
# looking at correlations
pairs(numerics,upper.panel = panel.cor,diag.panel = panel.hist)
#
# creating transformations
Age.rz <- rz.transform(passengers$Age)
hist(Age.rz)
Siblings.rz <- rz.transform(passengers$Siblings)
hist(Siblings.rz)
ParentsChildren <- rz.transform(passengers$ParentsChildren)
hist(ParentsChildren)
Fare <- rz.transform(passengers$Fare)
hist(Fare)
#
# some of the data require more transformations and
# other data has a better Gaussian distribution
