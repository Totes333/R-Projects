# Advaith Cheruvu
# August 30th, 2021
# WK2L2Cheruvu.R
# this script shows the process of cleaning a dataset
# for the weather data in Chicago
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
source("myfunctions.R")
#
# install and load libraries

library(tidyverse)
#
# load weather data
weather <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\chicago.csv",header=T)
#
# overview of the data set
names(weather)
summary(weather)
str(weather)
dim(weather)
class(weather)
glimpse(weather)
head(weather)
#
# removing index and city columns
weather = select(weather, -1:-2)
#
# renaming columns
colnames(weather)<-c("Temp", "Dewpoint","Date","PM25","PM10","O3","NO3")
#
# Changing the date format
weather$Date <- as.Date(weather$Date, format= "%m/%d/%y")
#
# Normalizing PM25 Data
weather$PM25 <- ifelse(is.na(weather$PM25), round(mean(weather$PM25, na.rm=TRUE),3), weather$PM25)
PM25.normalize <- weather$PM25/max(weather$PM25)
weather$PM25 <- PM25.normalize
# Normalizing PM10 Data
weather$PM10 <- ifelse(is.na(weather$PM10), round(mean(weather$PM10, na.rm=TRUE),3), weather$PM10)
PM10.normalize <- weather$PM10/max(weather$PM10)
weather$PM10 <- PM10.normalize
#
# Taking out columns with numeric values
numerics <- weather[,c(1:2,4:7)]
summary(numerics)
#
# Looking at Correlation
pairs(numerics,upper.panel = panel.cor,diag.panel = panel.hist)
#
# Transformations and histogram plots
Temp.rz <- rz.transform(weather$Temp)
hist(Temp.rz)
Dewpoint.rz <- rz.transform(weather$Dewpoint)
hist(Dewpoint.rz)
PM25.rz <- rz.transform(weather$PM25)
hist(PM25.rz)
PM10.rz <- rz.transform(weather$PM10)
hist(PM10.rz)
O3.rz <- rz.transform(weather$O3)
hist(O3.rz)
NO3.rz <- rz.transform(weather$NO3)
hist(NO3.rz)