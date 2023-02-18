# Advaith Cheruvu
# September 6th, 2021
# WK3L1Cheruvu.R
# This script prepares alcohol and tobacco data
# for various ages
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
source("myfunctions.R")
#
# install and load libraries
library(tidyverse)
library(datasets)
#
# load esoph data
#esoph <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\esophOrig.csv",header=T)
esoph <- esophOrig
names(esoph)
summary(esoph)
sapply(esoph)
str(esoph)
dim(esoph)
class(esoph$Tobacco)
glimpse(esoph)
head(esoph)
#
# renaming columns
colnames(esoph)<-c("AgeGroup","Alcohol","Tobacco","Cases","Controls")
#
# reclassify the age group
esoph$AgeGroup <- factor(esoph$AgeGroup, 
  c("25-34", "35-44", "45-54", "55-64", "65-74", "75+"), 
  labels=c("Millenials", "Xennials", "GenX", "Boomers", "Seniors", "Elderly"))
#
# reclassify the alcohol group
esoph$Alcohol <- factor(esoph$Alcohol, levels=c("0-39g/day", "40-79", "80-119", "120+"),
  labels=c("Minimal", "Moderate", "Significant", "Heavy"))
#
# reclassify the tobacco group
esoph$Tobacco <- factor(esoph$Tobacco, levels=c("0-9g/day", "10-19", "20-29", "30+"),
  labels=c("Minimal", "Moderate", "Significant", "Heavy"))
#
# New datasets for age groups and alcohol, 
# heavy smokers, and light/moderate smokers
alcoholAge <- select(esoph, AgeGroup, Alcohol)
heavySmoke <- filter(esoph, Tobacco=="Heavy")
lightModSmoke <- filter(esoph, Tobacco=="Minimal" | Tobacco=="Moderate")
#
# New column for difference
esoph <- mutate(esoph, Difference = abs(Controls -Cases))
#
# Organizing database based on the difference 
esoph <- arrange(esoph, -Difference, AgeGroup)
#
# summarizing the mean of controls and cases
meanControls = mean(esoph$Controls)
meanCases = mean(esoph$Cases)
meanConCases = summarize(esoph,meanControls,meanCases,)
round(meanConCases, digits=2)
#
# function that finds control value furthest away from control mean
controlsFurthest = esoph$Controls[which.max(abs(esoph$Controls - mean(esoph$Controls)))]
#
# create a dataset that groups by alcohol use
AlcoholGroup <- esoph %>% 
  group_by(Alcohol)
#
# create a dataset that groups by age and summarizes mean of controls and cases
AgeGroupSum <- esoph %>%
  group_by(AgeGroup) %>%
  summarize(meanControls,meanCases)
#
# writing the new CSV file
write.csv(alcoholAge,'alcoholAge.csv')
write.csv(AgeGroupSum,'AgeGroupSum.csv')
write.csv(AlcoholGroup,'AlcoholGroup.csv')
write.csv(heavySmoke,'heavySmoke.csv')
write.csv(lightModSmoke,'lightModSmoke.csv')
write.csv(meanConCases,'meanConCases.csv')
