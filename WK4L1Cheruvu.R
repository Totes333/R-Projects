# Advaith Cheruvu
# September 13th, 2021
# WK4L1Cheruvu.R
# This script does EDA for mouse data
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advaithc/Desktop/RProjects/DSFS Labs")
#
# install and load libraries
library(tidyverse)
source("myfunctions.R")
#
# load mouse data
mouse <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\mouseData.csv",header=T)
#
# looking at the data
names(mouse)
summary(mouse)
str(mouse)
dim(mouse)
class(mouse)
glimpse(mouse)
head(mouse)
#
# cleaning up the column names
names(mouse) <- c("Mouse", "Sex","BloodPressure", "HeartRate", "HeartWeight")
#
# Check for missing data
clean <- ifelse(complete.cases(mouse) == TRUE, 1, 0)
paste("There are", dim(mouse)[1]-sum(clean), "rows with missing data.")
#
# Find which columns have missing data
missingDataCol <-colnames(mouse)[apply(mouse, 2, anyNA)]
paste0("The following columns have missing data: ", missingDataCol)
#
# Replace the missing data with the mean of existing data (since the missing data is numeric)
mouse$BloodPressure <- ifelse(is.na(mouse$BloodPressure), round(mean(mouse$BloodPressure, na.rm=TRUE),0), mouse$BloodPressure)
mouse$HeartRate <- ifelse(is.na(mouse$HeartRate), round(mean(mouse$HeartRate, na.rm=TRUE),0), mouse$HeartRate)
mouse$HeartWeight <- ifelse(is.na(mouse$HeartWeight), round(mean(mouse$HeartWeight, na.rm=TRUE),3), mouse$HeartWeight)
#
# Replacing sex values
mouse$Sex <- ifelse(mouse$Sex == 0, "female", ifelse(mouse$Sex == 1, "male", mouse$Sex))
#
# Checking descriptive statistics and basic structure
str(mouse)
summary(mouse)
head(mouse)
tail(mouse)
#
# Box plots of numeric data
boxplot(mouse$BloodPressure, xlab="Systolic Blood Pressure", main="Box Plot of Mouse Blood Pressure", horizontal = TRUE)
boxplot(mouse$HeartRate, xlab="Heart Rate (BPM)", main="Box Plot of Mouse Heart Rate", horizontal = TRUE)
boxplot(mouse$HeartWeight, xlab="Hear Weight (kg)", main="Box Plot of Mouse Heart Weight", horizontal = TRUE)
#
# Histograms data
hist(mouse$BloodPressure, xlab="Systolic Blood Pressure", main="Mouse Blood Pressure")
hist(mouse$HeartRate, xlab="Heart Rate (BPM)", main="Mouse Heart Rate")
hist(mouse$HeartWeight, xlab="Hear Weight (kg)", main="Mouse Heart Weight")
#
# Normalized Histogram data
hist(rz.transform(mouse$BloodPressure), xlab="Systolic Blood Pressure", main="Mouse Blood Pressure")
hist(rz.transform(mouse$HeartRate), xlab="Heart Rate (BPM)", main="Mouse Heart Rate")
hist(rz.transform(mouse$HeartWeight), xlab="Heart Weight (kg)", main="Mouse Heart Weight")
#
# Pairwise plots
pairs(~mouse$BloodPressure + mouse$HeartRate + mouse$HeartWeight, upper.panel = panel.cor, diag.panel = panel.hist)
#
# BIC modeling for BP -> Heart Rate
BIC(lm(mouse$HeartRate~1)) # 2217.143
BIC(lm(mouse$HeartRate~mouse$BloodPressure)) # 2204.246
# Supports Causation
#
# BIC modeling for Heart Rate -> BP
BIC(lm(mouse$BloodPressure~1)) # 1603
BIC(lm(mouse$BloodPressure~mouse$HeartRate)) # 1590
# Supports Causation
#
# BIC modeling for Heart Weight -> Heart Rate
BIC(lm(mouse$HeartRate~1)) # 2217
BIC(lm(mouse$HeartRate~mouse$HeartWeight)) # 2222
# No causation
#
# Linear regression of blood pressure and heart weight
plot(mouse$BloodPressure~mouse$HeartWeight, 
     main="Mouse Heart Weight vs. Blood Pressure", 
     xlab="Heart Weight (kg)", ylab="Systolic Blood Pressure")
fitline <- lm(mouse$BloodPressure~mouse$HeartWeight)
abline(fitline)
#
# Linear regression of blood pressure and heart rate
plot(mouse$BloodPressure~mouse$HeartRate, 
     main="Mouse Heart Rate vs. Blood Pressure", 
     xlab="Heart Rate (BPM)", ylab="Systolic Blood Pressure")
fitline <- lm(mouse$BloodPressure~mouse$HeartRate)
abline(fitline)
#
# Multifit of blood pressure, heart weight, and heart rate
multifit <- lm(mouse$BloodPressure ~ mouse$HeartWeight + mouse$HeartRate)
summary (multifit)
#
#
# EOF (End of File)