# Advaith Cheruvu
# September 9th, 2021
# Air Quality Data Analysis Script 
# this script shows basic data analysis of air quality
# data, including linear regression and BIC scoring
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/CompSci")
source("myfunctions.R")
#
# install and load libraries
#
library(tidyverse)
#
# load AQ data
airQuality <- read.csv(file = "C:\\Users\\advai\\Documents\\CompSci\\airquality.csv",header=T)
#
# creating file for column names
sink(file = "names.txt")
names(airQuality)
sink()
#
# creating file for basic descriptive statistics
sink(file = "summary.txt")
summary(airQuality)
sink()
#
# other basic data exploration commands
str(airQuality)
dim(airQuality)
class(airQuality)
glimpse(airQuality)
head(airQuality)
#
# multiple linear regression model for ozone, temperature and wind.
multiFit <- lm(airQuality$Ozone ~ airQuality$Temp + airQuality$Wind)
#
# creating a file for the summary of the regression results
sink(file = "multireg.txt")
summary(multiFit)
sink()
#
# Saving the chart as a file
jpeg("regressionplot.jpg")
#
# linear regression between temp and ozone
tempOzoneFit <- lm(airQuality$Ozone ~ airQuality$Temp)
plot(airQuality$Temp,airQuality$Ozone, 
    main = "Regression of Temperature and Ozone",
    abline(lm(airQuality$Ozone~airQuality$Temp)), 
    ylab = ("Ozone"), xlab = ("Temp")
    )
#
# Closing the regression plot file
graphics.off()
#
# Saving the plots as a file
jpeg("boxplots.jpg")
#
# setting the plot window to 2x2
par(mfrow=c(2,2))
#
# Boxplots for wind, temperature, solar radiation, and ozone
boxplot(airQuality$Ozone, horizontal=TRUE, main = "Ozone Boxplot")
boxplot(airQuality$Solar.R, horizontal=TRUE, main = "Solar Boxplot")
boxplot(airQuality$Wind, horizontal=TRUE, main = "Wind Boxplot")
boxplot(airQuality$Temp, horizontal=TRUE, main = "Temp Boxplot")
#
# setting the plot window back to 1x1
par(mfrow=c(1,1))
#
# Closing the boxplots file
graphics.off()
#
#
# First save the outputs as a file
sink(file = "bicscores.txt")
#
# Bayesian Information criterion (BIC) Scores
ozoneTempBIC <- BIC(lm(airQuality$Temp~airQuality$Ozone))
ozoneSolarBIC <- BIC(lm(airQuality$Solar.R~airQuality$Ozone))
tempOzoneBIC <-BIC(lm(airQuality$Ozone~airQuality$Temp))
solarTempBIC <-BIC(lm(airQuality$Temp~airQuality$Solar.R))
solarOzoneBIC <- BIC(lm(airQuality$Ozone~airQuality$Solar.R))
tempSolarBIC <- BIC(lm(airQuality$Solar.R~airQuality$Temp))
#
# BIC scores of larger models
solarTempOzoneBIC <-BIC(lm(airQuality$Temp~airQuality$Solar.R)) + BIC(lm(airQuality$Ozone~airQuality$Temp)) 
solarTempOzone2BIC <-BIC(lm(airQuality$Ozone~airQuality$Solar.R+airQuality$Temp)) + BIC(lm(airQuality$Temp~airQuality$Solar.R))
#
#
# I'm using the paste0 command because it allows me
# put a string and number in the same line
paste0("Temp -> Ozone ", tempOzoneBIC)
paste0("Solar.R -> Ozone ", solarOzoneBIC)
paste0("Ozone -> Solar.r ", ozoneSolarBIC)
paste0("Solar.R -> Temp -> Ozone ", solarTempOzoneBIC)
paste0("Ozone -> Temp ", ozoneTempBIC)
paste0("Solar.R -> Temp -> Ozone ", solarTempOzone2BIC)
paste0("  |_________________^")
#
# Explanation of the BIC scores
print("The BIC scores of these models can be compared by determining their value against each other. Since the 5th model has the lowest BIC score, it has the strongest causal relationship. This means that there most likely to be causation between ozone and temperature, when comparing this potential causal relationship to other potential causal relationships. Conversely, the 4th model is least likely to show causal a relationship, since its BIC score was the highest. The other models show some sign of causality, but the strongest is the 5th model.")
#
# Explanation as a comment:
# The BIC scores of these models can be compared 
# by determining their value against each other. 
# Since the 5th model has the lowest BIC score, it 
# has the strongest causal relationship. This means 
# that there most likely to be causation between 
# ozone and temperature, when comparing this 
# potential causal relationship to other potential 
# causal relationships. Conversely, the 4th model 
# is least likely to show causal a relationship, 
# since its BIC score was the highest. The other 
# models show some sign of causality, but the 
# strongest is the 5th model.
#
#
# Close the output file
sink()
# EOF (End of File)