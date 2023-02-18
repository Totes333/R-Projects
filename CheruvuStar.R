# Advaith Cheruvu
# September 13th, 2021
# CheruvuStar.R
# This script produces plots and charts for star data
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/CompSci")
#
# install and load libraries
library(tidyverse)
source("myfunctions.R")
#
# read stars data
stars <- read.csv(file = "C:\\Users\\advai\\Documents\\CompSci\\HIP_star.csv",header=T)
#
# creating file for column names
sink(file = "starnames.txt")
names(stars)
sink()
#
# Right ascension vs declination plot
# Saving the plot as a file
jpeg("starchart.jpg")
#
# Plotting RA and DE
plot(stars$RA,stars$DE, xlab = "RA", ylab = "DE", pch="*",
     main = "Plot of all stars with right ascension (RA) and declination (DE)")
#
# Closing the plot file
graphics.off()
#
# Saving the plot as a file
jpeg("boxplots.jpg")
#
# Setting the plot window to 2x2
par(mfrow=c(2,2))
#
# Box plots
boxplot(stars$Vmag, main = "Vmag")
boxplot(stars$Plx, main = "Plx")
boxplot(stars$pmRA, main = "pmRA")
boxplot(stars$pmDE, main = "pmDE")
#
# Setting the plot window back to 1x1
par(mfrow=c(1,1))
#
# Closing the boxplots file
graphics.off()
#
# Adding a new column for logPlx
stars$logPlx <- log10(stars$Plx)
#
# Adding a new column for logL
stars$logL <- (15 - stars$Vmag - 5*stars$logPlx)/2.5
#
# Filtering stars with Vmag >=8 and <= 9
vmagFiltered <- filter(stars, stars$Vmag >= 8 & stars$Vmag <= 9)
#
# Saving the plot as a file
jpeg("VmagvsBV.jpg")
#
# Plotting Vmag vs BV for the filter Vmag values
plot(vmagFiltered$Vmag, vmagFiltered$B.V, 
     xlab = "Vmag[vmagfiltered]", 
     ylab = "B.V.[vmagfiltered]", 
     main = "Plot of Filtered Vmag values and B.V",
     pch="*")
#
# Closing the plot file
graphics.off()
#
# Saving the plot as a file
jpeg("BVvslogL.jpg")
#
# plot of vmag filtered BV and logL with linear regression
plot(vmagFiltered$B.V,vmagFiltered$logL, 
     main = "Plot of B-V and logL (luminosity)",
     xlab = "B.V[vmagfiltered]", ylab = "logL[vmafiltered]",
     abline(lm(vmagFiltered$logL~vmagFiltered$B.V)),
     pch="*"
     )
#
# Closing the plot file
graphics.off()
#
# EOF (End of File)