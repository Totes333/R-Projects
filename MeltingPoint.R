# Advaith Cheruvu
# September 14th, 2021
# MeltingPoint.r
# Data analysis of melting point data
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advaithc/Desktop/RProjects/DSFS Labs")
#
# install and load libraries
library(tidyverse)
source("myfunctions.R")
#
# read mp data
meltingPoints <- read.csv(file = "/Users/advaithc/Desktop/RProjects/DSFS Labs/dirtyMPdata.csv",header=T)
#
# Looking at the melting points data
head(meltingPoints)
names(meltingPoints)
sapply(meltingPoints, class)
str(meltingPoints)
glimpse(meltingPoints)
summary(meltingPoints)
dim(meltingPoints)
#
# Check for missing data
missingData <- ifelse(complete.cases(meltingPoints) == TRUE, 1, 0)
paste("There are", dim(meltingPoints)[1]-sum(missingData), "rows with missing data.")
#
# Check which columns have missing data
missingDataCol <-colnames(meltingPoints)[apply(meltingPoints, 2, anyNA)]
paste0("The following columns have missing data: ", missingDataCol)
#
# Replace data with the mean of existing data
meltingPoints$energy <- ifelse(is.na(meltingPoints$energy), round(mean(meltingPoints$energy, na.rm=TRUE),0), meltingPoints$energy)
#
# Creating a table with data distribution
minimum <- c(round(min(meltingPoints$mp),1), round(min(meltingPoints$molar.mass),1), round(min(meltingPoints$heavy.atoms),1), round(min(meltingPoints$logP),1), round(min(meltingPoints$refractivity), 1), round(min(meltingPoints$dipole.moment),1))
maximum <- c(round(max(meltingPoints$mp),1), round(max(meltingPoints$molar.mass),1), round(max(meltingPoints$heavy.atoms),1), round(max(meltingPoints$logP),1), round(max(meltingPoints$refractivity), 1), round(max(meltingPoints$dipole.moment),1))
mean <- c(round(mean(meltingPoints$mp),1), round(mean(meltingPoints$molar.mass),1), round(mean(meltingPoints$heavy.atoms),1), round(mean(meltingPoints$logP),1), round(mean(meltingPoints$refractivity), 1), round(mean(meltingPoints$dipole.moment),1))
standard.deviation <- c(round(sd(meltingPoints$mp),1), round(sd(meltingPoints$molar.mass),1), round(sd(meltingPoints$heavy.atoms),1), round(sd(meltingPoints$logP),1), round(sd(meltingPoints$refractivity), 1), round(sd(meltingPoints$dipole.moment),1))
firstQuartile <- c(118, 243.3, 17, 2.2, 6.6, 2.4)
median <- c(round(median(meltingPoints$mp),1), round(median(meltingPoints$molar.mass),1), round(median(meltingPoints$heavy.atoms),1), round(median(meltingPoints$logP),1), round(median(meltingPoints$refractivity), 1), round(median(meltingPoints$dipole.moment),1))
thirdQuartile <- c(210.5, 376.5, 27, 4.6, 10.2, 5.5)
#
# data frame of data distribution
dataDist <- data.frame(minimum, maximum, mean, standard.deviation, firstQuartile, median, thirdQuartile)
dataDist <- t(dataDist)
colnames(dataDist) <-c("meltingPoint/C", "molecular weight/g/mol", "number of heavy atoms", "SlogP", "molar refractivity/cm3", "dipole moment (AMI)/debye")
#
# linear regression plots
mpFormalChargeFit <- lm(meltingPoints$mp~meltingPoints$formal.charge)
plot(meltingPoints$formal.charge, meltingPoints$mp, 
     main="Plot of Melting Point vs. Charge of a Molecule")
abline(mpFormalChargeFit)

mpVolumeFit <- lm(meltingPoints$mp~meltingPoints$volume)
plot(meltingPoints$volume, meltingPoints$mp, 
     main="Plot of Melting Point vs. Charge of a Molecule")
abline(mpVolumeFit)

mpRefractivityFit <- lm(meltingPoints$mp~meltingPoints$refractivity)
plot(meltingPoints$refractivity,meltingPoints$mp, 
     main="Plot of Melting Point vs. Charge of a Molecule")
abline(mpRefractivityFit)
#
# Multifit
multiFit <- lm(meltingPoints$mp~meltingPoints$refractivity+meltingPoints$formal.charge+meltingPoints$volume)
summary(multiFit)
#
# BIC scores targeted on melting point
# Question: Do the dipole moment and Melting Point have a causal relationship?
BIC(lm(meltingPoints$dipole.moment~1))
BIC(lm(meltingPoints$dipole.moment~meltingPoints$mp))
#
# Since the BIC score of the relationship is greater 
# than 10 less than the BIC score of just the dipole 
# moment, there is likely to be some causation 
# between dipole moments and melting points
#
# EOF (End of File)