# Advaith Cheruvu
# September 27th, 2021
# WK5L1Cheruvu.R
# This script cleans opiates data
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
#
# install and load libraries
library(tidyverse)
library(ggdendro)
library(ggalt)
library(plotluck)
library(gridExtra)
source("myfunctions.R")
#
# load opiates data
opiates <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\opiates.csv",header=T)
#
# looking at the data
names(opiates)
summary(opiates)
str(opiates)
dim(opiates)
class(opiates)
glimpse(opiates)
head(opiates)
#
# rename columns
names(opiates) <- c("X", "Year","State_name", "FIPS_code", "Deaths", "Population", "Crude_death_rate", "Adjusted_death_rate", "Standard_error_adjusted_death_rate", "Census_region", "Abbreviated_state_name", "Census_division")
#
# converting Census region to factor
opiates$Census_region <- as.factor(opiates$Census_region)
opiates$Census_region <- ordered(opiates$Census_region, levels = c("Northeast", "South", "West", "Midwest"))
class(opiates$Census_region)
#
# summary command tells us that columns "crude" and "adjusted" have 28 NAs
# fill missing data with 0
opiates[is.na(opiates)] <- 0
#
# converting column population to population in the millions
opiates$Population <- (opiates$Population/1000000)
#
# deaths vs population boxplot
bp <- ggplot(opiates, aes(grouping= Census_division,  #groups the data
                         x=Population,                # sets the x to population
                         y=Deaths))+                  # sets the y to deaths
  geom_boxplot() +                                    # tells R to create a boxplot
  xlab("Population (millions)") +                     # labels the x axis
  ylab("Deaths") +                                    # labels the y axis
  ggtitle("Boxplot of deaths\nvs. population") +      # labels the plot
  theme(plot.title = element_text(hjust = 0.5))       # centers the title

# Prints the plot
bp

#
# population by deaths and region scatterplot

sp <- ggplot(opiates, aes(grouping = Census_region,   # groups the data
                    x=Population,                     # sets the x to population
                    y=Deaths,                         # sets the y to deaths
                    color = Census_region))+          # colors the points based on the census region
  geom_point() +                                      # tells R to create a scatter plot
  xlab("Population (millions)") +                     # labels the x axis
  ylab("Deaths") +                                    # labels the y axis
  ggtitle("Scatterplot of population\n by deaths\nand region") +  # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+                  # centers the title
  scale_color_manual(values=c("green4", "cornflowerblue", "purple", "tomato1")) # changes the colors of the points

# Prints the plot
sp 
#
# Violin plot of deaths vs population

vp <- ggplot(opiates, aes(grouping = Abbreviated_state_name,   # groups the data
                    x=Population,                              # sets the x to population
                    y=Deaths)) +                               # sets the y to deaths
  xlab("Population (millions)") +                              # labels the x axis
  ylab("Deaths") +                                             # labels the y axis
  ggtitle("Violin Plot of deaths\n vs population") +           # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+               # centers the title
  geom_violin()+                                               # tells R to create a violin plot
  geom_boxplot()                                               # tells R to create a boxplot on top of the violin plot

# Prints the plot
vp
#
# Stripchart of deaths vs. population

sc <-ggplot(opiates, aes(
                         x=Adjusted_death_rate,               # sets the x to adjusted death rate
                         y=Standard_error_adjusted_death_rate,# sets the y to standard error of the adjusted death rate
                         scale_color_gradient(low = "darkblue", high = "deepskyblue"), # creates the color gradient
                         shape = Census_region)) + # changes the points to shapes based on census region
  xlab("Adjusted Death rate") +                    # labels the x axis
  ylab("Standard Error of Adjusted Death Rate") +  # labels the y axis
  ggtitle("Stripchart of deaths\n vs population")+ # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+   # centers the title
  geom_point(aes(color=FIPS_code))+                # colors the plot based on FIPS code
  ylim(0.0, 1.5)                                   # changes the y axis scale

# Prints the plot
sc
#
# Consolidating these charts onto one page using gridExtra
grid.arrange(bp, sp, vp, sc, ncol=2)