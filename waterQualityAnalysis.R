# Advaith Cheruvu
# September 29th, 2021
# waterQualityAnalysis.r
# This script analyzes water quality data to
# identify patterns in the data
#
# clean up and set up
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
# load wq data
wq <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\ellerbee.csv",header=T)
#
# looking at the data
names(wq)
summary(wq)
str(wq)
dim(wq)
class(wq)
glimpse(wq)
head(wq)
#
# rename column
names(wq) <- c("Water_temperature", "Pressure","Stream_depth", "Cubic_flow_sec", "Dissolved_oxygen", "pH", "NO3", "Turbidity", "Conductance", "NH4", "Saturation")
#
# summary command tells us that there are a few
# replace columns of missing data with 0
wq[is.na(wq)] <- 0
#
# Graphics: 2 boxplots, 2 linear regression, 2 scatter plot, 2 histogram
# Boxplots
#
# cfs vs Turbidity boxplot
bp1 <- ggplot(wq, aes(group = Stream_depth,         # groups the data
                          x=Cubic_flow_sec,           # sets the x to population
                          y=Turbidity))+              # sets the y to deaths
  geom_boxplot() +                                    # tells R to create a boxplot
  xlab("Cubic flow per second") +                     # labels the x axis
  ylab("Turbidity (NTU)") +                                    # labels the y axis
  ggtitle("Boxplot of Cubic Flow per Second\nvs. Turbidity") +      # labels the plot
  theme(plot.title = element_text(hjust = 0.5))       # centers the title

# Prints the plot
bp1
#
# Boxplot of NH4 vs. pH


bp2 <- ggplot(wq, aes(group = Stream_depth,         # groups the data
                     x=NH4,           # sets the x to population
                     y=pH))+              # sets the y to deaths
  geom_boxplot() +                                    # tells R to create a boxplot
  xlab("NH4 content") +                     # labels the x axis
  ylab("pH") +                                    # labels the y axis
  ggtitle("Boxplot of\n NH4 content vs. pH") +      # labels the plot
  theme(plot.title = element_text(hjust = 0.5))

#
# Prints the plot
bp2

#
# Scatter Plots
# Water Temperature vs. Dissolved Oxygen with NO3

sp1 <-ggplot(wq, aes(
  x=Water_temperature,               # sets the x to adjusted death rate
  y=Dissolved_oxygen,          # sets the y to standard error of the adjusted death rate
  scale_color_gradient(low = "darkblue", high = "deepskyblue")))+ # creates the color gradient
  xlab("Water Temperature (C)") +                    # labels the x axis
  ylab("Dissolved Oxygen (mg/L)") +  # labels the y axis
  ggtitle("Scatterplot of \nTemperature vs. Dissolved Oxygen")+ # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+   # centers the title
  geom_point(aes(color=NO3))                # colors the plot based on NO3

# Prints the plot
sp1
#
# Turbidity vs. Dissolved Oxygen with pH
sp2 <-ggplot(wq, aes(
  x=Turbidity,              
  y=Dissolved_oxygen,          
  scale_color_gradient(low = "darkblue", high = "deepskyblue")))+ 
  xlab("Turbidity (NTU)") +                    
  ylab("Dissolved Oxygen (mg/L)") +  
  ggtitle("Scatterplot of \nTurbidity vs. Dissolved Oxygen")+ 
  theme(plot.title = element_text(hjust = 0.5))+   # centers the title
  geom_point(aes(color=pH))                # colors the plot based on pH

# Prints the plot
sp2

# Linear regression models
# Water Temperature vs. Dissolved Oxygen

lm1 <- ggplot(wq, aes(x = Water_temperature, y = Dissolved_oxygen)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  xlab("Water Temperature (C)") +                    
  ylab("Dissolved Oxygen (mg/L)") +  
  ggtitle("Linear Regression of \nWater Temperature vs. Dissolved Oxygen")+
  theme(plot.title = element_text(hjust = 0.5))

# Prints the plot
lm1

#
# Linear regression of Water Temperature vs. NO3
lm2 <- ggplot(wq, aes(x = Water_temperature, y = NO3)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  xlab("Water Temperature (C)") +                    
  ylab("NO3 (mg/L)") +  
  ggtitle("Linear Regression of \nWater Temperature vs. NO3")+
  theme(plot.title = element_text(hjust = 0.5))

# Prints the plot
lm2

#
# Violin plots
#  Water temperature vs pressure 
vp1 <- ggplot(wq, aes(x=Water_temperature,                              # sets the x to population
                      y=Pressure)) +                               # sets the y to deaths
  xlab("Water Temperature (C)") +                              # labels the x axis
  ylab("Pressure (mmHG)") +                                             # labels the y axis
  ggtitle("Violin Plot of Water Temperature\n vs Pressure") +           # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+               # centers the title
  geom_violin()                                             # tells R to create a boxplot on top of the violin plot

# Prints the plot
vp1

# Turbidity vs Conductance
vp2 <- ggplot(wq, aes(x=Turbidity,                              # sets the x to population
                      y=Conductance)) +                               # sets the y to deaths
  xlab("Turbidity (NTU)") +                              # labels the x axis
  ylab("Conductance (Microsiemens per cm)") +                                             # labels the y axis
  ggtitle("Violin Plot of Turbidity\n vs Conductance") +           # labels the plot
  theme(plot.title = element_text(hjust = 0.5))+               # centers the title
  geom_violin()                                             # tells R to create a boxplot on top of the violin plot

# Prints the plot
vp2
#
# EOF (End of File)