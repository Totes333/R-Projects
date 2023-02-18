# Advaith Cheruvu
# October 4th, 2021
# CheruvuElection.r
# This script creates graphics for election data 
# in the U.S.
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
source("theme_map.R")
#
# load election2020 data
election2020 <- read.csv("http://chemistry.ncssm.edu/digihum/data/electiondata2020.csv", header=T)
#
# looking at the data
names(election2020)
summary(election2020)
str(election2020)
dim(election2020)
class(election2020)
glimpse(election2020)
head(election2020)
#
# Renaming state column name
names(election2020)[1] <-'state'
#
# Adding a new column for trump percentage votes
election2020$trump_vote <- as.numeric(gsub(",","", election2020$trump_vote))
election2020$total_vote <- as.numeric(gsub(",","", election2020$total_vote))
election2020$trump_percent <- ((election2020$trump_vote/election2020$total_vote)*100)
#
# Adding a new column for biden percentage votes
election2020$biden_vote <- as.numeric(gsub(",","", election2020$biden_vote))
election2020$biden_percent <- ((election2020$biden_vote/election2020$total_vote)*100)
#
# Look at US States data
us_states<-map_data("state")
head(us_states)
dim(us_states)
summary(us_states)
#
# set republican/democrat colors
party_colors <- c("#2E74C0","#CB454A")
#
# Creating a simple US map
usMap <-
  ggplot(data = us_states,
  mapping = aes(x=long, y=lat, group=group, fill=region))+
  geom_polygon(color="white", size=0.25)+
  guides(fill= "none")
#
# Print the map
usMap + theme_map()
usMap
#
# Merging state data and election2020 data
election2020$region <- tolower(election2020$state)
us_states_elec2020<- left_join(us_states, election2020)
summary(us_states_elec2020)
#
# Create election2020 map and color by party
usParties2020 <-
  ggplot(data=us_states_elec2020, aes(x=long,y=lat, group=group, fill=party))+ #maps aesthetics
  geom_polygon(color="blue", size=0.1)+ # creates border aesthetics
  coord_map(projection="albers", lat0=30, lat1=45)+ # albers projection
  scale_fill_manual(values=party_colors)+ # fills colors
  labs(title="Election Results 2020", fill=NULL)+ # title
  theme_map()
#
# Print the US parties map
usParties2020
#
# load election2016 data
election2016 <- read.csv("http://chemistry.ncssm.edu/digihum/data/electiondata2016.csv", header=T)
#
# looking at the data
names(election2016)
summary(election2016)
str(election2016)
dim(election2016)
class(election2016)
glimpse(election2016)
head(election2016)
#
# Renaming state column name
names(election2016)[1] <-'state'
#
# Merging state data and election2016 data
election2016$region <- tolower(election2016$state)
us_states_elec2016<- left_join(us_states, election2016)
summary(us_states_elec2016)
#
# Create election2016 map and color by party
usParties2016 <-
  ggplot(data=us_states_elec2016, aes(x=long,y=lat, group=group, fill=party))+ #maps aesthetics
  geom_polygon(color="blue", size=0.1)+ # creates border aesthetics
  coord_map(projection="albers", lat0=30, lat1=45)+ # albers projection
  scale_fill_manual(values=party_colors)+ # fills colors
  labs(title="Election Results 2016", fill=NULL)+ # title
  theme_map()
#
# Print the US parties map
usParties2016
#
# Trump Election Map
trumpElec <-
  ggplot(data=us_states_elec2020,
  mapping=aes(x=long, y=lat, group=group, fill=pct_trump)) + # maps aesthetics
  geom_polygon(color="white", size=0.08) + # creates border aesthetics
  coord_map(projection="albers", lat0=30, lat1=45) + # albers projection
  labs(title="Trump Election Results 2020", fill="Trump \n %") + # title and legend
  theme_map()
#
# Print the Trump Election map
trumpElec
#
# Biden election map
bidenElec <-
  ggplot(data=us_states_elec2020,
         mapping=aes(x=long, y=lat, group=group, fill=pct_biden)) + # maps aesthetics
  geom_polygon(color="white", size=0.08) + # creates border aesthetics
  coord_map(projection="albers", lat0=30, lat1=45) + # albers projection
  labs(title="Biden Election Results 2020", fill="Biden \n %") + # title and legend
  theme_map()
#
# Print the biden election map
bidenElec
#
# arranging the plots on the same page
grid.arrange(usParties2016, usParties2020, trumpElec, bidenElec, ncol=2)
#
# EOF (End of File)
