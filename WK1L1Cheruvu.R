# Setting up working directory
getwd()
setwd("/Users/advai/Documents/DSFS")
projectWD <- "/Users/advai/Documents/DSFS"
setwd(projectWD)
# use the MASS library and myfunctions code
library(MASS)
source("myfunctions.R")
#
# Exploratory Data Analysis (EDA commands)
head(cats)
dim(cats)
summary(cats)
#
# Nicer looking summary table
knitr::kable(  summary(cats),  caption = "Table 1:  Summary table of cat anatomy data")
#
#Data munging: renaming columns
colnames(cats) <- c("Gender", "BodyWeight", "HeartWeight")
names(cats)
cats$Gender <- factor(cats$Gender, levels = c("F", "M"), labels = c("Female", "Male"))
head(cats)
#
# Graphical representations
hist(cats$BodyWeight, main="Histogram of Body weight (kg)")
hist(cats$HeartWeight, main="Histogram of Heart weight (g)")
pairs(cats,upper.panel=panel.cor,diag.panel=panel.hist)
#
# Scatter plot of heart weight as a function of body weight with linear regression line
plot(cats$HeartWeight~cats$BodyWeight, main="Simple plot of heart weight \n as a function of body weight", xlab="Body weight (kg)", ylab="Heart weight (g)")
fit<- lm(cats$HeartWeight~cats$BodyWeight)
abline(fit)