# Advaith Cheruvu
# September 21th, 2021
# CCDataAnalysis.r
# This script analyzes the cervical cancer data to
# identify patterns in the data
#
# clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
#
# install and load libraries
library(tidyverse)
library(stringr)
source("myfunctions.R")
#
# load clean Cervical cancer data
cervicalCA <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\ccdataMod.csv",header=T, na.strings=c("?"))
#
# looking at the data
names(cervicalCA)
summary(cervicalCA)
str(cervicalCA)
dim(cervicalCA)
class(cervicalCA)
glimpse(cervicalCA)
head(cervicalCA)
tail(cervicalCA)
#
# Basic boxplots
boxplot(cervicalCA$age, horizontal = TRUE, main = "Age Box Plot", xlab = "Age in Years")
quantile(cervicalCA$age)
hist(cervicalCA$rz_transform_age)
# This is included because it tells us that most of 
# the women that were studied were relatively young 
# with some older women as outliers.  
#
boxplot(cervicalCA$sexual_partners, horizontal = TRUE, main = "Sexual Partners Box Plot", xlab = "Number of Sexual Partner")
quantile(cervicalCA$sexual_partners)
#
# This boxplot is included because it tells us that 
# most of the women had a few sexual partners, with a
# few outliers with many sexual partners. Since the 
# main cause of Cervical cancer is HPV (an STD), it
# only makes sense that more sexual partners increases
# the likelihood of transmission.
#
boxplot(cervicalCA$first_intercourse, horizontal = TRUE, main = "First Intercourse Box Plot", xlab = "Age of First Intercourse in Years")
quantile(cervicalCA$first_intercourse)
#
# This box plot is included because it tells us that
# most women in this dataset became sexually active
# relatively early (shortly after puberty), 
# exposing them to stds early on in their lives.
#
boxplot(cervicalCA$pregnancies, horizontal = TRUE, main = "Pregnancies Box Plot", xlab = "Number of Pregnancies")
quantile(cervicalCA$pregnancies)
#
# This box plot is included because it gives us an
# idea of the protection used during sexual intercourse.
# This isn't a perfect measure of protection, but
# the amount of pregnancies as well as the number of
# sexual partners show that these women were generally
# not using much protection. Not using contraceptives is
# a major risk factor for HPV and cervical cancer.
#
# Pairwise correlation plot
pairs(~cervicalCA$age + cervicalCA$sexual_partners + cervicalCA$first_intercourse + cervicalCA$pregnancies, upper.panel = panel.cor, diag.panel = panel.hist)
#
# This plot shows possible correlation between the
# variables age, sexual partners, first intercourse,
# and pregnancies. The correlation coefficients 
# a moderate positive linear relationship between age
# and pregnancies. The coefficients also suggest a
# weak positive linear relationship between age and
# first sexual intercourse. The other coefficients
# don't show defensible relationships. 
#
# Linear regression of age and pregnancies
plot(cervicalCA$age~cervicalCA$pregnancies, 
     main="Pregnancies vs. Age", 
     xlab="Pregnancies", ylab="Age (in years)")
fitline <- lm(cervicalCA$age~cervicalCA$pregnancies)
abline(fitline)
summary(fitline)
#
# BIC modeling for age -> hormonal contraceptives time
BIC(lm(cervicalCA$hormonal_contraceptives_time~1)) # 4644
BIC(lm(cervicalCA$hormonal_contraceptives_time~cervicalCA$age)) # 4570
# Causal
#
# Separating based on a low number of sexual partners
lowSexualPartners <- filter(cervicalCA, sexual_partners <= median(cervicalCA$sexual_partners))
#
# BIC modeling for condylomatosis -> diagnosis num for those
# with low sexual partners
BIC(lm(lowSexualPartners$diagnosis_num~1)) # 248
BIC(lm(lowSexualPartners$diagnosis_num~lowSexualPartners$condylomatosis)) # -149
# Causal
# 
# BIC modeling for hiv -> diagnosis num for those
# with low sexual partners
BIC(lm(lowSexualPartners$diagnosis_num~1)) # 248
BIC(lm(lowSexualPartners$diagnosis_num~lowSexualPartners$hiv)) # 32
#
# Causal, but less than condylomatosis which hints at
# less recognition of hiv than condylomatosis. This
# makes sense because the group of people have less
# sexual partners, making diagnosis less common.
# 
# Separating based on having an std
peopleWithSTDs <- filter(cervicalCA, std == TRUE)
#
# BIC modeling for hpv -> dx_cancer
# for people with stds
BIC(lm(peopleWithSTDs$dx_cancer~1)) # -59
BIC(lm(peopleWithSTDs$dx_cancer~peopleWithSTDs$hpv)) # -5638
# Causal
#
# Pairwise plot of test results from people with stds with hpv.
pairs(~peopleWithSTDs$hpv + peopleWithSTDs$hinselmann + peopleWithSTDs$schiller + peopleWithSTDs$cytology + peopleWithSTDs$biopsy, upper.panel = panel.cor, diag.panel = panel.hist)
# This shows that hpv generally goes unnoticed by these tests.
# Also the tests are correlated with each other (with the
# exception of cytology and hinselmann).
#
# Multifit of the tests based on hpv
multifit <- lm(peopleWithSTDs$hpv ~ peopleWithSTDs$hinselmann + peopleWithSTDs$schiller + peopleWithSTDs$cytology + peopleWithSTDs$biopsy)
summary (multifit)
# 
# The P value of 0.97 shows that the 
# tests are not statistically significant enough
# to prove correlation.
#
# EOF (End of File)