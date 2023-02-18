# Advaith Cheruvu
# September 20th, 2021
# WK5L1Cheruvu.R
# This script cleans cervical cancer data
#
# Clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
#
# install and load libraries
library(tidyverse)
library(stringr)
source("myfunctions.R")
#
# load Cervical cancer data
cervicalCA <- read.csv(file = "C:\\Users\\advai\\Documents\\DSFS\\cervicalCA.csv",header=T, na.strings=c("?"))
#
# looking at the data
names(cervicalCA)
summary(cervicalCA)
str(cervicalCA)
dim(cervicalCA)
class(cervicalCA)
glimpse(cervicalCA)
head(cervicalCA)
#
# Cleaning up the column names
names(cervicalCA) <- c("age", "sexual_partners","first_intercourse", "pregnancies", "smokes", "smoking_time", "smoke_rate", "hormomal_contraceptives", "hormonal_contraceptives_time", "iud", "iud_time", "std", "std_time", "condylomatosis", "cervical_condylomatosis", "vaginal_condylomatosis", "vulvo_perineal_condylomatosis", "syphilis", "pelvic_inflammatory_disease", "genital_herpes", "molluscum_contagiosm", "aids", "hiv", "hepatitis_b", "hpv", "diagnosis_num", "first_diagnosis_time", "last_diagnosis_time", "dx_cancer", "dx_cin", "dx_hpv", "dx", "hinselmann", "schiller", "cytology", "biopsy")
#
# Check for missing data
clean <- ifelse(complete.cases(cervicalCA) == TRUE, 1, 0)
paste("There are", dim(cervicalCA)[1]-sum(clean), "rows with missing data.")
#
# Find which columns have missing data
missingDataCol <-colnames(cervicalCA)[apply(cervicalCA, 2, anyNA)]
paste0("The following columns have missing data: ", missingDataCol)
#
# Replacing the missing data with the mean for 
# appropriate columns
cervicalCA$sexual_partners <- ifelse(is.na(cervicalCA$sexual_partners), round(mean(cervicalCA$sexual_partners, na.rm=TRUE),0), cervicalCA$sexual_partners)
cervicalCA$first_intercourse <- ifelse(is.na(cervicalCA$first_intercourse), round(mean(cervicalCA$first_intercourse, na.rm=TRUE),0), cervicalCA$first_intercourse)
cervicalCA$pregnancies <- ifelse(is.na(cervicalCA$pregnancies), round(mean(cervicalCA$pregnancies, na.rm=TRUE),0), cervicalCA$pregnancies)
#
# Replacing the other missing data with 0
cervicalCA[is.na(cervicalCA)] = 0
#
# Replacing integer data as boolean where appropriate
logicals <- c("smokes", "iud", "std", "condylomatosis", "vaginal_condylomatosis", "vulvo_perineal_condylomatosis", "syphilis", "pelvic_inflammatory_disease", "genital_herpes", "molluscum_contagiosm", "aids", "hiv", "hepatitis_b", "hpv", "dx_cancer", "dx_cin", "dx_hpv", "dx", "hinselmann", "schiller", "cytology", "biopsy")
cervicalCA[logicals] <- lapply(cervicalCA[logicals], as.logical)
#
# Round values in columns with many digits
cervicalCA$smoking_time <- round(cervicalCA$smoking_time, 3)
cervicalCA$smoke_rate <- round(cervicalCA$smoke_rate, 3)
cervicalCA$hormonal_contraceptives_time <- round(cervicalCA$hormonal_contraceptives_time,3)
#
# Normal distribution of numerical data
# Checking for age
hist(rz.transform(cervicalCA$age))
qqnorm(cervicalCA$age)
qqline(cervicalCA$age)
cervicalCA$rz_transform_age <- rz.transform(cervicalCA$age)
#
# Checking for sexual_partners
hist(rz.transform(cervicalCA$sexual_partners))
qqnorm(cervicalCA$sexual_partners)
qqline(cervicalCA$sexual_partners)
cervicalCA$rz_transform_sexual_partners <- rz.transform(cervicalCA$sexual_partners)
#
# Checking for first_intercourse
hist(rz.transform(cervicalCA$first_intercourse))
qqnorm(cervicalCA$first_intercourse)
qqline(cervicalCA$first_intercourse)
cervicalCA$rz_transform_first_intercourse <- rz.transform(cervicalCA$first_intercourse)
#
# Checking for pregnancies
hist(rz.transform(cervicalCA$pregnancies))
qqnorm(cervicalCA$pregnancies)
qqline(cervicalCA$pregnancies)
cervicalCA$rz_transform_pregnancies <- rz.transform(cervicalCA$pregnancies)
#
# Checking for smoking_time
hist(rz.transform(cervicalCA$smoking_time))
qqnorm(cervicalCA$smoking_time)
qqline(cervicalCA$smoking_time)
cervicalCA$rz_transform_smoking_time <- rz.transform(cervicalCA$smoking_time)
#
# Checking for smoke_rate
hist(rz.transform(cervicalCA$smoke_rate))
qqnorm(cervicalCA$smoke_rate)
qqline(cervicalCA$smoke_rate)
cervicalCA$rz_transform_smoke_rate <- rz.transform(cervicalCA$smoke_rate)
#
# Checking for hormonal_contraceptives_time
hist(rz.transform(cervicalCA$hormonal_contraceptives_time))
qqnorm(cervicalCA$hormonal_contraceptives_time)
qqline(cervicalCA$hormonal_contraceptives_time)
cervicalCA$rz_transform_hormonal_contraceptives_time <- rz.transform(cervicalCA$hormonal_contraceptives_time)
#
# Checking for iud
hist(rz.transform(cervicalCA$iud))
qqnorm(cervicalCA$iud)
qqline(cervicalCA$iud)
cervicalCA$rz_transform_iud <- rz.transform(cervicalCA$iud)
#
# Checking for std
hist(rz.transform(cervicalCA$std))
qqnorm(cervicalCA$std)
qqline(cervicalCA$std)
cervicalCA$rz_transform_std <- rz.transform(cervicalCA$std)
#
# Checking for diagnosis_num
hist(rz.transform(cervicalCA$diagnosis_num))
qqnorm(cervicalCA$diagnosis_num)
qqline(cervicalCA$diagnosis_num)
cervicalCA$rz_transform_diagnosis_num <- rz.transform(cervicalCA$diagnosis_num)
#
# Checking for first_diagnosis_time
hist(rz.transform(cervicalCA$first_diagnosis_time))
qqnorm(cervicalCA$first_diagnosis_time)
qqline(cervicalCA$first_diagnosis_time)
cervicalCA$rz_transform_first_diagnosis_time <- rz.transform(cervicalCA$first_diagnosis_time)
#
# Checking for last_diagnosis_time
hist(rz.transform(cervicalCA$last_diagnosis_time))
qqnorm(cervicalCA$last_diagnosis_time)
qqline(cervicalCA$last_diagnosis_time)
cervicalCA$rz_transform_last_diagnosis_time <- rz.transform(cervicalCA$last_diagnosis_time)
#
# Writing export file
write.csv(cervicalCA,"/Users/advai/Documents/DSFS/ccdataMod.csv")
#
# EOF (End of File)