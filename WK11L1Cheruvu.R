# Advaith Cheruvu
# November 1st 2021
# WK11L1Cheruvu
# Association Rule Mining of Technology Product Data
# this script conducts ARM (Association Rule Mining) 
# using the A-Priori algorithm for technology related products
#
# clean up and set up
rm(list=ls())
setwd("/Users/advai/Documents/DSFS")
#
# libraries
library(arules)
library(arulesViz)
#
# read in tech data
techData <- read.csv("laptops.csv",  na.strings = "?", header = FALSE)
#
# look at data
class(techData)
head(techData)
names(techData)
summary(techData)
#
# read in tech data as a transactions file
tech <- read.transactions(file="laptops.csv",format = "basket",
                          sep = ",",rm.duplicates = TRUE)
class(tech)
summary(tech)
#
# exploring data
inspect(head(tech))
#
# cross tables 
# look at tables by support, confidence and lift
# look at the entire dataset (since it is relatively small)
ct_count <-crossTable(tech, measure ="count", sort=TRUE)
ct_count
ct_supp <-crossTable(tech, measure="support", sort=TRUE)
ct_supp
ct_lift <-crossTable(tech, measure="lift", sort=TRUE)
ct_lift
# Tablet -> Headset, Headset -> Tablet, Headset -> Laptop
# Laptop -> Headset all have lift values greater than 1
# these items are likely bought in association

#
# sorts
sort(itemFrequency(tech, type="relative"), decreasing=TRUE)
sort(itemFrequency(tech, type="absolute"), decreasing=TRUE)
#
# visualizing purchased products
itemFrequencyPlot(tech,type="relative")
#
# Association Rule Mining (ARM)
# normal workflow
rules <- apriori(tech, parameter = list(supp=0.01, conf=0.50))
#
# inspect rules: there are 47 rules
inspect(rules)
inspectDT(rules)
# rules plot
plot(rules)
#
# Pruning
rules.sorted <- sort(rules, by="support")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix
subset.matrix[lower.tri(subset.matrix, diag =T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm =T) >= 1
redundant
which(redundant)
# most of the 47 rules were redundant (5 were not redundant)
# remove redundant rules
rules.pruned <- rules[!redundant]
inspect(rules.pruned)
#
# sorting rules based on metrics
# sort by conf
rules <- sort(rules.pruned, by="confidence", decreasing = TRUE)
inspect(rules)
inspectDT(rules)

# sort by lift
rules <- sort(rules.pruned, by="lift", decreasing = TRUE)
inspect(rules)
inspectDT(rules)

# detecting specific item shopping patterns

# finding itemsets (LHS/antecedant) which are associated with 
# buying another item (RHS/consequent)

metric.params <- list(supp=0.001,conf=0.5,minlen=2)
rules<-apriori(data=tech, parameter=metric.params,
               appearance=list(default="lhs",rhs="Laptop"),
               control=list(verbose=T))
rules<-sort(rules, decreasing = TRUE,by="confidence")
inspect(rules)
# Headset -> Laptop with supp=0.4 conf=0.6667 lift=1.33
# Headset -> Tablet with supp=0.6 conf=1.0 lift=1.11
# Laptops have a high probability of being bought with Tablets
# Tablets have a high probability of being bought with Tablets

# 
# visualization
plot(rules,method="graph",engine="htmlwidget")
plot(rules,method="graph",engine="interactive")

# EOF (End of File)