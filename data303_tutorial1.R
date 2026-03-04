#tutorial 1
#problem 1, do some EDA
library(car)
library(dplyr)
library(psych)
library(ggplot2)
library(ape)
library(pander)
#data from car library
data(UN98)
str(UN98)


#perform a summary for the dataset
summarise(UN98)


#select a subset of variables
un2<-dplyr::select(UN98, infantMortality, region, tfr, GDPperCapita, economicActivityFemale, illiteracyFemale)
str(un2)

#each of these numbers in the summaries are percentages
summary(un2)

#1.a carry out EDA for:
#i determine the number and types of variables in the dataset

#ii. Determine whether the variable values seem sensible and whether there are observations with missing values for any of the predictors


#iii. make a scatterplot matrix + boxplot as appropriate to explore the data. Use graphs to identify any considerations when fitting a regressiong model
#have infantMortality as the response variable

#choose spearmen correlation
un2 %>% select(where(is.numeric)) %>%
  pairs.panels(method='spearman', hist.col='lightgreen', density=T, ellipses = F)


#categorical boxplots
un2 %>% select(infantMortality, where(is.factor)) %>%
  ggplot(aes(x=region, y=infantMortality)) + geom_boxplot()

#multi-colinearity should be investigated

#1b.) use lm command to fit a linear model with infantMortality as the response variable and all other variables in un2 as predictors
#write the equation of the fitted model
fit1 <- lm(infantMortality ~ tfr + GDPperCapita + economicActivityFemale
      + illiteracyFemale + region, data=un2)

#summarize the fitted model
#USE PANDER IN ASSIGNMENTS PLEASE THIS IS VERY IMPORTANT
#BECAUSE WE GET THIS REALLY NICE TABLE
pander(summary(fit1), caption='')
#africa is missing because it is the reference level + create dummy variables cause these are categorical variables and u cant do math for them

#to see the matrix for categorical variables
head( model.matrix(fit1) )



