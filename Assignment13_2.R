1. Use the given link Data Set.
   Answer the below questions:
a. Visualize the correlation between all variables in a meaningful and clear way of representing. Find out
   top 3 reasons for having more crime in a city.
b. What is the difference between co-variance and correlation? Take an example from this dataset and
   show the differences if any?
     
     setwd("F:/AcadGild/workings")
   library(readr)
   library(Hmisc)
   library(dplyr)
   library(MASS)
   library(ggplot2)
   library(lattice)
   library(car)
   library(caret)
   library(rpart)
   library(randomForest)
   library(rpart)
   library(rpart.plot)
   library(RColorBrewer)
   library(rattle)
   library(mice)
   library(VIM)
   
   df<-read.csv("F:/AcadGild/workings/COBRA-YTD2017-A13.csv")
   
   summary(df)
   str(df)
   dim(df)
   describe(df)
   head(df)
   View(df)
   sapply(df,class)
   
   # missing values imputation with  with median  
   sapply(df, function(x) sum(is.na(x))) # missing values  
   df$loc_type[is.na(df$loc_type)] <- median(df$loc_type, na.rm=TRUE)
   df$MaxOfnum_victims[is.na(df$MaxOfnum_victims)] <- median(df$MaxOfnum_victims, na.rm=TRUE)
   md.pattern(df)
   
   # correlation scatterplot for all the variables 
   plot(df$beat, df$MaxOfnum_victims)
   plot(df$beat, df$MinOfucr)
   plot(df$beat, df$y)
   
   cor(df[,c(8,12,15,22,23)], method = "spearman")

# covariance test

cov(df[,c(8,12,15,22,23)],method = "spearman")

cov(df$MaxOfnum_victims, df$y)
cov(df$MaxOfnum_victims, df$beat)
cov(df$beat, df$y)
cov(df$MinOfucr, df$beat)

model2<-lm(beat~ MinOfucr+MaxOfnum_victims+x+y, data=df)
model2

summary(model2)

abline(model2)

plot(model2)

par(mfrow=c(2,2))

plot(model2)



