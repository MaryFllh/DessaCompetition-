library(readr)
library(rlang)
library(tidyverse)
library(lubridate)

data <- read_csv("~/Dropbox/School/Grad School/U of T/Courses/Dessa Case Comp/ks-projects-201612.csv")
problems <- problems(data)
errors <- problems[,1] #identify the rows with problems
errors <- unique(errors)  #some rows have multiple problems, find the unique row values
errors <- as.numeric(unlist(t(c(errors) ))) #extract indices from list as numeric
data <- data[-c(errors), ]  #Clean by rows with errors, take the complement of the matrix
data <- data[,c(1:13)]  #Clean by removing empty end columns
View(data)
barplot(prop.table(table(data$main_category)),cex.names = .75,main="Frequency of Kickstart Main Category",ylim=c(0,.20))
barplot(prop.table(table(data$state)),cex.names = .75,main="Distribution of Outcomes",ylim=c(0,.60))


data <-mutate_if(data, is.character, str_replace_all, pattern = "canceled", replacement = "failed") # Treat canceled projects as failed 
data <-mutate_if(data, is.character, str_replace_all, pattern = "suspended", replacement = "failed") # Treat suspended projects as failed
live<-which(data$state == "live") #Remove ongoing projects
data <- data[-c(live), ]  #Clean by rows with errors, take the complement of the matrix
undefined<-which(data$state == "undefined") # Remove projects with an undefined state, these also have incomplete country and backer informations
data <- data[-c(undefined), ]  #Clean by rows with errors, take the complement of the matrix
barplot(prop.table(table(data$state)),cex.names = .75,main="Distribution of Outcomes - Cleaned",ylim=c(0,.60))

data <-mutate_if(data, is.character, str_replace_all, pattern = "failed", replacement = "0") # Treat canceled projects as failed 
data <-mutate_if(data, is.character, str_replace_all, pattern = "successful", replacement = "1") # Treat suspended projects as failed

data$Timeline  <- difftime(data$deadline,data$launched, units = "days") #Calculate duration of campaign
data$TitleLengthChars  <- nchar(data$name, keepNA = FALSE) #Calculate length of campaign names
data$NumberWords  <- sapply(strsplit(data$name, " "), length)  #Calculate number of words in campaign names
data$HourKey  <- hour(data$launched) #launch hour
data$BackerPledge  <- data$'usd pledged'/data$backers #Pledged/Back USD
data$BackerPledge[is.nan(data$BackerPledge)] <- 0
data$BackerPledge[is.na(data$BackerPledge)] <- 0
data$BackerPledge[is.infinite(data$BackerPledge)] <- 0

categories <- unique(data$main_category ) #code main_category variables as number so they can be used in logistic regression
CategoricalKey  <- data.frame(data$main_category)
CategoricalKey <- data.frame(lapply(CategoricalKey, as.character), stringsAsFactors=FALSE)
for (i in 1:length(categories)) {
  CategoricalKey <-mutate_if(CategoricalKey, is.character, str_replace_all, pattern = categories[i], replacement = as.character(which(categories == categories[i])) )  
}
CategoricalKey <-as.numeric(unlist(CategoricalKey))

subcategories <- unique(data$category ) #code category variables as number so they can be used in logistic regression
SubCategoricalKey  <- data.frame(data$category)
SubCategoricalKey <- data.frame(lapply(SubCategoricalKey, as.character), stringsAsFactors=FALSE)
for (i in 1:length(subcategories)) {
  SubCategoricalKey <-mutate_if(SubCategoricalKey, is.character, str_replace_all, pattern = subcategories[i], replacement = as.character(which(subcategories == subcategories[i])) )  
}
SubCategoricalKey <-as.numeric(unlist(SubCategoricalKey))

day<-weekdays(data$launched)  #code day as number so they can be used in logistic regression
daysofweek <- unique(day)
DayKey  <- data.frame(day)
DayKey <- data.frame(lapply(DayKey, as.character), stringsAsFactors=FALSE)
for (i in 1:length(daysofweek)) {
  DayKey <-mutate_if(DayKey, is.character, str_replace_all, pattern = daysofweek[i], replacement = as.character(which(daysofweek == daysofweek[i])) )  
}
DayKey <-as.numeric(unlist(DayKey))

currency <- unique(data$currency ) #code currency variables as number so they can be used in logistic regression
CurrencyKey  <- data.frame(data$currency)
CurrencyKey <- data.frame(lapply(CurrencyKey, as.character), stringsAsFactors=FALSE)
for (i in 1:length(currency)) {
  CurrencyKey <-mutate_if(CurrencyKey, is.character, str_replace_all, pattern = currency[i], replacement = as.character(which(currency == currency[i])) )  
}
CurrencyKey <-as.numeric(unlist(CurrencyKey))

country <- unique(data$country ) #code country variables as number so they can be used in logistic regression
CountryKey  <- data.frame(data$country)
CountryKey <- data.frame(lapply(CountryKey, as.character), stringsAsFactors=FALSE)
for (i in 1:length(country)) {
  CountryKey <-mutate_if(CountryKey, is.character, str_replace_all, pattern = country[i], replacement = as.character(which(country == country[i])) )  
}
CountryKey <-as.numeric(unlist(CountryKey))

CategoryCount <- data.frame(data$main_category) #determine the effect of competition and category count on success
CategoryCount <- data.frame(lapply(CategoryCount, as.character), stringsAsFactors=FALSE)
for (i in 1:length(categories)) {
  CategoryCount <-mutate_if(CategoryCount, is.character, str_replace_all, pattern = categories[i], replacement = as.character(length(which(data$main_category==categories[i]))))  
}
CategoryCount <-as.numeric(unlist(CategoryCount))

GoalVsCategory <- data.frame(data$main_category) # data frame for eventual goal vs category goal
GoalVsCategory <- data.frame(lapply(GoalVsCategory, as.character), stringsAsFactors=FALSE)
for (i in 1:length(categories)) {
  indices<-which(data$main_category == categories[i]) #idenifity indices of category
  temp <- data[c(indices), ]  #create subset of data for category
  goalAverage = mean(temp$goal)
  GoalVsCategory <-mutate_if(GoalVsCategory, is.character, str_replace_all, pattern = categories[i], replacement = as.character(goalAverage))  
}
GoalVsCategory <-as.numeric(unlist(GoalVsCategory))
data$GoalVsCategory  <- data$goal/GoalVsCategory 

BackerPledgeVsCategory <- data.frame(data$main_category) # data frame for eventual goal vs category goal
BackerPledgeVsCategory <- data.frame(lapply(BackerPledgeVsCategory, as.character), stringsAsFactors=FALSE)
for (i in 1:length(categories)) {
  indices<-which(data$main_category == categories[i]) #idenifity indices of category
  temp <- data[c(indices), ]  #create subset of data for category
  backerAverage = mean(temp$BackerPledge)
  BackerPledgeVsCategory <-mutate_if(BackerPledgeVsCategory, is.character, str_replace_all, pattern = categories[i], replacement = as.character(backerAverage))  
}
BackerPledgeVsCategory <-as.numeric(unlist(BackerPledgeVsCategory))
data$BackerPledgeVsCategory  <- data$BackerPledge/BackerPledgeVsCategory 

model <- glm(as.numeric(unlist(t(c(data$state) ))) ~ data$goal+data$Timeline+ data$TitleLengthChars + data$NumberWords + CategoricalKey + DayKey + CurrencyKey + CountryKey + SubCategoricalKey + data$HourKey + data$BackerPledge + CategoryCount + data$GoalVsCategory + data$BackerPledgeVsCategory ,family=binomial(link='logit'))
summary(model)

# Bonferroni Correction In Regression to account for bias due to repeated testing of factors
pvalues = summary(model)$coefficients[,4] 
pvalues = p.adjust(pvalues, "bonferroni", n = length(pvalues))
pvalues  <- data.frame(pvalues)

