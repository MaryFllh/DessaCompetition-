library(readr)
library(rlang)
library(tidyverse)

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

categories <- unique(data$main_category ) #code main_category variables as number so they can be used in logistic regression
CategoricalKey  <- data.frame(data$main_category)
CategoricalKey <- data.frame(lapply(CategoricalKey, as.character), stringsAsFactors=FALSE)
for (i in 1:length(categories)) {
  CategoricalKey <-mutate_if(CategoricalKey, is.character, str_replace_all, pattern = categories[i], replacement = as.character(which(categories == categories[i])) ) # Treat canceled projects as failed 
}

CategoricalKey <-as.numeric(unlist(CategoricalKey))

model <- glm(as.numeric(unlist(t(c(data$state) ))) ~ data$goal+data$Timeline+ data$TitleLengthChars + data$NumberWords + CategoricalKey ,family=binomial(link='logit'))
summary(model)

