# Case Study 1 - Study of Graft Arteries
# Angus Robertson
# 22 July 2017
# Packages: data.table, 

library(data.table)

# Import data file from csv and clean data
data <- read.csv('artery030510.csv', header=T)

  #Remove unused columns and rows
  data <- subset(data, select = -c(X, X.1, X.2, X.3))
  data <- data[!apply(is.na(data) | data == '', 1, all), ]
  
  #Fix column names
  setnames(data, old=c('RA.intimal.abnormality'), new=c('Radial Artery Intimal Abnormality'))
  
  #Fix data types
  data$Age <- as.numeric(data$Age)
  data$Gender <- factor(c(0, 1), lavels=c('female', 'male'))
  data$Diabetes <- factor(c(0, 1), lavels=c('no', 'yes'))
  data$Ever.smoked <- factor(c(0, 1), lavels=c('no', 'yes'))
  data$PVD <- factor(c(0, 1), lavels=c('no', 'yes'))
  data$CVD <- factor(c(0, 1), lavels=c('no', 'yes'))
  data$Hypertension <- factor(c(0, 1), lavels=c('no', 'yes'))
  data$PVD <- factor(c(0, 1), lavels=c('no', 'yes'))