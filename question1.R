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
  setnames(data, old=c("RA...luminal.narrowing",         
                       "RA..Intimal.thickness.index",
                       "RA.Intima.to.media.ratio",     
                       "ITA...luminal.narrowing",        
                       "ITA..Intimal.thickness.index",    
                       "ITA.Intima.to.media.ratio",    
                       "RA.DLI",       
                       "RA.IEL.area",                      
                       "RA.Intimal.area",                  
                       "RA.Medial.area",                   
                       "RA.Intimal.width",                 
                       "RA.Medial.width",                  
                       "ITA.DLI",                      
                       "ITA.IEL.area",                     
                       "ITA.Intimal.area",                 
                       "ITA.Medial.area",                
                       "ITA.Intimal.width",                
                       "ITA.Medial.width"), 
                new=c("RA.Luminal.narrowing.(%)",
                      "RA.Intimal.thickness.index",
                      "RA.Intima-to-media.ratio",
                      "ITA.Luminal.narrowing.(%)",
                      "ITA.Intimal.thickness.index",
                      "ITA.Initma-to-media.ratio",
                      "RA.DLI.(mm)",
                      "RA.IEL.area.(mm^2)",
                      "RA.Intimal.area.(mm^2)",
                      "RA.Medial.area.(mm^2)",                   
                      "RA.Intimal.width.(mm)",                 
                      "RA.Medial.width.(mm)",                  
                      "ITA.DLI.(mm)",                      
                      "ITA.IEL.area.(mm^2)",                     
                      "ITA.Intimal.area.(mm^2)",                 
                      "ITA.Medial.area.(mm^2)",                
                      "ITA.Intimal.width.(mm)",                
                      "ITA.Medial.width.(mm)"
                      ))
  
  #Fix data types
  data$Age <- as.numeric(data$Age)

  data$Gender <- as.factor(data$Gender)
  data$Diabetes <- as.factor(data$Diabetes)
  data$Ever.smoked <- as.factor(data$Ever.smoked)
  data$PVD <- as.factor(data$PVD)
  data$CVD <- as.factor(data$CVD)
  data$Hypertension <- as.factor(data$Hypertension)
  data$RA.medial.calcification <- as.factor(data$RA.medial.calcification)
  data$ITA.intimal.abnormality <- as.factor(data$ITA.intimal.abnormality)
  data$RA.intimal.abnormality <- as.factor(data$RA.intimal.abnormality)
  
  levels(data$Gender) <- c('female', 'male')
  levels(data$Diabetes) <- c('no', 'yes')
  levels(data$Ever.smoked) <- c('no', 'yes')
  levels(data$PVD) <- c('no', 'yes')
  levels(data$CVD) <- c('no', 'yes')
  levels(data$Hypertension) <- c('no', 'yes')
  levels(data$RA.medial.calcification) <- c('no', 'yes')
  levels(data$RA.intimal.abnormality) <- c('normal', 'intimal thickening', 'atherosclerosis')
  levels(data$ITA.intimal.abnormality) <- c('normal', 'intimal thickening', 'atherosclerosis')
  
  
  # part (b) - boxplot display of RA luminal narrowing, intimal thickness and intima-media ratio depending on ever smoked
  par(mfrow=c(3,1))
  boxplot(data$"RA.Luminal.narrowing.(%)" ~ data$Ever.smoked, 
          data=data,
          main="Radial Artery Luminal Narrowing for Smokers vs. Non-Smokers",
          ylab="Smoker",
          xlab="Radial Artery Luminal Narrowing (%)",
          col="grey",
          ylim = c(0, 70),
          pars = list(xaxp = c(0, 70, 14)),
          horizontal=T)
  grid(nx = NULL, ny= NA)
  boxplot(data$RA.Intimal.thickness.index ~ data$Ever.smoked, 
          data=data,
          main="Radial Artery Intimal Thickness Index for Smokers vs. Non-Smokers",
          ylab="Smoker",
          xlab="Radial Artery Intimal Thickness Index",
          col="grey",
          ylim = c(0, 2),
          pars = list(xaxp = c(0, 2, 10)),
          horizontal=T)
  grid(nx = NULL, ny= NA)
  boxplot(data$"RA.Intima-to-media.ratio" ~ data$Ever.smoked, 
          data=data,
          main="Radial Artery Intima-to-media Ratio for Smokers vs. Non-Smokers",
          ylab="Smoker",
          xlab="Radial Artery Intima-to-media Ratio",
          col="grey",
          ylim = c(0, 12),
          pars = list(xaxp = c(0, 12, 12)),
          horizontal=T)
  grid(nx = NULL, ny= NA)
  par(mfrow=c(1,1))
  
  # part (c) - boxplot display of 
  t.test(data$"RA.Luminal.narrowing.(%)" ~ data$Ever.smoked, var.equal=T, conf.level=0.95)
  t.test(data$RA.Intimal.thickness.index ~ data$Ever.smoked, var.equal=T, conf.level=0.95)
  t.test(data$"RA.Intima-to-media.ratio" ~ data$Ever.smoked, var.equal=T, conf.level=0.95)