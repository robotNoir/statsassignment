# ------------------------------
# Case Study 1 - Study of Graft Arteries
# Angus Robertson
# 22 July 2017
# Packages: 
# ------------------------------

library(data.table, ggplot2)

# Import and fix graft.arteries data
graft.arteries <- read.csv('artery030510.csv', header=T)

  graft.arteries <- subset(graft.arteries, select = -c(X, X.1, X.2, X.3))
  graft.arteries <- graft.arteries[!apply(is.na(graft.arteries) | data == '', 1, all), ]
  colnames(graft.arteries)[which(names(graft.arteries) == "RA...luminal.narrowing")] <- 'RA.Luminal.narrowing.percent'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA..Intimal.thickness.index")] <- 'RA.Intimal.thickness.index'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA...luminal.narrowing")] <- 'ITA.Luminal.narrowing.percent'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA..Intimal.thickness.index")] <- 'ITA.Intimal.thickness.index'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA.DLI")] <- 'RA.DLI.mm'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA.IEL.area")] <- 'RA.IEL.area.mm2'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA.Intimal.area")] <- 'RA.Intimal.area.mm2'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA.Medial.area")] <- 'RA.Medial.area.mm2'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA.Intimal.width")] <- 'RA.Intimal.width.mm'
  colnames(graft.arteries)[which(names(graft.arteries) == "RA.Medial.width")] <- 'RA.Medial.width.mm'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA.DLI")] <- 'ITA.DLI.mm'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA.IEL.area")] <- 'ITA.IEL.area.mm2'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA.Intimal.area")] <- 'ITA.Intimal.area.mm2'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA.Medial.area")] <- 'ITA.Medial.area.mm2'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA.Intimal.width")] <- 'ITA.Intimal.width.mm'
  colnames(graft.arteries)[which(names(graft.arteries) == "ITA.Medial.width")] <- 'ITA.Medial.width.mm'
  graft.arteries$Age                     <- as.numeric(graft.arteries$Age)
  graft.arteries$Gender                  <- factor(graft.arteries$Gender, levels=c(0:1), labels=c('female', 'male'))
  graft.arteries$Diabetes                <- factor(graft.arteries$Diabetes, levels=c(0:1), labels=c('no', 'yes'))
  graft.arteries$Ever.smoked             <- factor(graft.arteries$Ever.smoked, levels=c(0:1), labels=c('no', 'yes'))
  graft.arteries$PVD                     <- factor(graft.arteries$PVD, levels=c(0:1), labels=c('no', 'yes'))
  graft.arteries$CVD                     <- factor(graft.arteries$CVD, levels=c(0:1), labels=c('no', 'yes'))
  graft.arteries$Hypertension            <- factor(graft.arteries$Hypertension, levels=c(0:1), labels=c('no', 'yes'))
  graft.arteries$RA.medial.calcification <- factor(graft.arteries$RA.medial.calcification, levels=c(0:1), labels=c('no', 'yes'))
  graft.arteries$RA.intimal.abnormality  <- factor(graft.arteries$RA.intimal.abnormality, levels=c(0:2), labels=c('normal', 'intimal thickening', 'atherosclerosis'))
  graft.arteries$ITA.intimal.abnormality <- factor(graft.arteries$ITA.intimal.abnormality, levels=c(0:2), labels=c('normal', 'intimal thickening', 'atherosclerosis'))
  
  write.csv(graft.arteries, 'graft.arteries.cleaned.CSV.csv')
  
# Generate plots for question 1
  plot1.1 <- ggplot(graft.arteries, aes(Ever.smoked,RA.Luminal.narrowing.percent)) + 
    geom_boxplot() + 
    coord_flip() +
    theme_bw(base_size = 8) +
    scale_x_discrete(labels=c("No","Yes")) +
    scale_y_continuous(breaks = seq(0, 70, 10), limits=c(0,70), expand=c(0,0)) +
    labs(
      y="Luminal Narrowing (%)", 
      x="Smoker?")
  plot1.1
  ggsave("plot1.1.tiff", height=4.6, width=17, units="cm", dpi=600)
  
  plot1.2 <- ggplot(graft.arteries, aes(Ever.smoked,RA.Intimal.thickness.index)) + 
    geom_boxplot() + 
    coord_flip() +
    theme_bw(base_size = 8) +
    scale_x_discrete(labels=c("No","Yes")) +
    scale_y_continuous(breaks = seq(0, 2, 0.2), limits=c(0,2), expand=c(0,0)) +
    labs(
      y="Intimal Thickness Index", 
      x="Smoker?")
  plot1.2
  ggsave("plot1.2.tiff", height=4.6, width=17, units="cm", dpi=600)
  
  plot1.3 <- ggplot(graft.arteries, aes(Ever.smoked,RA.Intima.to.media.ratio)) + 
    geom_boxplot() + 
    coord_flip() +
    theme_bw(base_size = 8) +
    scale_x_discrete(labels=c("No","Yes")) +
    scale_y_continuous(breaks = seq(0, 11, 1), limits=c(0,11), expand=c(0,0)) +
    labs(
      y="Intima-to-Media Ratio", 
      x="Smoker?")
  plot1.3
  ggsave("plot1.3.tiff", height=4.6, width=17, units="cm", dpi=600)
  
# Compute t-test's for question 1
  t.test.1.1 <- t.test(graft.arteries$RA.Luminal.narrowing.percent ~ graft.arteries$Ever.smoked, var.equal=T, conf.level=0.95)
  t.test.1.2 <- t.test(graft.arteries$RA.Intimal.thickness.index ~ graft.arteries$Ever.smoked, var.equal=T, conf.level=0.95)
  t.test.1.3 <- t.test(graft.arteries$RA.Intima.to.media.ratio ~ graft.arteries$Ever.smoked, var.equal=T, conf.level=0.95)
  
  
  

  