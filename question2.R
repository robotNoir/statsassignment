# ------------------------------
# Case Study 2 - Growing Better Pinot Noir
# Angus Robertson
# 22 July 2017
# Packages: 
# ------------------------------

# Import and fix trunk diameter data
trunk.diameter <- read.csv('winery_datasheet/Trunk diameter-Table 1.csv', header=T)

  trunk.diameter$Treatment <- factor(trunk.diameter$Treatment, levels=c(1:3), labels=c('Herbicide', 'Compost', 'Straw'))
  trunk.diameter$EWblock   <- factor(trunk.diameter$EWblock, levels=c(1:3), labels=c('Block 1', 'Block 2', 'Block 3'))
  trunk.diameter$Aspect    <- factor(trunk.diameter$Aspect, levels=c(1:2), labels=c('North', 'South'))
  trunk.diameter$Block     <- factor(trunk.diameter$Block, levels=c(1:6), labels=c('Block 1, South', 'Block 1, North', 'Block 2, South', 'Block 2, North', 'Block 3, South', 'Block 3, North'))
  trunk.diameter$Row       <- as.numeric()
  trunk.diameter$Panel     <- as.numeric()
  trunk.diameter$Vine      <- as.numeric()
  colnames(trunk.diameter)[which(names(trunk.diameter) == "Trunk.diameter.in.2000..cm.")] <- 'Trunk.diameter.2000.cm'
  colnames(trunk.diameter)[which(names(trunk.diameter) == "Trunk.diameter.in.2001..cm.")] <- 'Trunk.diameter.2001.cm'
  colnames(trunk.diameter)[which(names(trunk.diameter) == "Trunk.diameter.in.2002..cm.")] <- 'Trunk.diameter.2002.cm'
  colnames(trunk.diameter)[which(names(trunk.diameter) == "Trunk.diameter.in.2003..cm.")] <- 'Trunk.diameter.2003.cm'
  
  write.csv(trunk.diameter, file='winery_datasheet/trunk.diameter.cleaned.CSV.csv')

# Import and fix harvest yield data  
harvest.yield <- read.csv('winery_datasheet/harvest.yield.corrected.csv', header=T)
  harvest.yield$Treatment <- factor(harvest.yield$Treatment, levels=c(1:3), labels=c('Herbicide', 'Compost', 'Straw'))
  harvest.yield$EWblock   <- factor(harvest.yield$EWblock, levels=c(1:3), labels=c('Block 1', 'Block 2', 'Block 3'))
  harvest.yield$Aspect    <- factor(harvest.yield$Aspect, levels=c(1:2), labels=c('North', 'South'))
  harvest.yield$Block     <- factor(harvest.yield$Block, levels=c(1:6), labels=c('Block 1, South', 'Block 1, North', 'Block 2, South', 'Block 2, North', 'Block 3, South', 'Block 3, North'))
  harvest.yield$Year      <- as.factor(harvest.yield$Year)
  colnames(harvest.yield)[which(names(harvest.yield) == "Number.of.bunches.per.vine..2001.")] <- 'Number.of.bunches.per.vine'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.bunches.per.vine.in.kg..2001.")] <- 'Weight.of.bunches.per.vine.kg'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.material.pruned.per.vine.in.kg..2001.")] <- 'Weight.of.material.pruned.per.vine.kg'
  
  write.csv(harvest.yield, file='winery_datasheet/harvest.yield.cleaned.CSV.csv')
  
# Graph to show relationship between average number of bunches harvested and average weight harvested for 2002
  plot2.1 <- ggplot(data=subset(harvest.yield, Year == "2002"), aes(x=Number.of.bunches.per.vine, y=Weight.of.bunches.per.vine.kg)) +
              geom_point(shape=1) +
              theme_bw(base_size = 12) +
              scale_x_continuous(breaks = seq(0, 50, 5), limits=c(0,50), expand=c(0,0)) +
              scale_y_continuous(breaks = seq(0, 3, 0.5), limits=c(0,3), expand=c(0,0)) +
              labs(
                y="Weight of Bunches per vine (kg)", 
                x="Number of Bunches per vine")
  plot2.1
  ggsave("plot2.1.tiff", height=11.22, width=17, units="cm", dpi=600)
  
  model2.1 <- lm(formula = Weight.of.bunches.per.vine.kg ~ Number.of.bunches.per.vine, data=subset(harvest.yield, Year == "2002"))
  summary(model2.1)
  confint(model2.1, level=0.95)
  
  model2.2 <- lm(formula = Weight.of.bunches.per.vine.kg ~ Number.of.bunches.per.vine*Year, data=harvest.yield)
  summary(model2.2)
  
  ggplot(data=harvest.yield, aes(x=Number.of.bunches.per.vine, y=Weight.of.bunches.per.vine.kg, group=Year)) +
    geom_point(aes(shape=as.factor(Year))) +
    scale_shape_manual(values=c(1:3)) +
    theme_bw(base_size = 12) +
    scale_x_continuous(breaks = seq(0, 50, 5), limits=c(0,50), expand=c(0,0)) +
    scale_y_continuous(breaks = seq(0, 7, 1), limits=c(0,3), expand=c(0,0)) +
    labs(
      y="Weight of Bunches per vine (kg)", 
      x="Number of Bunches per vine")
  ggsave("plot2.2.tiff", height=11.22, width=17, units="cm", dpi=600)