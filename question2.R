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
harvest.yield  <- read.csv('winery_datasheet/Harvest yield-Table 1.csv', header=T)
  harvest.yield$Treatment <- factor(harvest.yield$Treatment, levels=c(1:3), labels=c('Herbicide', 'Compost', 'Straw'))
  harvest.yield$EWblock   <- factor(harvest.yield$EWblock, levels=c(1:3), labels=c('Block 1', 'Block 2', 'Block 3'))
  harvest.yield$Aspect    <- factor(harvest.yield$Aspect, levels=c(1:2), labels=c('North', 'South'))
  harvest.yield$Block     <- factor(harvest.yield$Block, levels=c(1:6), labels=c('Block 1, South', 'Block 1, North', 'Block 2, South', 'Block 2, North', 'Block 3, South', 'Block 3, North'))
  colnames(harvest.yield)[which(names(harvest.yield) == "Number.of.bunches.per.vine..2001.")] <- 'Number.of.bunches.per.vine.2001'
  colnames(harvest.yield)[which(names(harvest.yield) == "Number.of.bunches.per.vine..2002.")] <- 'Number.of.bunches.per.vine.2002'
  colnames(harvest.yield)[which(names(harvest.yield) == "Number.of.bunches.per.vine..2003.")] <- 'Number.of.bunches.per.vine.2003'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.bunches.per.vine.in.kg..2001.")] <- 'Weight.of.bunches.per.vine.2001.kg'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.bunches.per.vine.in.kg..2002.")] <- 'Weight.of.bunches.per.vine.2002.kg'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.bunches.per.vine.in.kg..2003.")] <- 'Weight.of.bunches.per.vine.2003.kg'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.material.pruned.per.vine.in.kg..2001.")] <- 'Weight.of.material.pruned.per.vine.2001.kg'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.material.pruned.per.vine.in.kg..2002.")] <- 'Weight.of.material.pruned.per.vine.2002.kg'
  colnames(harvest.yield)[which(names(harvest.yield) == "Weight.of.material.pruned.per.vine.in.kg..2003.")] <- 'Weight.of.material.pruned.per.vine.2003.kg'
  
  write.csv(harvest.yield, file='winery_datasheet/harvest.yield.cleaned.CSV.csv')
  
  plot2.1 <- ggplot(data=harvest.yield) +
              geom_point(aes(x=Number.of.bunches.per.vine.2001, y=Weight.of.bunches.per.vine.2001.kg, group='2001', shape="2001")) +
              geom_point(aes(x=Number.of.bunches.per.vine.2002, y=Weight.of.bunches.per.vine.2002.kg, group='2002', shape="2002")) +
              geom_point(aes(x=Number.of.bunches.per.vine.2003, y=Weight.of.bunches.per.vine.2003.kg, group='2003', shape="2003")) +
              scale_shape_manual("Year",
                                 breaks = c("2001", "2002", "2003"),
                                 values = c(1:3)) +
              theme_bw(base_size = 12) +
              scale_x_continuous(breaks = seq(0, 50, 5), limits=c(0,50), expand=c(0,0)) +
              scale_y_continuous(breaks = seq(1, 7, 1), limits=c(0,7), expand=c(0,0)) +
              labs(
                y="Weight of Bunches per vine (kg)", 
                x="Number of Bunches per vine")
  plot2.1
  ggsave("plot2.1.tiff", height=11.22, width=17, units="cm", dpi=600)