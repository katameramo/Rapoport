# Distribution and traits in W-Palearctic bats
library(tidyverse) # general data processing toolbox
library(nlme) # linear modeling
library(PerformanceAnalytics)
library(ggpubr)


setwd("")
data <- read_csv("traits_data_updated.csv") %>%
  filter(is.na(family) == FALSE, hibernation.breadth != 0)  # discard empty rows (data not entirely clean for some reason)
geodata <- read_csv("geodata.csv") %>%
  rename(species = binomial)

data <- left_join(data, geodata, by = "species")
data <- dplyr::select(data, tree.sp, distr_area, lat_ave, hibernation.breadth, mobility) #select relevant variables

data <- mutate(data, mobility.num = NA, distr_area.sqrt = sqrt(distr_area))
data$mobility <- factor(data$mobility, levels = c("low", "medium", "high"))
data$mobility.num[data$mobility == "low"] <- 1
data$mobility.num[data$mobility == "medium"] <- 2
data$mobility.num[data$mobility == "high"] <- 3

#head(data)

chart.Correlation(dplyr::select(data, distr_area, lat_ave, hibernation.breadth, mobility.num))

#### corr plot ####
a1 <- ggplot(data = data) +
  geom_point(aes(distr_area, lat_ave)) +
  scale_x_continuous(name = 'distr_area size (1000 km2)', breaks = c(0,5000000,10000000), labels = c(0,5000,10000))+
  scale_y_continuous(name = 'latitude (°N)')+
  stat_smooth(aes(distr_area, lat_ave), span = 2) +
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))
cor(data$distr_area, data$lat_ave)
a2 <- ggplot(data = data) +
  geom_point(aes(distr_area, hibernation.breadth)) +
  scale_x_continuous(name = 'range size (1000 km2)', breaks = c(0,5000000,10000000), labels = c(0,5000,10000))+
  scale_y_continuous(name = 'hibernation breadth', breaks = c(1,2,3), labels = c("1","2","3"))+
  stat_smooth(aes(distr_area, hibernation.breadth), span = 2)+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))
cor(data$distr_area, data$hibernation.breadth)

a3 <- ggplot(data = data) +
  geom_boxplot(aes(mobility, distr_area)) +
  scale_y_continuous(name = 'range size (1000 km2)', breaks = c(0,5000000,10000000), labels = c(0,5000,10000))+
  scale_x_discrete(name = "mobility", labels = c("low", "medium", "high")) +
  coord_flip()+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))


b2 <- ggplot(data=data)+
  geom_point(aes(lat_ave, hibernation.breadth)) +
  scale_x_continuous(name = 'latitude (°N)')+
  scale_y_continuous(name = 'hibernation breadth', breaks = c(1,2,3), labels = c("1","2","3"))+
  stat_smooth(aes(lat_ave, hibernation.breadth), span = 2)+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))
cor(data$lat_ave, data$hibernation.breadth)

b3 <- ggplot(data = data) +
  geom_boxplot(aes(mobility, lat_ave)) +
  scale_y_continuous(name = 'latitude (°N)')+
  scale_x_discrete(name = "mobility", labels = c("low", "medium", "high")) +
  coord_flip()+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))


c3 <- ggplot(data=data)+
  geom_jitter(aes(mobility, hibernation.breadth), width = 0.2, height = 0.2) +
  scale_y_continuous(name = 'hibernation breadth', breaks = c(1,2,3), labels = c("1","2","3"))+
  scale_x_discrete(name = "mobility", labels = c("low", "medium", "high")) +
  coord_flip()+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))


ggarrange(a1, NA, NA, a2, b2, NA, a3, b3, c3, ncol = 3, nrow = 3, labels = c("a", NA, NA, "b", "c", NA, "d", "e", "f"))
ggarrange(a1, NA, NA, a2, b2, NA, a3, b3, c3, ncol = 3, nrow = 2)



#Rapoport
fit1 <- lm(distr_area ~ lat_ave, data = data)
summary(fit1)

#dispersal
fit2 <- lm(distr_area ~ mobility, data = data) 
summary(fit2)

#hibernation
fit3 <- lm(distr_area ~ hibernation.breadth, data = data)
summary(fit3)

#All together
fit4 <- lm(distr_area ~ lat_ave + hibernation.breadth + mobility, data = data)
summary(fit4)

fun1 <- function(x){
  return(fit1$coefficients[1] + fit1$coefficients[2]*x)
}
fun2 <- function(x){
  return(fit2$coefficients[1] + fit2$coefficients[2]*x)
}
fun3 <- function(x){
  return(fit3$coefficients[1] + fit3$coefficients[2]*x)
}

a <- ggplot(data = data) +
  geom_point(aes(lat_ave, distr_area)) +
  scale_y_continuous(name = "range size (1000 km2)", breaks = c(0,5000000,10000000),labels = c("0", "5000", "10000"))+
  scale_x_continuous(name = "latitude")+
  stat_function(fun = fun1) +
  coord_cartesian(ylim = c(0,14000000))+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA))

c <- ggplot(data = data) +
  geom_point(aes(hibernation.breadth, distr_area))+
  stat_function(fun = fun3) +
  scale_y_continuous(name = "range size (1000 km2)", breaks = c(0,5000000,10000000),labels = c("0", "4000", "8000"))+
  scale_x_continuous(name = "hibernation breadth", breaks = c(1,2,3))+
  coord_cartesian(ylim = c(0,14000000), xlim = c(0.5,3.5))+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA),
                         panel.grid.minor.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank())

b <- ggplot(data = data) +
  geom_boxplot(aes(mobility, distr_area, fill = mobility))+
  scale_y_continuous(name = "range size (1000 km2)", breaks = c(0,5000000,10000000),labels = c("0", "4000", "8000"))+
  scale_x_discrete(name = "mobility", labels = c("low", "medium", "high"))+
  scale_fill_manual(values = c("white", "orchid1", "skyblue"))+
  #scale_fill_viridis_d(option = "C", end = 0.7) +
  coord_cartesian(ylim = c(0,14000000))+
  theme_minimal()+ theme(panel.border = element_rect(color = "gray", size = 1, fill = NA),
                         legend.position = "none",
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank())


ggarrange(a,c,b, nrow = 1, ncol = 3, widths = c(1,0.83,0.83))



######## WITH PHYLOGENY #########

library(ape) # phylogenetics

# Read in and process the phylogeny and data
data <- read_csv("table_1.csv")

phylo <- read.nexus("Small_phylogeny.nex") #read in MultiPhylo (1000 trees) of Mammal species
tree <- phylo[[1]] #save the first tree into another variable
species <- intersect(data$tree.sp, tree$tip.label) #create a list of species present in both data and phylogeny
tree <- drop.tip(tree, setdiff(tree$tip.label, data$tree.sp)) #drop extra tips from the tree
phy_data <- data[data$tree.sp %in% species, ] #drop extra rows from the dataset
phy_data <- phy_data[order(match(phy_data$tree.sp,tree$tip.label)),] #order dataset to match the order of the tree tips

# Phylogenetic signal is pretty low for these models. Let's compare free floating lambda and fixed lambda=0 models in therms of AIC.

# Is phylogenetic gls better? 
# This function prints the model with higher AIC and prints residual plots for comparison
compare.fits <- function(fit1, fit2){
  par(mfrow = c(2,1))
  if (AIC(fit1) < AIC(fit2)) {
    print("with phylogeny")
    print(summary(fit1))
    plot(residuals(fit1), main = "floating lambda (better AIC)")
    plot(residuals(fit2), main = "lambda = 0 (worse AIC)")
  }
  if (AIC(fit1) > AIC(fit2)) {
    print("without phylogeny")
    print(summary(fit2))
    plot(residuals(fit2), main = "lambda = 0 (better AIC)")
    plot(residuals(fit1), main = "floating lambda (worse AIC)")
  }
}
lambdas <- c()

#Rapoport
fit1 <- gls(distr_area ~ lat_ave, correlation = corPagel(0.2, phy = tree, fixed = FALSE), data = phy_data)
lambdas["rapoport"] <- fit1$modelStruct$corStruct
fit2 <- gls(distr_area ~ lat_ave, correlation = corPagel(0, phy = tree, fixed = TRUE), data = phy_data)
compare.fits(fit1,fit2)

#dispersal
fit1 <- gls(distr_area ~ mobility, correlation = corPagel(0.2, phy = tree, fixed = FALSE), data = phy_data) 
lambdas["hanski"] <- fit1$modelStruct$corStruct
fit2 <- gls(distr_area ~ mobility, correlation = corPagel(0, phy = tree, fixed = TRUE), data = phy_data) 
compare.fits(fit1,fit2)

#hibernation
fit1 <- gls(distr_area ~ hibernation.breadth, correlation = corPagel(0.2, phy = tree, fixed = FALSE), data = phy_data)
lambdas["brown"] <- fit1$modelStruct$corStruct
fit2 <- gls(distr_area ~ hibernation.breadth, correlation = corPagel(0, phy = tree, fixed = TRUE), data = phy_data)
compare.fits(fit1,fit2)

#All together
fit1 <- gls(distr_area ~ lat_ave + hibernation.breadth + mobility, correlation = corPagel(0.2, phy = tree, fixed = FALSE), data = phy_data)
lambdas["combination"] <- fit1$modelStruct$corStruct
fit2 <- gls(distr_area ~ lat_ave + hibernation.breadth + mobility, correlation = corPagel(0, phy = tree, fixed = TRUE), data = phy_data)
compare.fits(fit1,fit2)

lambdas


