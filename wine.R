# Analysing a 2ˆ(8-4) fractional design 

# Author: Rosane Rech, December 2020.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Design and analysis of experiments / Douglas C. Montgomery. — Eighth edition.
# ISBN 978-1-118-14692-7

# Data file: wine.csv
# Variables	                  Low Level (-)	  High Level (+)
# A: Pinot Noir Clone	        Pommard	        Wandenswil
# B: Oak type	                Allier	        Troncais
# C: Age of barrel	          Old	            New
# D: Yeast/skin contact	      Champagne	      Montrachet
# E: Fermentation temperature	Low	            High
# F: Whole cluster	          None	          10 %
# G: Stems	                  None	          All
# H: Barrel Toast	            Light	          Medium

# loading the FrF2 library
library(FrF2)
update.packages("FrF2")

# file structure
str(wine)

# model
model <- lm(Rank ~ (A+B+C+D+E+F+G+H)^2 , data=wine)
summary(model)
alias(model)

# half-normal score of the effects
DanielPlot(model, half = TRUE, autolab = FALSE)

model <- lm(Rank ~ (A+B+D+E+F+G)^2 , data=wine)
summary(model)
alias(model)

model <- lm(Rank ~ (A+B+D+E+F+G)+A:D+B:D+B:G , data=wine)
summary(model)
alias(model)

# main effects and interactions plot
MEPlot(model) 
IAPlot(model)


###
##
# Visualisation of the results

library(dplyr)
library(ggplot2)

# Table for Yeast (D) x Temperature (E) x Stem (G)
dt <- group_by(wine, D, E, G) %>%
  summarise(mean=mean(Rank), sd=sd(Rank), n=n())
View(dt)

dt$Yeast <- as.factor(dt$D)
levels(dt$Yeast) <- c("Champagne", "Montrachet")

dt$Temperature <- as.factor(dt$E)
levels(dt$Temperature) <- c("Low", "High")

dt$Stem <- as.factor(dt$G)
levels(dt$Stem) <- c("None", "All")

ggplot(dt, aes(x=Stem, y=mean, fill=Yeast)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2) +
  facet_grid(Temperature~.~Yeast, labeller = label_both) +
  theme(legend.position = "none") +
  ylab("Rank") +
  theme_bw()

###
##
# Building the next design

dsg <- FrF2(nfactors = 5, resolution = 5)
summary(dsg)
View(dsg)

dsg$Pinot.Noir.Clone <- as.factor(dsg$A)
levels(dsg$Pinot.Noir.Clone) <- c("Pommard", "Wandenswil")

dsg$Oak.Type <- as.factor(dsg$B)
levels(dsg$Oak.Type) <- c("Allier", "Troncais")

dsg$Temperature <- as.factor(dsg$C)
levels(dsg$Temperature) <- c("Low", "High")

dsg$Whole.Cluster <- as.factor(dsg$D)
levels(dsg$Whole.Cluster) <- c("None", "10 %")

dsg$Stem <- as.factor(dsg$E)
levels(dsg$Stem) <- c("None", "All")

write.csv(dsg, "1986_Pinot_Noir_design.csv")

