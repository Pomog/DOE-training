# Regression Analysis for a 2^3 Design with Central Points

# Author: Rosane Rech, December 2020.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Design and analysis of experiments / Douglas C. Montgomery. — Eighth edition.
# ISBN 978-1-118-14692-7

# Data file: DoEOpt02.csv
# T: Temperature (ºC)
# P: Pressure (psi)
# C: Concentration (g/L)
# Y: Yield (%)

# loading Response Surface Methodology package
install.packages("rsm")
library(rsm)

#
##
### Creating the 2ˆ3 design with central points

dsg <- cube(3, n0=4)
dsg

dsg$T <- dsg$x1*20 + 140
dsg$P <- dsg$x2*20 + 60
dsg$C <- dsg$x3*7.5 + 22.5
dsg

dsg <- as.data.frame(dsg)
write.csv(dsg, "dsg.csv")

#
##
### Analysing the 2ˆ3 design with central points

# file view

str(DoEOpt02)

# setting the relationship between coded and natural variables

DoEOpt02 <- as.coded.data(DoEOpt02, 
              xT ~ (T-140)/20,
              xP ~ (P-60)/20,
              xC ~ (C-22.5)/7.5)

# regression model with coded variables

# model <- rsm(Y ~ FO(xT,xP, xC) + TWI(xT,xP, xC), data = DoEOpt02)
model <- rsm(Y ~ FO(xT,xP), data = DoEOpt02)
summary(model)

# contour plot

contour(model, ~ xT+xP, 
        image = TRUE, 
        xlabs=c("Pressure (psig)", "Temperature (ºC)")
        )
points(DoEOpt02$T, DoEOpt02$P)

# regression model with natural variables

nat.model <- lm(Y ~ P+T, data=DoEOpt02)
summary(nat.model)

