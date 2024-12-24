# Regression Analysis for a 2^3 Design with innacurate levels and missing observations

# Author: Rosane Rech, January 2021.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Design and analysis of experiments / Douglas C. Montgomery. — Eighth edition.
# ISBN 978-1-118-14692-7

# Data file: DoEOpt03.csv
# T: Temperature (ºC)
# P: Pressure (psi)
# C: Concentration (g/L)
# Y: Yield (%)

# loading Response Surface Methodology package
library(rsm)

# file structure
str(DoEOpt03)

# correcting the coded variables values

DoEOpt03$xT <- (DoEOpt03$T-140)/20
DoEOpt03$xP <- (DoEOpt03$P-60)/20
DoEOpt03$xC <- (DoEOpt03$C-22.5)/7.5

# setting the relationship between coded and natural variables

DoEOpt03 <- as.coded.data(DoEOpt03, 
              xT ~ (T-140)/20,
              xP ~ (P-60)/20,
              xC ~ (C-22.5)/7.5)

# regression model with coded variables

model <- rsm(Y ~ FO(xT,xP, xC) + TWI(xT,xP, xC), data = DoEOpt03)

model <- rsm(Y ~ FO(xT,xP), data = DoEOpt03)
summary(model)

# residuals plots

plot(DoEOpt03$Y, model$residuals,) +
  abline(h=0, col = "gray75")

plot(DoEOpt03$T, model$residuals,) +
  abline(h=0, col = "gray75")

plot(DoEOpt03$P, model$residuals,) +
  abline(h=0, col = "gray75")

# contour plot

contour(model, ~ xT+xP, 
        image = TRUE, 
        xlabs=c("Pressure (psig)", "Temperature (ºC)"))
points(DoEOpt03$T, DoEOpt03$P)

