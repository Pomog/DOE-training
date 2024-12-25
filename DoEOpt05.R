# Regression Analysis for a 2^2 Design

# Author: Rosane Rech, January 2021.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Design and analysis of experiments / Douglas C. Montgomery. — Eighth edition.
# ISBN 978-1-118-14692-7

# Data file: DoEOpt05.csv
# Time (x1): Time (min)
# Temp (x2): Temperature (ºC)
# Y: Yield (%)

# loading Response Surface Methodology package

library(rsm)

# file view

str(DoEOpt05)

DoEOpt05 <- as.coded.data(DoEOpt05, 
              x1 ~ (Time-85)/5,
              x2 ~ (Temp-175)/5)

# regression model with coded variables

model <- rsm(Y ~ FO(x1,x2) + TWI(x1,x2), data = DoEOpt05)
summary(model)

# plots

plot(DoEOpt05$Time, DoEOpt05$Y, xlab = "Time", ylab = "Yield (%)")
plot(DoEOpt05$Temp, DoEOpt05$Y, xlab = "Temperature", ylab = "Yield (%)")

contour(model, ~ x1+x2, 
        image = TRUE, 
        xlabs=c("Time (min)", "Temperature (ºC)"))
points(DoEOpt05$Time,DoEOpt05$Temp)


