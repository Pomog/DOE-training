# Analysis for a 2^2 Design with Central Points

# Author: Rosane Rech, January 2021.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Design and analysis of experiments / Douglas C. Montgomery. — Eighth edition.
# ISBN 978-1-118-14692-7

# Data file: DoEOpt04.csv
# Time (x1): Time (min)
# Temp (x2): Temperature (ºC)
# Y: Yield (%)

# loading Response Surface Methodology package

library(rsm)

# file view

str(DoEOpt04)

# setting the relationship between coded and natural variables

DoEOpt04 <- as.coded.data(DoEOpt04, 
              x1 ~ (Time-35)/5,
              x2 ~ (Temp-155)/5)

# regression model with coded variables

model <- rsm(Y ~ FO(x1,x2) + TWI(x1,x2), data = DoEOpt04)
summary(model)

# contour plot

contour(model, ~ x1+x2, 
        image = TRUE, 
        xlabs=c("Time (min)", "Temperature (ºC)"))
points(DoEOpt04$Time,DoEOpt04$Temp)


