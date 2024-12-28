# Analysis for a Central Conposite Design for 2 factors

# Author: Rosane Rech, December 2020.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Design and analysis of experiments / Douglas C. Montgomery. — Eighth edition.
# ISBN 978-1-118-14692-7

# Data file: DoEOpt06.csv
# Time (x1): Time (min)
# Temp (x2): Temperature (ºC)
# Y: Yield (%)
# Visc: Viscosity (mPa s)
# Mw: Molecular Weight (g/mol)


# loading Response Surface Methodology package

library(rsm)

# checking file structure

str(DoEOpt06)

# dsg <- ccd(2, alpha="spherical")
dsg <- ccd(2, alpha="spherical", oneblock = TRUE, n0 = 2,
           randomize = FALSE)
dsg

dsg$Time <- dsg$x1*5 + 85
dsg$Temp <- dsg$x2*5 + 175
dsg

dsg <- as.data.frame(dsg)
write.csv(dsg, "ccd.csv")

# setting the realtionship between the coded and the natural variables

DoEOpt06 <- as.coded.data(DoEOpt06, 
              x1 ~ (Time-85)/5,
              x2 ~ (Temp-175)/5)

###
##
# regression model for the Yield

model_Y <- rsm(Y ~ SO(x1,x2), data = DoEOpt06)

# model_Y <- rsm(Y ~ FO(x1,x2) + TWI(x1,x2) + PQ(x1,x2), data = DoEOpt06)
model_Y <- rsm(Y ~ FO(x1,x2) + PQ(x1,x2), data = DoEOpt06)
summary(model_Y)

# contour and perspective plots

contour(model_Y, ~ x1+x2, 
        image = TRUE, 
        xlabs=c("Time (min)", "Temperature (ºC)"))

persp(model_Y, ~ x1+x2, col = terrain.colors(50), contours = "colors",
      zlab = "Yield (%)", 
      xlabs=c("Time (min)", "Temperature (ºC)"))

# predict the Yield at the stationary point
max <- data.frame(x1 = 0.361, x2 = 0.257)
predict(model_Y, max)

###
##
# regression model for viscosity

model_v <- rsm(Visc ~ FO(x1,x2) + TWI(x1,x2) + PQ(x1,x2), data = DoEOpt06)
summary(model_v)

model_v <- rsm(Visc ~ FO(x1,x2) + PQ(x2), data = DoEOpt06)
summary(model_v)

# contour and perspective plots

contour(model_v, ~x1+x2,
        image = TRUE, 
        xlabs=c("Time (min)", "Temperature (ºC)"))

persp(model_v, x1~x2, col = terrain.colors(50), contours = "colors",
      zlab = "Viscosity (mPa∙s)", 
      xlabs=c("Time (min)", "Temperature (ºC)"))

###
##
# regression model for Mw

model_Mw <- rsm(Mw ~ FO(x1,x2) + TWI(x1,x2) + PQ(x1,x2), data = DoEOpt06)
summary(model_Mw)

model_Mw <- rsm(Mw ~ FO(x1,x2), data = DoEOpt06)
summary(model_Mw)

# contour and perspective plots
contour(model_Mw, ~x1+x2,
        image = TRUE, 
        xlabs=c("Time (min)", "Temperature (ºC)"))

persp(model_Mw, x1~x2, col = terrain.colors(50), contours = "colors",
      zlab = "Molecular Weight (g/mol)", 
      xlabs=c("Time (min)", "Temperature (ºC)"))


###
##
# Contour plots for overlay:

# Yield higher than 78.5 %
contour(model_Y, ~ x1+x2, 
        levels = c(78.5), col = "blue",
        xlabs=c("Time (min)", "Temperature (ºC)"))

# Viscosity between 62 and 68 mPa.s
contour(model_v, ~x1+x2,
        levels = c(62,68), col = "red",
        xlabs=c("Time (min)", "Temperature (ºC)"),
        add = TRUE)

# Molecular weight lower than 3400 g/mol
contour(model_Mw, ~x1+x2,
        levels = c(3400), col = "purple",
        xlabs=c("Time (min)", "Temperature (ºC)"),
        add = TRUE)



### Functions for desirability
# (Derringer and Suich)

# y = response
# L = unacceptability boundary
# T = target acceptability boundary T
# U = upper unacceptability boundary
# r, r1, r2 = exponent 1 for L and U
# d = desirability function

# Maximum
maxD <- function(y, L, T, r) {
  if (y < L){d <- 0} 
  else if (y > T){d <- 1}
  else{d <- ((y - L) / (T - L))^r}
  return(d) }

# Minimum
minD <- function(y, U, T, r) {
  if (y < T){d <- 1}
  else if (y > U){d <- 0}
  else{d <- ((U - y) / (U - T))^r}
  return(d) }

# Target
targetD <- function(y, L, T, U, r1, r2) {
  if (y < L){d <- 0}
  else if ( L<=y & y<=T ){
    d <- ((y - L) / (T - L))^r1}
  else if( T<=y & y<=U ){
    d <- ((U - y) / (U - T))^r2}
  else {
    d <- 0}
  return(d)}

# create a grid for the desirability determination

dataD <- expand.grid(seq(-1.5, 1.5, by=0.05), seq(-1.5, 1.5, by=0.05))
colnames(dataD) <- c("x1", "x2")
View(dataD)

# add the natural variables

dataD$Time <- dataD$x1*5 + 85       
dataD$Temp <- dataD$x2*5 + 175

# add the predicted responses

dataD$Y <- predict(model_Y, newdata = dataD)
dataD$v <- predict(model_v, newdata = dataD)
dataD$Mw <- predict(model_Mw, newdata = dataD)

# For each data value, calculate desirability

for (i in 1:nrow(dataD)) { 
  d1 <- maxD(dataD$Y[i]
             , L = 78.5, T = 81, r = 1)
  d2 <- targetD(dataD$v[i]
                , L = 62, T = 65, U = 68, r1 = 1, r2 = 1)
  d3 <- minD(dataD$Mw[i]
             , U = 3400, T = 3000, r = 1) 
  D <- (d1 * d2 * d3)^(1/3)
  dataD[i, c("d1", "d2", "d3", "D")] <- c(d1, d2, d3, D)
}


# plot the desirability results

library(ggplot2)

ggplot(data = dataD, aes(x=Time, y=Temp, z = D)) +
  geom_contour_filled() +
  scale_fill_brewer() +
  scale_x_continuous(limits=c(78,92), breaks=seq(78,92,2)) +
  scale_y_continuous(limits=c(168, 182), breaks=seq(168,182, 2)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(subtitle="Desirability function")
ggsave("desirability.png", width = 4.1, height = 2.5, dpi = 1000)

###
##
#
# Build a central composite design for 2 factors

library(rsm)

dsg <- ccd(2, alpha = "spherical")
dsg   

# add the natural variables

dsg$Time <- dsg$x1*5 + 85       
dsg$Temp <- dsg$x2*5 + 175
dsg

 # saves the design in a file

dsg <- as.data.frame(dsg)
write.csv(dsg, "design.csv")

