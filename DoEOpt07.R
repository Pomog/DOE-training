# Building and Analysis of a Central Composite Design for 3 Factors

# Author: Rosane Rech, February 2021.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Quality Progress "For Starbucks, It's in the Bag”, March 2011, pp. 18-2. 
# http://rube.asq.org/quality-progress/2011/03/design-of-experiments/for-starbucks-its-in-the-bag.html

# Data file: DoEOpt07.csv
# v: plastic viscosity (cP)
# p: calmp pressure (psi)
# g: plate gap (mm)
# Tear: response - tear (0 - 9 rating) low is better
# Leakage: response - leakage (proportion pass) low is better


# loading Response Surface Methodology package
library(rsm)

# calculating the coded variables 

DoEOpt07$x_v <- (DoEOpt07$v - 350)/31
 DoEOpt07$x_p <- (DoEOpt07$p - 180)/6
DoEOpt07$x_g <- (DoEOpt07$g/1.8)

# setting the realtionship between the coded and the natural variables

DoEOpt07 <- as.coded.data(DoEOpt07, 
              x_v ~ (v - 350)/31,
              x_p ~ (p - 180)/6,
              x_g ~ (g/1.8))
str(DoEOpt07)

###
##
# regression model for the tear

model.Tear <- rsm(Tear ~ FO(x_v,x_p,x_g) + TWI(x_v,x_p,x_g) + PQ(x_v,x_p,x_g), data = DoEOpt07)

summary(model.Tear)

# residuals plots

par(mfrow=c(2,2))
plot(DoEOpt07$Tear, model.Tear$residuals, 
     xlab = "Tear (experimental)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt07$v, model.Tear$residuals,
     xlab = "Viscosity (cP)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt07$p, model.Tear$residuals,
     xlab = "Pressure (psi)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt07$g, model.Tear$residuals,
     xlab = "Plate Gap (mm)", ylab = "residuals") + 
  abline(h=0, col="grey")

###
##
# regression model for the leakage

model.Leak <- rsm(Leakage ~ FO(x_v,x_p,x_g) + TWI(x_v,x_p,x_g) + PQ(x_v,x_p,x_g), data = DoEOpt07)

summary(model.Leak)

# residuals plots

par(mfrow=c(2,2))
plot(DoEOpt07$Leakage, model.Leak$residuals, 
     xlab = "Leakage (experimental)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt07$v, model.Leak$residuals,
     xlab = "Viscosity (cP)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt07$p, model.Leak$residuals,
     xlab = "Pressure (psi)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt07$g, model.Leak$residuals,
     xlab = "Plate Gap (mm)", ylab = "residuals") + 
  abline(h=0, col="grey")

###
##
# contour plots of the responses

par(mfrow=c(2,2))

contour(model.Tear, ~x_v+x_p,
        image = TRUE, 
        zlim = c(-2,6),
        xlabs=c("Pressure (psi)","Viscosity (cP)"))
points(DoEOpt07$v, DoEOpt07$p)
title(main = list("(a) Tear", cex = 1, font = 1))

contour(model.Tear, ~x_v+x_g,
        image = TRUE,
        zlim = c(-2,6),
        xlabs=c("Plate Gap (mm)", "Viscosity (cP)"))
points(DoEOpt07$v, DoEOpt07$g)
title(main = list("(b) Tear", cex = 1, font = 1))

contour(model.Tear, ~x_g+x_p,
        image = TRUE,
        zlim = c(-2,6),
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"))
points(DoEOpt07$g, DoEOpt07$p)
title(main = list("(c) Tear", cex = 1, font = 1))

contour(model.Leak, ~x_g+x_p,
        image = TRUE, 
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"))
points(DoEOpt07$g, DoEOpt07$p)
title(main = list("(d) Leakage", cex = 1, font = 1))

###
##
# Contour plots for Tear at Viscosity at 300 cP

par(mfrow=c(1,1))
contour(model.Tear, ~x_g+x_p, at =c(x_v = -1.6129032),
        image=TRUE,
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"))
points(DoEOpt07$g, DoEOpt07$p)

###
##
# Contour plots for overlaping

par(mfrow=c(1,1))
contour(model.Tear, ~x_g+x_p, at =c(x_v = -1.6129032),
        levels = c(0.75), col = "darkolivegreen",
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"))
points(DoEOpt07$g, DoEOpt07$p)

contour(model.Leak, ~x_g+x_p,
        levels = c(0.1), col ="darkorange4",
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"), add = TRUE)
points(DoEOpt07$g, DoEOpt07$p)


###
## 
# Functions for desirability
# (Derringer and Suich, 1980)

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


# models for the responses

model.Tear <- rsm(Tear ~ FO(x_v,x_p,x_g) + TWI(x_v,x_p,x_g) + PQ(x_g), data = DoEOpt07)
model.Leak <- rsm(Leakage ~ FO(x_p,x_g) +  PQ(x_g), data = DoEOpt07)

# create a grid for the desirability determination

dataD <- expand.grid(seq(300, 400, by=10),  # viscosity
                     seq(170, 190, by=1),   # clamp pressure
                     seq(-3, 3, by=0.1))    # plate gap
colnames(dataD) <- c("v", "p", "g")
str(dataD)
View(dataD)

# add the natural/coded variables

dataD$x_v <- (dataD$v - 350)/31
dataD$x_p <- (dataD$p - 180)/6
dataD$x_g <- (dataD$g/1.8)

# add the predicted responses

dataD$Tear <- predict(model.Tear, newdata = dataD)
dataD$Leak <- predict(model.Leak, newdata = dataD)

# For each data value, calculate desirability

for (i in 1:nrow(dataD)) { 
  d1 <- minD(dataD$Tear[i]
             , U = 0.75, T = 0, r = 1)
  d2 <- minD(dataD$Leak[i]
             , U = 0.1, T = 0, r = 1) 
  D <- (d1 * d2)^(1/2)
  dataD[i, c("d1", "d2", "D")] <- c(d1, d2, D)
}

# plot the desirability results
library(ggplot2)
ggplot(data = dataD, aes(x=g, y=p, z = D)) +
  geom_contour_filled(bins =8) +
  scale_fill_brewer() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Plate Gap (mm)", y = "Pressure (psi)") +
  facet_wrap(~v) +
  theme(legend.position = c(0.88, 0.13))
ggsave("desirability_StarBucks.png", width = 10, height = 7, dpi = 1000)

