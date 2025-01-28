# Building and Analysis of a Box-Behnken Design for 3 Factors

# Author: Rosane Rech, February 2021.
# This code is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
# https://creativecommons.org/licenses/by-nc-sa/4.0/ 

# Data source:
# Zhang et al., Scientific Reports (2011) 11: 1167
# Identification of VOCs in essential oils extracted using ultrasound and microwave‑assisted methods from sweet cherry flower
# https://doi.org/10.1038/s41598-020-80891-0

# Data file: DoEOpt08.csv
# R: liquid-to-solid ratio (ml/g)
# t: extraction time (min)
# P: microwave power (W)
# Y: oil yield (%)


# loading Response Surface Methodology package
library(rsm)

# setting the relationship between the coded and the natural variables

DoEOpt08 <- as.coded.data(DoEOpt08, 
              x_R ~ (R - 50)/10,
              x_t ~ (t - 25)/5,
              x_P ~ (P - 400)/100)
str(DoEOpt08)

###
##
# regression model

# model <- rsm(Y ~ FO(x_R,x_t,x_P) + TWI(x_R,x_t,x_P) + PQ(x_R,x_t,x_P), data = DoEOpt08)
model <- rsm(Y ~ FO(x_R,x_t,x_P) + PQ(x_R,x_t,x_P), data = DoEOpt08)
summary(model)


# residuals plots

par(mfrow=c(2,2))
plot(DoEOpt08$Y, model$residuals, 
     xlab = "Yield (experimental)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt08$R, model$residuals,
     xlab = "liquid-to-solid ratio (mL/g)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt08$t, model$residuals,
     xlab = "extraction time (min)", ylab = "residuals") + 
  abline(h=0, col="grey")
plot(DoEOpt08$P, model$residuals,
     xlab = "microwave power (W)", ylab = "residuals") + 
  abline(h=0, col="grey")



# contour plots of the responses

par(mfrow=c(1,3))

contour(model, ~ x_R + x_t,
        image = TRUE, 
        zlim = c(1,1.25),
        xlabs=c("Liquid:solid Ratio (mL/g)","Extraction Time (min)")
        )
points(DoEOpt08$R, DoEOpt08$t)

contour(model, ~ x_R + x_P,
        image = TRUE,
        zlim = c(1,1.25),
        xlabs=c("Microwave Power (W)", "Liquid:solid Ratio (mL/g)")
        )
points(DoEOpt08$R, DoEOpt08$P)

contour(model, ~ x_t + x_P,
        image = TRUE,
        zlim = c(1,1.25),
        xlabs=c("Microwave Power (W)", "Extraction Time (min)")
        )
points(DoEOpt08$t, DoEOpt08$P)

# predicted maximum extraction

max <- data.frame(x_R = 0.1648352, x_t = 0.2688172, x_P = 0.3557312 )
predict(model, max)

# perspective plots of the responses

par(mfrow=c(1,3))

persp(model, ~ x_R + x_t, 
        col = terrain.colors(50), contours = "colors",
        zlab = "Extraction Yield (%)",
        zlim = c(1,1.25),
        xlabs=c("Liquid:solid Ratio (mL/g)","Extraction Time (min)"))

persp(model, ~ x_R + x_P, 
        col = terrain.colors(50), contours = "colors",
        zlab = "Extraction Yield (%)",
      zlim = c(1,1.25),
        xlabs=c("Microwave Power (W)", "Liquid:solid Ratio (mL/g)"))

persp(model, ~ x_t + x_P, 
        col = terrain.colors(50), contours = "colors",
        zlab = "Extraction Yield (%)",
        zlim = c(1,1.25),
        xlabs=c("Microwave Power (W)", "Extraction Time (min)"))

#
##
### Building face-centered and Box-Behnken designs in R

# building a face-centered composite design:

fcd3 <- ccd(3, alpha = "faces", oneblock = TRUE)
fcd3

fcd3$R <- fcd3$x1*10 + 50
fcd3$t <- fcd3$x2*5 + 25
fcd3$p <- fcd3$x3*100 + 400
fcd3

fcd3 <- as.data.frame(fcd3)
write.csv(fcd3, "FCCD3.csv")

# building a Box-Behnken design

bbd3 <- bbd(3)
bbd3

bbd3$R <- bbd3$x1*10 + 50
bbd3$t <- bbd3$x2*5 + 25
bbd3$p <- bbd3$x3*100 + 400
bbd3

bbd3 <- as.data.frame(bbd3)
write.csv(bbd3, "BBD3.csv")

