# Analysis of a 2?3 Factorial Design

#visulalize data
str(SETY_1_)

#creating factors for the Analysis of Variance
SETY_1_$Factor_T <- as.factor(SETY_1_$T)
SETY_1_$Factor_S <- as.factor(SETY_1_$S)
SETY_1_$Factor_E <- as.factor(SETY_1_$E)
str(SETY_1_)
View(SETY_1_)

#ANOVA
anova <- aov(Yield ~ Factor_T*Factor_S*Factor_E, data = SETY)
summary(anova)

# regression model with coded variables
coded.model <- lm(Yield ~ xT + xS + xE + xT*xE, data = SETY)
summary(coded.model)

# regression model with natural variables
nat.model <- lm(Yield ~ T + S + xE + T*xE, data = SETY)
summary(nat.model)

# residual plot
plot(SETY$Yield,nat.model$residuals,
     xlab = "Experimental Results", ylab = "Residuals") +
  abline(h=0, col = "gray75")

# contour plots
library(rsm)
par(mfrow = c(1,2)) # creates a window for 2 plots
contour(nat.model, ~T+S, image = TRUE, at = c(xE=-1),
        #zlim = c(45,85),
        xlabs = c("Substrate (g/L)", "Temperature (?C)"))
contour(nat.model, ~T+S, image = TRUE, at = c(xE=+1),
        #zlim = c(45,85),
        xlabs = c("Substrate (g/L)", "Temperature (?C)"))
