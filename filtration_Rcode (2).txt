# Analysis of a Unreplicated 2ˆ4 Factorial Design

#visulalize data
str(filtration)

#creating factors for the Analysis of Variance
filtration$Factor_T <- as.factor(filtration$T)
filtration$Factor_P <- as.factor(filtration$P)
filtration$Factor_C <- as.factor(filtration$C)
filtration$Factor_W <- as.factor(filtration$W)
str(filtration)
View(filtration)

#ANOVA
anova1 <- aov(filt ~ Factor_T*Factor_P*Factor_C*Factor_W, data = filtration)
summary(anova1)

anova2 <- aov(filt ~ (Factor_T+Factor_P+Factor_C+Factor_W)^2, data = filtration)
summary(anova2)

anova3 <- aov(filt ~ (Factor_T+Factor_C+Factor_W)^2, data = filtration)
summary(anova3)


anova4 <- aov(filt ~ Factor_T+Factor_C+Factor_W + Factor_T:Factor_C + Factor_T:Factor_W, data = filtration)
summary(anova4)

# regression model with coded variables
coded.model <- lm(filt ~ xT + xC+ xW + xT*xC + xT*xW, data=filtration)
summary(coded.model)

# regression model with natural variables
nat.model <- lm(filt ~ T + C + W + T*C + T*W, data=filtration)
summary(nat.model)

# residual plot
plot(filtration$filt, nat.model$residuals,
     xlab = "Experimental Results", ylab = "Residuals") +
  abline(h=0, col = "gray75")

library(rsm)
par(mfrow = c(1,2)) # creates a window for 2 plots
contour(nat.model, ~T+C, image = TRUE,
        zlim = c(45,100),
        xlabs = c("Concentration (g/L)", "Temperature (ºC)"))
contour(nat.model, ~T+W, image = TRUE,
        zlim = c(45,100),
        xlabs = c("Temperature (ºC)","Stirrer Speed (rpm)"))