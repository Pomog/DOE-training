# Analysing a 2^2 Factorial Design with replicates in blocks

# file view
str(SEYB)

# creating factors for the Analysis of Variance
SEYB$Factor_S <- as.factor(SEYB$xS)
SEYB$Factor_E <- as.factor(SEYB$xE)
SEYB$Blocks <- as.factor(SEYB$Blocks)
str(SEYB)
View(SEYB)

# ANOVA
anova <- aov(Yield ~ Blocks + Factor_S*Factor_E, data=SEYB)
summary(anova)

# regression model with coded variables
coded.model <- lm(Yield ~ xS + xE, data = SEYB)
summary(coded.model)

# regression model with natural variables
nat.model <- lm(Yield ~ Substrate + Enzyme, data = SEYB)
summary(nat.model)

# residual plot
plot(SEYB$Yield, nat.model$residuals,
     xlab = "Experimental Results", ylab = "Residuals") +
  abline(h=0, col = "gray75")

# contour plot
library(rsm)
contour(nat.model, ~ Substrate+Enzyme, 
        image = TRUE, 
        xlabs=c("Enzyme (g/L)","Substrate (g/L)" ))