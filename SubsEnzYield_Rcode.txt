# Analysing a 2^2 Factorial Design

# file view
str(SubsEnzYield)

# creating factors for the Analysis of Variance
SubsEnzYield$Factor_S <- as.factor(SubsEnzYield$xS)
SubsEnzYield$Factor_E <- as.factor(SubsEnzYield$xE)
str(SubsEnzYield)
View(SubsEnzYield)

# ANOVA
anova <- aov(Yield ~ Factor_S + Factor_E + Factor_S:Factor_E, data=SubsEnzYield)
summary(anova)

# regression model with coded variables	
coded.model <- lm(Yield ~ xS + xE, data = SubsEnzYield)
summary(coded.model)

# regression model with natural variables
nat.model <- lm(Yield ~ Substrate + Enzyme, data = SubsEnzYield)
summary(nat.model)

# contour plot
library(rsm)
contour(nat.model, ~ Substrate+Enzyme, 
        image = TRUE, 
        xlabs=c("Enzyme (g/L)","Substrate (g/L)" ))

# residual plot
plot(SubsEnzYield$Yield, nat.model$residuals,
     xlab = "Experimental Results", ylab = "Residuals") +
  abline(h=0, col = "gray75")

# creating a table with factors, means and standard deviation

install.packages("dplyr") # run this line code if the dplyr package will be used for the first time.

library(dplyr)
dt <- group_by(SubsEnzYield, Substrate, Enzyme) %>%
  summarise(mean=mean(Yield), sd=sd(Yield), n=n())
View(dt)

write.csv(dt, "summarySEY.csv")
