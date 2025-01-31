# loading Response Surface Methodology package
library(rsm)

# Define real variable ranges
Solvent_V_min <- 8
Solvent_V_max <- 16
Acid_eq_min <- 1.0
Acid_eq_max <- 1.8

# Calculate center and range
Solvent_V_center <- (Solvent_V_max + Solvent_V_min) / 2
Solvent_V_range05 <- (Solvent_V_max - Solvent_V_min) / 2

Acid_eq_center <- (Acid_eq_max + Acid_eq_min) / 2
Acid_eq_range05 <- (Acid_eq_max - Acid_eq_min) / 2

# calculating the coded variables 
b5$volume <- (b5$Solvent_V - Solvent_V_center)/Solvent_V_range05
b5$eq <- (b5$Acid_eq - Acid_eq_center)/Acid_eq_range05

# setting the relationship between the coded and the natural variables
b5 <- as.coded.data(b5, 
                    volume ~ (Solvent_V - 12)/4,
                    eq ~ (Acid_eq - 1.4)/0.4)

str(b5)

#model.Yield_percent <- rsm(Yield_percent ~ FO(volume,eq) + TWI(volume,eq) + PQ(volume,eq), data = b5)
model.Yield_percent <- rsm(Yield_percent ~ FO(volume), data = b5)

# Check model summary
summary(model.Yield_percent)

par(mfrow=c(2,2))

plot(b5$Yield_percent, model.Yield_percent$residuals, 
     xlab = "Yield_percent (experimental)", ylab = "residuals") + 
              abline(h=0, col="grey")

plot(b5$Solvent_V, model.Yield_percent$residuals, 
     xlab = "Solvent_V", ylab = "residuals") + 
  abline(h=0, col="grey")

plot(b5$Acid_eq, model.Yield_percent$residuals, 
     xlab = "Acid_eq", ylab = "residuals") + 
  abline(h=0, col="grey")

plot(b5$Solvent_V, b5$Yield_percent, 
     xlab = "Solvent_V", ylab = "Yield_percent")



# Fit a second-order response surface model
model <- rsm(Yield_percent ~ SO(x1, x2), data = b5)

# Check model summary
summary(model)

# Generate a contour plot with coded variables
contour(model, ~ x1 + x2, image = TRUE)

nge05

# Check model summary
summary(model)
str(b5)


