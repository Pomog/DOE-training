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

b5_clean <- b5[-9, ]  # Remove 9th row
#model.Yield_percent <- rsm(Yield_percent ~ FO(volume,eq) + TWI(volume,eq) + PQ(volume,eq), data = b5_clean)

#model.Yield_percent <- rsm(Yield_percent ~ FO(volume,eq), data = b5_clean)
model.Yield_percent <- rsm(Yield_percent ~ FO(volume), data = b5_clean)

# Check model summary
summary(model.Yield_percent)

par(mfrow=c(2,3))

plot(b5_clean$Yield_percent, model.Yield_percent$residuals, 
     xlab = "Yield_percent (experimental)", ylab = "residuals") + 
              abline(h=0, col="grey")

plot(b5_clean$Solvent_V, model.Yield_percent$residuals, 
     xlab = "Solvent_V", ylab = "residuals") + 
  abline(h=0, col="grey")


qqnorm(model.Yield_percent$residuals)  # Creates the Q-Q plot
qqline(model.Yield_percent$residuals, col = "red")  # Adds reference line

install.packages("lmtest")  # Install if you haven't
library(lmtest)
bptest(model.Yield_percent)  # Perform Breusch-Pagan test for heteroscedasticity

# plot the relationship between Yield_percent and volume
min_volume <- min(b5_clean$volume)
max_yield <- predict(model.Yield_percent, newdata = data.frame(volume = min_volume))
# Create a sequence of volume values
volume_seq <- seq(min(b5_clean$volume), max(b5_clean$volume), length.out = 100)

# Predict yield for each volume
predicted_yield <- predict(model.Yield_percent, newdata = data.frame(volume = volume_seq))

# Plot the regression line
plot(b5_clean$volume, b5_clean$Yield_percent, 
     xlab = "Volume", ylab = "Yield (%)", 
     main = "Yield vs. Volume", pch = 16, col = "blue")

# Add regression line
lines(volume_seq, predicted_yield, col = "red", lwd = 2)


plot(b5$Acid_eq, model.Yield_percent$residuals, 
     xlab = "Acid_eq", ylab = "residuals") + 
  abline(h=0, col="grey")

plot(b5$Solvent_V, b5$Yield_percent, 
     xlab = "Solvent_V", ylab = "Yield_percent")