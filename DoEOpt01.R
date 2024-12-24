# Linear Regression Model

# checking the file structure

str(DoEOpt01)

# regression model Purity=f(Pollution)
model <- lm(Purity ~ Pollution, data = DoEOpt01)
summary(model)

# residual plot
par(mfrow = c(1,3))
plot(DoEOpt01$Purity, model$residuals,
     xlab = "Experimental Results", ylab = "Residuals") +
  abline(h=0, col = "gray75")
plot(model$fitted.values, model$residuals,
     xlab = "Predicted Results", ylab = "Residuals") +
  abline(h=0, col = "gray75")
plot(DoEOpt01$Pollution, model$residuals,
     xlab = "Pollution Count", ylab = "Residuals") +
  abline(h=0, col = "gray75")

# scatter plot + model plot
par(mfrow = c(1,1))
plot(DoEOpt01$Pollution, DoEOpt01$Purity,
     xlab = "Pollution Count (ppm)", ylab = "Oxygen Purity (%)") + 
  abline(96.4546,-2.9010)
