# Create a data frame
doe_data <- data.frame(
  Run = 1:4,
  CoolingRate = c(-1, -1, 1, 1),     # -1 = slow, 1 = fast
  AcidAdditionRate = c(-1, 1, -1, 1), # -1 = slow, 1 = fast
  Yield = c(90.7, 87.4, 98.0, 86.3)  # Yield in %
)

# View the data
print(doe_data)

# Calculate mean responses for each factor level
overall_mean <- mean(doe_data$Yield)

# Main effects
effect_cooling <- mean(doe_data$Yield[doe_data$CoolingRate == 1]) - 
  mean(doe_data$Yield[doe_data$CoolingRate == -1])
effect_acid <- mean(doe_data$Yield[doe_data$AcidAdditionRate == 1]) - 
  mean(doe_data$Yield[doe_data$AcidAdditionRate == -1])

# Interaction effect
interaction_effect <- mean(doe_data$Yield[doe_data$CoolingRate == 1 & doe_data$AcidAdditionRate == 1]) -
  mean(doe_data$Yield[doe_data$CoolingRate == 1 & doe_data$AcidAdditionRate == -1]) -
  mean(doe_data$Yield[doe_data$CoolingRate == -1 & doe_data$AcidAdditionRate == 1]) +
  mean(doe_data$Yield[doe_data$CoolingRate == -1 & doe_data$AcidAdditionRate == -1])

# Print effects
cat("Overall Mean:", overall_mean, "\n")
cat("Effect of Cooling Rate:", effect_cooling, "\n")
cat("Effect of Acid Addition Rate:", effect_acid, "\n")
cat("Interaction Effect:", interaction_effect, "\n")


par(mar = c(5, 5, 5, 5))

# Convert both CoolingRate and AcidAdditionRate to factors with custom labels
doe_data$CoolingRate <- factor(doe_data$CoolingRate, levels = c(-1, 1), labels = c("Slow", "Fast"))
doe_data$AcidAdditionRate <- factor(doe_data$AcidAdditionRate, levels = c(-1, 1), labels = c("Slow", "Fast"))

# Now create the interaction plot
interaction.plot(
  x.factor = doe_data$CoolingRate, 
  trace.factor = doe_data$AcidAdditionRate, 
  response = doe_data$Yield,
  xlab = "Cooling Rate", 
  ylab = "Yield", 
  trace.label = "Acid Addition Rate",
  col = c("blue", "red"), 
  lty = 1:2, 
  type = "b",
  legend = FALSE
)

# Add the legend for better clarity
legend("topleft", legend = c("Slow Acid Addition", "Fast Acid Addition"), col = c("blue", "red"), lty = 1:2, title = "Acid Addition Rate")




