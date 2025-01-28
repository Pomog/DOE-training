# Analysis of a Unreplicated 2ˆ4 Factorial Design Confunded in 2 Blocks

#FR1: results for the design confounded in blocks
#FR2: results for the design with the 2 batchs of raw material randomly distributed

#visulalize data
str(filtB)

#creating factors for the Analysis of Variance
filtB$Factor_T <- as.factor(filtB$T)
filtB$Factor_P <- as.factor(filtB$P)
filtB$Factor_C <- as.factor(filtB$C)
filtB$Factor_W <- as.factor(filtB$W)
filtB$block <- as.factor(filtB$block)
str(filtB)
View(filtB)

#ANOVA
anova1 <- aov(FR1 ~ block + Factor_T*Factor_P*Factor_C*Factor_W, data = filtB)
summary(anova1)

anova2 <- aov(FR1 ~ block + (Factor_T+Factor_P+Factor_C+Factor_W)^2, data = filtB)
summary(anova2)

anova3 <- aov(FR1 ~ block + Factor_T+Factor_C+Factor_W + Factor_T:Factor_C + Factor_T:Factor_W, data = filtB)
summary(anova3)

# regression model with coded variables
coded.model <- lm(FR1 ~ xT + xC + xW + xT*xC + xT*xW, data=filtB)
summary(coded.model)

# regression model with natural variables
nat.model <- lm(FR1 ~ T + C + W + T*C + T*W, data=filtB)
summary(nat.model)

# bar plots
library(ggplot2)

# Temperature x Stirring Rate
p <- ggplot(filtB, aes(x=Factor_T,y=FR1, fill=W)) +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_se , geom = "errorbar", width=0.2) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() + 
  facet_grid(.~W, labeller = label_parsed) +
  xlab("Temperature (ºC)") + ylab("Filtration Rate (L/min)")
print(p)

# Temperature x Concentration
p <- ggplot(filtB, aes(x=Factor_T,y=FR1, fill=C)) +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_se , geom = "errorbar", width=0.2) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_bw() + 
  facet_grid(.~C, labeller = label_parsed) +
  xlab("Temperature (ºC)") + ylab("Filtration Rate (L/min)")
print(p)

# Temperature x Concentration x Stirring rate
p <- ggplot(filtB, aes(x=Factor_T,y=FR1, fill=W*C)) +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_se , geom = "errorbar", width=0.2) +
  theme_bw() + 
  facet_grid(C~.~W, labeller = label_both) +
  xlab("Temperature (ºC)") + ylab("Filtration Rate (L/min)")
print(p)
