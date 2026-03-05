install.packages("rsm")
library(rsm)

# (IP.6 amount in g)
m_g <- 3

# (mL per g)
DMSO_min <- 4;   DMSO_max <- 12
MeOH_min <- 4;  MeOH_max <- 30

# Mapping
DMSO_center <- (DMSO_min + DMSO_max)/2
DMSO_step   <- (DMSO_max - DMSO_min)/(2*alpha)

MeOH_center <- (MeOH_min + MeOH_max)/2
MeOH_step   <- (MeOH_max - MeOH_min)/(2*alpha)

# Rotatable CCD
alpha <- sqrt(2)
d_coded <- ccd(2, n0 = c(3, 2), alpha = "rotatable", randomize = TRUE)

# Add decoded factors
design <- transform(d_coded,
                    mass_g = m_g,
                    DMSO_V = DMSO_center + x1 * DMSO_step,
                    MeOH_V = MeOH_center + x2 * MeOH_step,
                    DMSO_mL = (DMSO_center + x1 * DMSO_step) * m_g,
                    MeOH_mL = (MeOH_center + x2 * MeOH_step) * m_g
)

design$Total_mL <- design$DMSO_mL + design$MeOH_mL

View(design)
write.csv(design, "01.csv")

ylim <- extendrange(design$MeOH_mL, f = 0.08) 
plot(design$DMSO_mL, design$MeOH_mL,
     pch = 16,
     xlab = "DMSO (mL)", ylab = "MeOH (mL)",
     main = "Design points in mL",
     ylim = ylim
     )

text(design$DMSO_mL, design$MeOH_mL, labels = design$run.order, pos = 3, cex = 0.7)