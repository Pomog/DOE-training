# loading Response Surface Methodology package
library(rsm)

# Data file: 15032026-results.csv
# run:  the run number in a block
# order:  the order of execution
# ip_6_mass_g:  amount in g of IP.6 used for the run
# dmso_m: amount in g of DMSO used for the run
# meoh_m: amount in g of MeOH used for the run
# dmso_vr:  calculated val. DMSO in mL divided by ip_6_mass_g
# meoh_vr:  calculated val. MeOH in mL divided by ip_6_mass_g
# total_ml: calculated val. DMSO + MeOH in mL
# fc_m: amount in g of isolated and dried product
# yield_pct:  calculated val. yield in % fc_m divided by ip_6_mass_g
# purge:  calculated val. ratio of the HPCL signals Area% at 220 nm 
#   for IP.5 in the starting material and product

# read the data
df <- read.csv("15032026-results.csv")

# get actual Δ for dmso_vr and meoh_vr
dmso_vr_max <- max(df[, "dmso_vr"])
dmso_vr_min <- min(df[, "dmso_vr"])
meoh_vr_max <- max(df[, "meoh_vr"])
meoh_vr_min <- min(df[, "meoh_vr"])

# calculate step
dmso_d <- (dmso_vr_max - dmso_vr_min) / 2
meoh_d <- (meoh_vr_max - meoh_vr_min) / 2

# center points
dmso_0 <- 8 # zero point
meoh_0 <- 17  # zero point

# setting the relationship between the coded and natural variables
# all variables defied as numeric
df_coded <- coded.data(
  df,
  DMSO_R ~ (dmso_vr - dmso_0) / dmso_d,
  MeOH_R ~ (meoh_vr - meoh_0) / meoh_d
)

# run regression model, simplify until the highest adjusted-R^2 and/or 
# the lowest p-value

fit_yield <- rsm(
  yield_pct ~ FO(DMSO_R, MeOH_R) + TWI(DMSO_R, MeOH_R) + PQ(DMSO_R, MeOH_R),
  data = df_coded
)
fit_yield_red <- rsm(
  yield_pct ~ FO(DMSO_R, MeOH_R) + TWI(DMSO_R, MeOH_R) + PQ(MeOH_R),
  data = df_coded
)

summary(fit_yield, fit_yield_red)
anova(fit_yield, fit_yield_red)

fit_purge_red <- rsm(
  purge ~ FO(DMSO_R, MeOH_R) + TWI(DMSO_R, MeOH_R) + PQ(MeOH_R),
  data = df_coded
)

summary(fit_purge, fit_purge_red)
anova(fit_purge, fit_purge_red)

# In both cases, MeOH showed a significant effect with negative quadratic curvature, 
# and the DMSOxMeOH interaction remained significant, 
# indicating that response depends on solvent balance 
# rather than on independent additive effects alone.

# look at the residuals plots against the regression results and 
# against the experimental results

# Make final models
yield_pct ~ FO(DMSO_R, MeOH_R) + TWI(DMSO_R, MeOH_R) + PQ(MeOH_R)
purge ~ FO(DMSO_R, MeOH_R) + TWI(DMSO_R, MeOH_R) + PQ(MeOH_R)

# build contour plots of the responses