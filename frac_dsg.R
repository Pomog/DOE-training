#Building 2^(k-p) designs

#install.packages("FrF2")
library(FrF2)

# parameters: number of runs, number of factors
# returns the design with the highest possible resolution.

# fractional factorial design with 5 factors and 2^(5-2) = 8 runs

dsg <- FrF2(nfactors = 5, nruns = 8)
summary(dsg)

# parameters: number of factors, resolution
# results in the smallest fractional design

# fractional factorial design with 5 factors and resolution IV

dsg <- FrF2(nfactors = 5, resolution = 4)
summary(dsg)

# to view the design as a table and save a csv file
View(dsg)
write.csv(dsg, "design.csv")





