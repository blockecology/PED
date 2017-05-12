# Indicator species analysis
install.packages("indicspecies")
require(indicspecies)


# Load canopy and understory data
can <- read.csv("can.csv", sep=",", header=T, row.names=1)
und <- read.csv("und.csv", sep=",", header=T)

units <- rep(c("OC", "SU", "UO", "LO", "CH", "TE"), each = 10)

# Indicator values analysis
indval.c <- multipatt(can, units, control = how(nperm=999))
summary(indval.c, indvalcomp=TRUE)

indval.u <- multipatt(und, units, control = how(nperm=999))
summary(indval.u, indvalcomp=TRUE)
