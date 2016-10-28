
# Author: tim
###############################################################################

setwd("/home/tim/workspace/LisbonTalk/")

# load some helpful functions
source("R/Functions.R")

pop  <- read("Data/Cleaned/pop2.Rdata")
dVV  <- read("Data/Cleaned/dVV2.Rdata")
dRR  <- read("Data/Cleaned/dRR2.Rdata")

# decompose pop by years lived and left
# chronologica age in rows,
# remaining years in columns
Pxy  <- getPxy(pop, dVV, dRR, geo = "PT", sex = "F", year = 2013)

# a time-to-death health pattern, imagine prevalence is worst in final
# year, and it's an 8-year build-
gy <- c(seq(.8,0,length=9),rep(0,91))

# prevalence-weighted, if it's time-to-death condition
Pxyg <- t(t(Pxy) * gy)
# overall prevalence:
sum(Pxyg) / sum(Pxy)

# time-to-death pattern:
plot(colSums(Pxyg) / colSums(Pxy))
# age-pattern: (this is what we normally see
plot(rowSums(Pxyg) / rowSums(Pxy))
# since these came from same assumption, total morbidity is equal.
# but if you apply either of these patterns to a new population,
# results will differ depending on which you use. If mort is lower
# in the new pop, the chronological age pattern will exaggerate morbidity
# prediction, while the ttd pattern does not have this bias (though it
# is of course less directly observable)

# can try out scenarios here.




