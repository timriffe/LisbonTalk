
# Author: tim
###############################################################################

setwd("/home/tim/workspace/LisbonTalk/")

#install.packages("eurostat")
#install.packages("rvest")
library(eurostat)
library(rvest)

# immigration by sex and age
imm   <- get_eurostat("migr_imm8", time_format = "num")
# emigration by sex and age (usually underestimate)
emm   <- get_eurostat("migr_emi2", time_format = "num")
# fertility rates by sex and age
fert  <- get_eurostat("demo_frate", time_format = "num")
birt  <- get_eurostat("demo_fasec", time_format = "num")
# pop Jan 1
pop   <- get_eurostat("demo_pjan", time_format = "num")
# just take exposures as avg btwn years.
dRR   <- get_eurostat("demo_magec", time_format = "num") # AP
dVV   <- get_eurostat("demo_mager", time_format = "num") # PC
# Eurostat projection, main
proj  <- get_eurostat("proj_13npms", time_format = "num")
ex    <- get_eurostat("demo_mlexpec", time_format = "num")

# SAVE OUT
save(imm, file = "Data/imm.Rdata")
save(emm, file = "Data/emm.Rdata")
save(fert, file = "Data/fert.Rdata")
save(pop, file = "Data/pop.Rdata")
save(dRR, file = "Data/dRR.Rdata")
save(dVV, file = "Data/dVV.Rdata")
save(proj, file = "Data/proj.Rdata")
save(birt, file = "Data/birt.Rdata")
save(ex, file = "Data/ex.Rdata")

# end
