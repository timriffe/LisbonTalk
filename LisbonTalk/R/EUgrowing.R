# Author: tim
###############################################################################
setwd("/home/tim/workspace/LisbonTalk/")

Lotkamin <- function(r, Lx, Fx, x){
	# if we have the right r, then this sums to 1
	SUM <- sum(exp(-r * (x + .5)) * Lx * Fx)
	
	# so the difference between 1 and our sum tells us how
	# far off we are.
	res <- 1 - SUM
	
	# Last piece returned:
	# do this ^2 to keep it positive. 
	# abs() would work, and other stuff too.
	res ^ 2 # needs to be positive because we're minimizing, 
	# otherwise it'd keep going to -Inf...
}

# two-sex Leslie

makeLeslie   <- function(Lxf, Lxm, Fxf, Fxm, FxVV = FALSE){
	
	# You could avoid having to do this approximation
	# if you have a PC lifetable, in which case it would
	# be simply 1 - qx
	Sxf           <- Lxf[-1] / Lxf[-length(Lxf)]
	Sxm           <- Lxm[-1] / Lxm[-length(Lxm)]
	# square
	Survf         <- diag(Sxf)
	Survm         <- diag(Sxm)
	
	avgFertf <- (Fxf[-1] + Fxf[-length(Fxf)]) / 2
	avgFertm <- (Fxm[-1] + Fxm[-length(Fxm)]) / 2
	
	FDf <- avgFertf * (1 - (1 - Sxf) / 2)   # accounting for survival
	FDm <- avgFertm * (1 - (1 - Sxf) / 2)
	# of mothers
	
	# assume all mortality in lower triangle for infants
	FirstRowf     <- FDf * (1 - (1 - Lxf[1]) / 2 )
	Malefert      <- FDf * (1 - (1 - Lxm[1]) / 2 )
	# population by age, falls into the columns,
	# when it hits a positive number, it multiplies,
	# then falls out the left side.
	Projf         <- cbind(rbind(FirstRowf, Survf), 0)
	Projm         <- cbind(rbind(0, Survf), 0)
	Fertm         <- Projm * 0
	Zeros         <- Projm * 0
	Fertm[1, ]    <- c(Malefert,0)
	
	Proj <- rbind(cbind(Projf, Zeros),
			cbind(Fertm, Projm))
	
	return(Proj)
}


source("R/mrates.R")
dRR   <- read("Data/Cleaned/dRR2.Rdata")
dVV   <- read("Data/Cleaned/dVV2.Rdata")
pop   <- read("Data/Cleaned/pop2.Rdata")
fert  <- read("Data/Cleaned/fert2.Rdata")
birt  <- read("Data/birt.Rdata")
birt  <- as.data.frame(birt)


geo   <- "EU28"
sex   <- "F"
year  <- 2013


EU28f <- getLx(mratesGSY(pop,dRR,dVV,"EU28","F",2014))
EU28m <- getLx(mratesGSY(pop,dRR,dVV,"EU28","M",2014))

#
#plot(EU28$age, EU28$Mx, type = 'l', log = 'y')
#

Fx   <- fert[fert$geo == geo & fert$time == year, ]
BB   <- birt[birt$age == "TOTAL" & birt$geo == geo & birt$sex == "M" & birt$time == year,"values"]
BG   <- birt[birt$age == "TOTAL" & birt$geo == geo & birt$sex == "F" & birt$time == year,"values"]
SRB  <- (BB / BG)
Fx   <- Fx[order(Fx$age), ]
beta <- max(Fx$age)


EU28f$Fxf <- 0
EU28f$Fxm <- 0
EU28f$Fxf[EU28f$age %in% Fx$age] <- Fx$values * 1 / (1 + SRB)
EU28f$Fxm[EU28f$age %in% Fx$age] <- Fx$values * SRB / (1 + SRB)
# just 0 to beta
EU28f <- EU28f[1:(beta + 1), ]
EU28m <- EU28m[1:(beta + 1), ]


# shrinking, but how fast?
(R0 <- sum(EU28f$Lx * EU28f$Fxf))
#(R0 <- sum(EU28f$Lx * EU28f$Fxm))

# lotka's r


opt1 <- optim(par = 0, 
		fn = Lotkamin, 
		Lx = EU28f$Lx,      
		Fx = EU28f$Fxf,       # these need to be given by name!
		x = EU28f$age,
		lower = -1,    # give it a reasonable left bound
		upper = 1,     # and a reasonable right bound.
		# would work if we omit these too, but why not?
		method = "Brent", # Brent because we're optimizing a single par
		# attempt to make estimate closer to truth
		control = list(abstol = 1e-12))
(r <- opt1$par) # Lotka's growth rate

getLotkar <- function(pop, dVV, dRR, fert, geo, year, SRB = 1.05){
	PLx <- getdx(pop,dRR,dVV,geo=geo,sex="F",year=year)
	
	
	Fx   <- fert[fert$geo == geo & fert$time == year, ]
	Fx   <- Fx[order(Fx$age), ]
	beta <- max(Fx$age)
	PLx$Fxf <- 0
	PLx$Fxf[PLx$age %in% Fx$age] <- Fx$values * 1 / (1 + SRB)
	
     # just 0 to beta
	PLx <- PLx[1:(beta + 1), ]
	
	optim(par = 0, 
			fn = Lotkamin, 
			Lx = PLx$Lx,      
			Fx = PLx$Fxf,       # these need to be given by name!
			x = PLx$age,
			lower = -1,    # give it a reasonable left bound
			upper = 1,     # and a reasonable right bound.
			# would work if we omit these too, but why not?
			method = "Brent", # Brent because we're optimizing a single par
			# attempt to make estimate closer to truth
			control = list(abstol = 1e-12))$par
}


# e^rT = R0
# rT = log(R0)
# T = log(R0) / r
TG <- log(R0) / r   # stable mean gen length

# so every TG years the pop would be R0 smaller

pEU28 <- pop[pop$geo == "EU28",]
ind   <- pEU28$sex == "T"
PP    <- tapply(pEU28$values[ind], pEU28$time[ind],sum)
plot(as.integer(names(PP)),PP,type = 'l')
mean(diff(PP))




EUL <- makeLeslie(EU28f$Lx,EU28m$Lx, EU28f$Fxf, EU28$Fxm)

lambda <- Re(eigen(EUL)$values[1])
rr     <- log(lambda)		

log(2) / (log(1 + rr) )  # rule of 72 works very well here!

plot(EU28f$age, EU28f$Fxm)
lines(EU28f$age, EU28f$Fxf)
(TFR <- sum(EU28f$Fxm) + sum(EU28f$Fxf))
72 / rr
10 * exp(rr*72)

# ---------------------------------------------------------------
#

83 * rr
72 * rr

# ---------------------------------------------------------------

ex <- read("Data/Cleaned/ex2.Rdata")
ex <- as.data.frame
ind <- ex$age == 0 & ex$sex == "F" & ex$geo == "EU28"
e0 <- ex[ind, ]

plot(e0$time, e0$values)






