
# Author: tim
###############################################################################

setwd("/home/tim/workspace/LisbonTalk/")
read <- function(X){
	local(get(load(X)))
}
RRVV_2_TLTU2 <- function(RR, VV) {
	# assuming ages organized in same way
	# no UNK and no TOT, last age open
	n  <- length(RR)
	TU <- cumsum(RR - VV)
	TL <- c((RR - TU)[-n], RR[n])
	list(TL = TL, TU = TU[-n])
}

# for geo, sex, year (time)


mrates <- function(pop,dRR,dVV,geo,sex,year){
	
	# cut down inputs
	p1 		<- pop[pop$geo == geo & pop$sex == sex & pop$time == year, ]
	p2 		<- pop[pop$geo == geo & pop$sex == sex & pop$time == year + 1, ]
	drr  	<- dRR[dRR$geo == geo & dRR$sex == sex & dRR$time == year, ]
	dvv  	<- dVV[dVV$geo == geo & dVV$sex == sex & dVV$time == year, ]
	
	# probably all w same open, 100
	maxAp1  <- max(p1$age)
	maxAp2  <- max(p2$age)
	maxAdr  <- max(drr$age)
	maxAdv  <- max(dvv$age)
	
	maxA    <- min(c(maxAp1, maxAp2, maxAdr, maxAdv))
	ages   	<- 0:(maxA + 1)
	
	# select
	drr  	<- drr[drr$age %in% ages, ]
	dvv  	<- dvv[dvv$age %in% ages, ]
	p1 		<- p1[p1$age %in% ages, ]
	p2 		<- p2[p2$age %in% ages, ]
	
	# order
	
	drr 	<- drr[order(drr$age), ]
	dvv 	<- dvv[order(dvv$age), ]
	p1 		<- p1[order(p1$age), ]
	p2 		<- p2[order(p2$age), ]
	
	n <- max
	# now get TL, TU
	TLTU 	<- RRVV_2_TLTU2(drr$values, dvv$values)
	
	# get vectors, cut to size
	N       <- maxA + 1
	P1 		<- p1$values[1:N]
	P2 		<- p2$values[1:N]
	du 		<- TLTU$TU[1:N]
	dl 		<- TLTU$TL[1:N]
	
	# now HMD v5 exposure formula
	exposure <- ((P1 + P2) / 2) + (1 / 6) * (dl - du)
	
	data.frame(geo = geo,
			   sex = sex,
			   time = year,
			   age = ages[1:N],
			   Exp = exposure,
			   Dx = dl + du,
			   Mx = (dl + du) / exposure,
			   stringsAsFactors = FALSE)
}


CDa0 <- function(m0, sex = "f"){
	sex <- rep(sex, length(m0))
	ifelse(sex == "m", 
			ifelse(m0 >= 0.107, 0.330, {0.045 + 2.684 * m0}), # males
			ifelse(m0 >= 0.107, 0.350, {0.053 + 2.800 * m0})  # females
	) 
}


getLx <- function(mChunk){
	mx    <- mChunk$Mx
	N     <- nrow(mChunk)
	ax    <- mx * 0 + .5
	ax[1] <- CDa0(mx[1], tolower(unique(mChunk$sex)))
	qx    <- mx / (1 + (1 - ax) * mx)
	px    <- 1 - qx
	lx    <- cumprod(c(1,px))[1:N]
	dx    <- -diff(c(lx,0))
	Lx    <- lx - (1 - ax) * dx
	
	# save interesting columns
	mChunk$dx <- dx
	mChunk$Lx <- Lx
	mChunk
} 

# does the above in friendlier way
getdx <- function(pop,dRR,dVV,geo,sex,year){
	Chunk <-getLx(mrates(pop = pop,
				 dRR = dRR,
				 dVV = dVV,
				 geo = geo,
				 sex = sex,
				 year = year))
	Chunk
}

#dRR <- read("Data/Cleaned/dRR2.Rdata")
#dVV <- read("Data/Cleaned/dVV2.Rdata")
#pop <- read("Data/Cleaned/pop2.Rdata")


#EU28 <- mratesGSY(pop,dRR,dVV,"EU28","F",2014)
#
#plot(EU28$age, EU28$Mx, type = 'l', log = 'y')
#
#getLx(EU28)





