# Author: tim
###############################################################################

read <- function(X){
	local(get(load(X)))
}

# functions for Sanderson and others:

select <- function(X, geo = "EU28",sex = "F",year = 2014){
	X <- X[X$geo == geo & X$sex == sex & X$time == year, ]
	X[order(X$age), ]
}
# Px <- p$values
# ex <- e$values
Sanderson <- function(pop, ex, geo, sex, year, cutoff = 15){
	
	p   <- select(pop, geo = geo, sex =sex, year = year)
	e   <- select(ex, geo = geo, sex = sex, year = year)
	
	# if necessary, extend ex
	
	# open ages:
	aOpen   <- max(e$age)
	paOpen  <- max(p$age)
	
	if (paOpen < aOpen){
		stop("I didn't prepare the program for having \nhigher e(x) open age than pop open age")
	}
	
	if (paOpen > aOpen){
		elast   <- e$values[which.max(e$age)]
		
		# ages we want values for
		aExtrap <- (aOpen + 1):100
		# linear approximation 
		# (we'd prefer a curve, but we're in a hurry. Best way to get that
		# is to work straight from rates, where it's easier to assume closeout
		# patterns)
		eExtrap       <- approx(c(aOpen, 110), c(elast, 1 / .8), xout = aExtrap)$y
		# create data block to extend:
		extend        <- e[1:length(aExtrap), ]
		extend$age    <- aExtrap
		extend$values <- eExtrap
		# glue it on:
		e             <- rbind(e, extend)
	}
	Px       <- p$values
	ex       <- e$values
	x        <- p$age
	myspline <- splinefun(x~ex)
	agecut   <- myspline(cutoff)
	agefloor <- floor(agecut)
	pless    <- agecut - agefloor
	
	Denom    <- sum(Px[x < agefloor]) + Px[x == agefloor] * pless
	Numer    <- sum(Px[x > agefloor]) + Px[x == agefloor] * (1 - pless)
	
	Numer / Denom
}


OASR <- function(pop, geo, sex, year, upper = 65, lower = 20){
	p   <- select(pop, geo = geo, sex = sex, year = year)
	sum(p$values[p$age >= upper]) / sum(p$values[p$age >= lower & p$age < upper])
}


# functions for Brouard and others:

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


# functions copied from DistributionTTD github repo:
#' 
#' @title replace NAs and NaNs with 0
#' 
#' @param M a vector or matrix
#' 
#' @return M, with NAs and NaNs imputed with zeros.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
#' 
Mna0 <- compiler::cmpfun(function(M){
			M[is.na(M)]  <- 0
			M[is.nan(M)] <- 0
			M
		})
#' 
#' @title replace Inf elements with 0
#' 
#' @param M a vector or matrix
#' 
#' @return M, with Inf imputed with zeros.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
#' 
Minf0 <- compiler::cmpfun(function(M){
			M[is.infinite(M)]  <- 0
			M
		})
#' 
#' @title replace Inf elements with NA
#' 
#' @param M a vector or matrix
#' 
#' @return M, with Inf imputed with NAs.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
#' 
MinfNA <- compiler::cmpfun(function(M){
			M[is.infinite(M)]  <- NA
			M
		})

#'
#' @title produce a matrix containing the age-specific distribution of remaining lifespans
#' 
#' @description Given a single-age vector of lifetable deaths (any radix), we convert to a matrix with chronological age in rows and thanatological age in columns. Each row represents the distribution of remaining lifespans for the given chronological age, and is scaled to sum to 1.
#' 
#' @details if \code{stagger} is \code{TRUE}, age 0 is not treated specially at this time, even though most of \eqn{d_0} is concentrated in the first day and weeks.
#' 
#' @param da a vector of single age lifetable deaths (dx). 
#' @param stagger logical. Should the da vector be element-wise averaged to account for deaths being spread through the year?
#' 
#' @return a matrix of the conditional distribution of remaining lifetime: chronological age in rows and remaining years in columns.
#' 
#' @export
#' 
da2fya <- function(da, stagger = FALSE){
	N       <- length(da)
	ay      <- 1:N - 1
	
	da      <- Mna0(da)   # remove NAs if any       
	da      <- c(da, da * 0) / sum(da) # pad out with 0s
	fya     <- matrix(da[col(matrix(nrow = N, 
									ncol = N)) + ay], 
			nrow = N, 
			ncol = N, 
			dimnames = list(Ex = ay, 
					Age = ay)
	)
	if (stagger){
		fya <- (fya + cbind(fya[, 2:ncol(fya)], 0)) / 2
	}
	fya <- Minf0(Mna0(fya / rowSums(fya)))
	fya
}

# do the Brouard years-left Pyramid
getPy <- function(pop, dRR, dVV, geo, sex, year){
	Pxdx <- getdx(pop,
			dRR,
			dVV,
			geo = geo,
			sex = sex,
			year = year)
# odd closeout in last row
	Pxdx <- Pxdx[-nrow(Pxdx), ]
	
	fya  <- da2fya(Pxdx$dx)
	Py   <- colSums(fya * Pxdx$Exp)
	Pxdx$Py <- Py
	
	Pxdx
}

getPxy <- function(pop, dRR, dVV, geo, sex, year){
	Pxdx <- getdx(pop,
			dRR,
			dVV,
			geo = geo,
			sex = sex,
			year = year)
# odd closeout in last row
	Pxdx <- Pxdx[-nrow(Pxdx), ]
	
	fya  <- da2fya(Pxdx$dx)
	
	# just return this
	fya * Pxdx$Exp
	
}


# -------------
# From Pyramid github repo:

#'
#' @title PyramidOutline draws a population pyramid as a single polygon
#' 
#' @description This function rescales and positions a population pyramid according to the convention of having males on the left and females on the right, and using discrete age groups. Ages are assumed to be in single-years intervals and to start at age 0. A plot device must already be open.
#' 
#' @param males vector of male population counts or fractions
#' @param females vector of male population counts or fractions
#' @param scale the total population, for purposes of plotting. For proportions in each age group, set to 1.
#' @param x the x position of the middle of the pyramid
#' @param y the y position of the bottom of the pyramid
#' @param ... arguments passed to \code{polygon()}.
#' 
#' @export
#' 
#' @return function called primarily for its graphical side effects, although a list of x and y coordinates is invisibly returned.
#' 

PyramidOutline <- function(males, females, 
		scale = sum(c(males, females)), 
		x = 0, y = 0, ...){
	N       <- length(males)
	Total   <- sum(c(males, females), na.rm = TRUE)
	widths  <- rep(1, N)
	age     <- c(0,cumsum(widths)[-N])
	u.age   <- age[N] + widths[N]
	
	males   <- scale * (males / Total)
	females <- scale * (females / Total)
	
	xout <- c(0, rep(females, each = 2) + 0,0, rev(c(-0, rep(-abs(males), each = 2) - 0, -0))) + x
	yout <- c(rep(c(age, u.age), each = 2), rev(c(rep(c(age, u.age), each = 2)))) + y
	
	polygon(x = xout, y = yout, ...)
	
	invisible(list(x = xout, y = yout))
}

PyrSetup <- function(xlim = c(-1.3, 1.3), ylim = c(0,100)){
	par(mai=c(.5,.5,.5,.5), xpd=TRUE,xaxs="i",yaxs="i")
	plot(NULL, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "")
}




# functions for Goldstein:
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

getLotkar <- function(pop, dVV, dRR, fert, geo, year, SRB = 1.05){
	PLx <- getdx(pop,dRR,dVV,geo=geo,sex="F",year=year)
	
	
	Fx   <- fert[fert$geo == geo & fert$time == year, ]
	Fx   <- Fx[order(Fx$age), ]
	beta <- max(Fx$age)
	PLx$Fxf <- 0
	PLx$Fxf[PLx$age %in% Fx$age] <- Fx$values * 1 / (1 + SRB)
	
	# just 0 to beta
	PLx <- PLx[1:(beta + 1), ]
	
	r <- optim(par = 0, 
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
	list(r=r, Dat = PLx)
}
args(mrates)
getTurnover <- function(pop, dVV, dRR, fert, geo, year, SRB = 1.05, r = NULL){
	if (is.null(r)){
		PT  <- getLotkar(pop, dVV, dRR, fert, geo = geo, year = year, SRB = SRB)
		r   <- PT$r
	}
	Dat <- getLx(mrates(pop, dRR, dVV, geo = geo, year = year, sex = "F"))
	Dat <- Dat[-nrow(Dat), ]
	
	#
	Lx  <- Dat$Lx
	Mx  <- Dat$Mx
	a   <- Dat$age
	
	# stable age structure
	Cx  <- Lx * exp(-r * (a + .5))
	Cx  <- Cx / (sum(Cx))
	
	# get stable rates:
	b <- sum(Cx[a %in% PT$Dat$age] * PT$Dat$Fxf)
	d <- sum(Cx * Mx)	
	# turnover:
	b + d
}
