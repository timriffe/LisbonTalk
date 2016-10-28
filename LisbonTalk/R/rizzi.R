
# E1 = single-age exposures: a1 = single ages for exposures
# F5 = grouped fertility rates: a5 = 5-year ages for fert
rizziFert <- function(E1, a1, F5, a5, lambda = 5.75, omega = 55){
  
  a15         <- a - a %% 5
  E5          <- tapply(E1, a15, sum)
  #set Dx as small as possible for pclm function
  D5          <- E5 * F5
  D5[D5 == 0] <- 0.001
  
  # how many abridged age classes
  n            <- length(D5)
  
  m            <- length(a1)
  B            <- diag(m)
  
  # grouping matrix
  # this is a complex matrix. Columns are for single ages
  # rows are for the abrdiged data. This code is funky looking
  # because it is flexible to optionally exclude infants
  C            <- kronecker(diag(n), matrix(1, 1, 5))
  
  C            <- cbind(C, matrix(0, nrow = nrow(C), ncol = m - ncol(C)))
  C            <- rbind(C, 0)
  # 
  C[nrow(C),colSums(C) == 0] <- 1
  #  
  # then we take the single age exposures and use them to update C and provide
  # an offset for graduating counts. This makes pclm() return rates, magically.
  C2 <- t(t(C) * E1)
  # repeat, splitting counts, but spitting back rates.
  mxhat <- pclm(y = D5, C2, B, lambda, deg = 2,show = F)$gamma
  
  if (excludeInfants){
    mxhat <- c(Dat$Mx[Dat$Age == 0],mxhat)
  }
  
  #put back the zeros into the data (just for Case 2)
  mxhat <- mxhat * !Impute0s
  
  #gives back the single age mx values
  mxhat
}




