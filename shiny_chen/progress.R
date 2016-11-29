progress <- function(dat, at) {
  #print(paste("progress phase",at))
  active <- dat$attr$active
  status <- dat$attr$status
  art_status <- dat$attr$art_status
  
  ##I1 to I2 progression
  num_i1i2 <- 0
  i1i2.rate <- dat$param$i1i2.rate  #inflow rate from i1 to i2
  nInf <- 0
  idsEligInf <- which(active == 1 & status == "i") #active and status of acute phase
  #print(paste("id of i1",idsEligInf))
  nEligInf <- length(idsEligInf)
  if (nEligInf > 0) {
    vec_i1i2 <- which(rbinom(nEligInf, 1, i1i2.rate) == 1) #select people moving from acute to stable phase
    if (length(vec_i1i2) > 0) {
      ids_i1i2 <- idsEligInf[vec_i1i2]
      num_i1i2 <- length(ids_i1i2)
      status[ids_i1i2] <- "i2" #update the style from acute to stable
    }
  }
  
  if (at == 2) {
    dat$epi$i1i2.flow <- c(0, num_i1i2)
  }
  else {
    dat$epi$i1i2.flow[at] <- num_i1i2
  }
  
  dat$attr$status <- status
  
  #change summary statistics
  
  return(dat)
}