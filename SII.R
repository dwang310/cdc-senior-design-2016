## ----setup, message = FALSE----------------------------------------------
library("EpiModel")

#parameters 
#total number of nodes in network
networkSize <- 196 
#percent of nodes that are males
percentMales <- 0.577
#percent of nodes that are white
percentWhite <- 0.985
#pecent of nodes with income <$10,000
percentIncome10K <- 0.919
#percent of nodes previously incarcerated 
percentIncarcerated <- 0.542
#average degree of nodes
##MADE UP
avgDegree <- 1.5
#average duration of edge
##MADE UP
avgEdgeDuration <- 90
#probability of infection per transmissible act
acuteProb <- 0.72
stableProb <- 0.06
transitRate <-1/84 ##12 weeks of acute phase
#average number of transmissible acts per partnership per unit of time
actRate <- 2
##MADE UP
acuteProb <- 0.1
stableProb <- 0.04
transitRate <-0.01
#average number of transmissible acts per partnership per unit of time
##MADE UP
actRate <- 3
#initial number of nodes infected
initAcute <- 5
initStable <- 10
#calculated parameters 
avgEdges <- (networkSize*avgDegree)/2


##Disease phases (SII):
##"s": susceptible stage
##"i": acute phases of the disease (I1)
##"i2": stable phases of the disease (I2)
##"i3": virus suppression phases of the disease (I3)
## ----infection---------------------------------------------------
## Change the default state in the Epimodel
infect <- function (dat,at) {  ##dat = data, at = at timestamp
  active <- dat$attr$active  #access attributes for people who are active (not dead).
  status <- dat$attr$status  #status will compartmentalize nodes in S, I1, I2, I3
  status <- dat$attr$status  #status will compartmentalize nodes in S, I1, I2
  nw <- dat$nw  #network
  
  idsSus <- which(active == 1 & status == "s") #people who are active and status S
  idsInf1 <- which(active == 1 & status == "i") #people who are active and status I1
  idsInf2 <- which(active == 1 & status == "i2") #people who are active and status I2
  idsInf3 <- which(active == 1 & status == "i3") #people who are active and status I3
  nActive <- sum(active == 1) #total number of active people
  
  nElig1 <- length(idsInf1) #Number of infectious people at acute phase
  nElig2 <- length(idsInf2) #Number of infectious people at stable infectious phase
  nInf1 <- 0 #Used as a variable in if loop
  nInf2 <- 0 #Used as a variable in if loop
  
  
  ##Identify the dyads between susceptible and acute phase patients  
  if (nElig1 > 0 && nElig1 < (nActive - nElig2)) { ##if people who are in acute infectious and that is less than total active
   ##Epimodel gives a vector of edges of people in acute phase, susceptible phase
   ##dat = data, at = at timestamp
    del <- discord_edgelist(dat, idsInf1, idsSus, at)
    if (!(is.null(del))) {
      del$transProb <- dat$param$inf.probAcute #transmission probability
      del$actRate <- dat$param$act.rate #acute phase rate
      del$finalProb <- 1 - (1 - del$transProb)^del$actRate #final transmission prob, using binomial prob
      transmit <- rbinom(nrow(del), 1, del$finalProb) #trasmit rate is a binary variable
      del <- del[which(transmit == 1), ] #select all the nodes where transmit rate is 1
      idsNewInf <- unique(del$sus) 
      nInf1 <- length(idsNewInf) #new number of infected nodes
      if (nInf1 > 0) {
        dat$attr$status[idsNewInf] <- "i" #status is active
        dat$attr$infTime[idsNewInf] <- at #timestamp
      }
    }
  }
  
  ##identify the dyads between susceptible and stable phase patients
  if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {   #nodes that are infected from the stable phase
    del1 <- discord_edgelist(dat, idsInf2, idsSus, at)
    if (!(is.null(del1))) {
      del1$transProb <- dat$param$inf.probStable
      del1$actRate <- dat$param$act.rate
      del1$finalProb <- 1 - (1 - del1$transProb)^del1$actRate
      transmit <- rbinom(nrow(del1), 1, del1$finalProb)
      del1 <- del1[which(transmit == 1), ]
      idsNewInf1 <- unique(del1$sus)
      nInf2 <- length(idsNewInf1)
      if (nInf2 > 0) {
        dat$attr$status[idsNewInf1] <- "i"
        dat$attr$infTime[idsNewInf1] <- at
      }
    }
  }
  
  
  ##update summary statistics
  if (at == 2) { ##time starts at 2
    dat$epi$si.flow <- c(0, nInf1+nInf2) #nInf1 is acute, nInf1 is susceptible
    #dat$epi$si1.flow <- c(0, nInf1)
    #dat$epi$i1i2.flow <- c(0,nInf2)
  }
  else {
    dat$epi$si.flow[at] <- nInf1+nInf2
    # dat$epi$si1.flow[at] <- nInf1
    # dat$epi$i1i2.flow[at] <- nInf2
    dat$epi$si.flow <- c(0, nInf1+nInf2) #nInf is acute, nInf1 is susceptible
  }
  else {
    dat$epi$si.flow[at] <- nInf1+nInf2
  }
  dat$nw <- nw
  return(dat)
}
##----disease progression---------------------------------------------------
progress <- function(dat, at) {
  
  active <- dat$attr$active
  status <- dat$attr$status
  prep_treat <- dat$attr$prep_treat
  art_treat <- dat$attr$art_treat
  # prep_compliance <- dat$attr$prep_compliance
  # art_compliance <- dat$attr$art_compliance
  ##needle_exchange <- dat$attr$needle_exchange

  i1i2.rate <- dat$param$i1i2.rate  #inflow rate from i1 to i2

  ## Acute to Chronic progression
  num_i1i2 <- 0

  i1i2.rate <- dat$param$i1i2.rate  #inflow rate from i1 to i2

  ## A to I progression
  nInf <- 0
  idsEligInf <- which(active == 1 & status == "i") #active and status of acute phase
  nEligInf <- length(idsEligInf)
  
  if (nEligInf > 0) {
    vec_i1i2 <- which(rbinom(nEligInf, 1, i1i2.rate) == 1) #select people moving from acute to stable phase
    if (length(vec_i1i2) > 0) {
      ids_i1i2 <- idsEligInf[vec_i1i2]
      num_i1i2 <- length(ids_i1i2)
      status[ids_i1i2] <- "i2" #update the style from acute to stable
    }
  }

  dat$attr$status <- status
  
  ## Acute to Viral Suppressed progression
  num_i1i3 <- 0
  ids_acute_art <- which(active == 1 & status == "i" & art_treat == 1)
  num_acute_art <- length(ids_acute_art)
  
  if (num_acute_art > 0) {
    num_i1i3 <- num_acute_art
    status[ids_acute_art] <- "i3"
  }
  
  dat$attr$status <- status
  
  ## Chronical to Viral Suppressed progression
  num_i2i3 <- 0
  ids_chronical_prep <- which(active == 1 & status == "i2" & prep_treat == 1)
  num_chronical_prep <- length(ids_chronical_prep)
  
  if (num_chronical_prep > 0) {
    num_i2i3 <- num_chronical_prep
    statusp[ids_chronical_prep] <- "i3"
  }
  
  #change summary statistics
  
  if (at == 2) {
    dat$epi$i1i2.flow <- c(0, num_i1i2) 
    dat$epi$i1i3.flow <- c(0,num_i1i3)
    dat$epi$i2i3.flow <- c(0,num_i2i3)
    dat$epi$i.num <- c(0, sum(active == 1 & status == "i"))
    dat$epi$i2.num <- c(0, sum(active == 1 & status == "i2"))
    dat$epi$i3.num <- c(0,sum(active == 1 & status == "i3"))
  }
  else {
    dat$epi$i1i2.flow[at] <- num_i1i2
    dat$epi$i1i3.flow[at] <- num_i1i3
    dat$epi$i2i3.flow[at] <- num_i2i3
    dat$epi$i.num[at] <- sum(active == 1 & status == "i")
    dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
    dat$epi$i3.num <- sum(active == 1 & status == "i3")

    vecInf <- which(rbinom(nEligInf, 1, i1i2.rate) == 1) #select people moving from acute to stable phase
    if (length(vecInf) > 0) {
      idsInf <- idsEligInf[vecInf]
      nInf <- length(idsInf)
      status[idsInf] <- "i2" #update the style from acute to stable
    }
  }
  
  dat$attr$status <- status
  
  #change summary statistics
  
  if (at == 2) {
    dat$epi$i1i2.flow <- c(0, nInf) 
    dat$epi$i.num <- c(0, sum(active == 1 & status == "i"))
    dat$epi$i2.num <- c(0, sum(active == 1 & status == "i2"))
  }
  else {
    dat$epi$i1i2.flow[at] <- nInf
    dat$epi$i.num[at] <- sum(active == 1 & status == "i")
    dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
  }
  
  return(dat)
}

## ----ExtEx2-netest-------------------------------------------------------
nw <- network.initialize(n=networkSize, directed = FALSE)

#sets gender attribute for nodes
nw <- set.vertex.attribute(nw, "Gender", sample(c(0,1),size=networkSize,
                                                prob=c(1-percentMales,percentMales),replace=TRUE))
#sets race attribute for nodes
nw <- set.vertex.attribute(nw, "Race", sample(c(0,1),size=networkSize,
                                              prob=c(1-percentWhite,percentWhite),replace=TRUE))

#sets income attribute for nodes
nw <- set.vertex.attribute(nw, "Income", sample(c(0,1),size=networkSize,
                                                prob=c(1-percentIncome10K,percentIncome10K),replace=TRUE))
#sets incarceration attribut for nodes
nw <- set.vertex.attribute(nw, "Incarceration", sample(c(0,1),size=networkSize,
                                                       prob=c(1-percentIncarcerated,percentIncarcerated),replace=TRUE))

#formation formula
formation <- ~edges

#target stats
target.stats <- c(avgEdges)

#edge dissolution
#edge duration same for all partnerships
coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)

#fit the model 
est <- netest(nw, formation, target.stats, coef.diss)
# est <- netest(nw, formation = ~edges, target.stats = 150,
#               coef.diss = dissolution_coefs(~offset(edges), 10))
##Simulations for models

## ----ExtEx2-params-------------------------------------------------------
param <- param.net(inf.probAcute = acuteProb, inf.probStable = stableProb, act.rate = actRate, i1i2.rate = transitRate)

init <- init.net(i.num = initAcute,i2.num = initStable, status.rand = FALSE)

## ----ExtEx2-control------------------------------------------------------
control <- control.net(type = "SI", nsteps = 500, nsims = 3, 
                       infection.FUN = infect, progress.FUN = progress, 
                       recovery.FUN = NULL, skip.check = TRUE, 
                       depend = FALSE, verbose.int = 0)

## ----ExtEx2-netsim, cache = TRUE, results = "hide"-----------------------
sim <- netsim(est, param, init, control)

## ----ExtEx2-plot1--------------------------------------------------------
par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y = c("s.num", "i.num", "i2.num"), popfrac = FALSE,
     mean.col = 1:4, qnts = 1, qnts.col = 1:4, leg = TRUE)
