
## ----setup, message = FALSE----------------------------------------------
library("EpiModel")

#parameters 
nsims <- 10
nsteps <- 100
#total number of nodes on prep
prep_number <- 50
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
actRate <- 10
##MADE UP
acuteProb <- 0.1
stableProb <- 0.04
transitRate <-0.01
#average number of transmissible acts per partnership per unit of time
##MADE UP
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
source("infect.R")

source("progress.R")

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

#sets prep attribute for nodes
nw <- set.vertex.attribute(nw,"prep_status",sample(c(0,1),size=networkSize,
                                                   prob=c(1-prep_number/networkSize,prep_number/networkSize),replace=TRUE))

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
param <- param.net(inf.probAcute = acuteProb, inf.probStable = stableProb, 
                   prep_efficacy = 0.95,
                   act.rate = actRate, i1i2.rate = transitRate)

init <- init.net(i.num = initAcute,i2.num = initStable, status.rand = FALSE)

## ----ExtEx2-control------------------------------------------------------
control <- control.net(type = "SI", nsteps = 100, nsims = 10, 
                       infection.FUN = infect, progress.FUN = progress, 
                       recovery.FUN = NULL, skip.check = TRUE, 
                       depend = FALSE, verbose.int = 0)

## ----ExtEx2-netsim, cache = TRUE, results = "hide"-----------------------
sim <- netsim(est, param, init, control)

## ----ExtEx2-plot1--------------------------------------------------------
#par(mar = c(3,3,1,1), mgp = c(2,1,0))
#plot(sim, y = c("s.num", "i.num", "i2.num"), popfrac = FALSE,
#     mean.col = 1:4, qnts = 1, qnts.col = 1:4, leg = TRUE)

plot(sim, y=c("si.flow"))

## ---- Start data binder script -------------------------------------------

datalist = list()

x <- 1
while(x <= nsims){
  dat <- as.data.frame(sim, out = "vals", sim = x)
  dat$x <= x
  datalist[[x]] <- dat
  x <- x + 1
}

big_data = do.call(rbind, datalist)


finaldatalist = NULL
i <- 1
while(i <= nsteps){
  subdat <- big_data[big_data$time == i,]
  time.weeks <- mean(subdat$time)
  avg.sNum <- mean(subdat$s.num)
  avg.iNum <- mean(subdat$i.num)
  avg.Num <- mean(subdat$num)
  avg.i1i2Flow <- mean(subdat$i1i2.flow)
  avg.i1i3Flow <- mean(subdat$i1i3.flow)
  avg.i2i3Flow <- mean(subdat$i2i3.flow)
  avg.i2Num <- mean(subdat$i2.num)
  avg.i3Num <- mean(subdat$i3.num)
  avg.siFlow <- mean(subdat$si.flow)
  avg.si1.Flow <- mean(subdat$si1.flow)
  
  finaldatalist = rbind(finaldatalist, data.frame(time.weeks,avg.sNum, avg.iNum, avg.i2Num,avg.i3Num, avg.Num,
                                                  avg.i1i2Flow, avg.i1i3Flow, avg.i2i3Flow, 
                                                  avg.siFlow, avg.si1.Flow))
  i <- i + 1
}


############### Cost-effectiveness Analysis #################

#### 1 - Gather constants
mult <- c(1, 0.79, 0.83, 0.83)

# BEFORE INTERVENTION
#### 2 - Computing Total QALY
TQALY <- (finaldatalist$avg.sNum*mult[1]) + (finaldatalist$avg.iNum*mult[2]) + (finaldatalist$avg.i2Num*mult[3]) + (finaldatalist$avg.i3Num*mult[4])

# Computing the Sum of TQALY
sum.TQALY <- sum(TQALY)

# Discount rate
drate <- 1.03
#### 3 - Computing Discounted QALY

cost.eff.datalist = NULL
ci <- 1
while(ci <= nsteps){
  DQALY <- (TQALY[ci])/(drate^(ci-1))
  cost.eff.datalist <- rbind(cost.eff.datalist, data.frame(DQALY))
  ci <- ci + 1
}
time.weeks <- finaldatalist$time.weeks
qaly.output <- data.frame(time.weeks, TQALY, cost.eff.datalist)



# AFTER INTERVENTION


# COST SECTION (per person)
cost.art <- 20600.00
cost.sep <- 30.00
cost.PREP <- 16260.00
cost.lifetime <- 466304.09







