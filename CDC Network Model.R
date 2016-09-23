library("EpiModel")
library(d3Network)

#### INPUTS #####
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
avgDegree <- 0.5
#average duration of edge
##MADE UP
avgEdgeDuration <- 90
#probability of infection per transmissible act
##MADE UP
infProb <- 0.1
#average number of transmissible acts per partnership per unit of time
##MADE UP
actRate <- 5
#initial number of nodes infected
initInfected <- 11
#intervention
intRate = 1




#### OUTPUTS ####

#calculated parameters 
avgEdges <- (networkSize*avgDegree)/2


#create network object 
nw <- network.initialize(n=networkSize, directed=FALSE)
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
modelFit <- netest(nw, formation, target.stats, coef.diss)
#use to check for significants of parameters in formation formula
#summary(modelFit)

#model diagnostics 
diagnositcs <- netdx(modelFit, nsims = 10, nsteps =100)

# might need to add prevalance here
param <- param.net(inf.prob = infProb, act.rate = actRate, inter.start = 50,inter.eff = intRate)
init <- init.net(i.num = initInfected, status.rand = TRUE)
control <- control.net(type = "SI", nsteps = 50, nsims = 10)
sim <- netsim(modelFit, param, init, control)
data <- head(as.data.frame(sim))

# plot(sim)
#par(mfrow = c(1,2), mar = c(0,0,1,0))
#plot(sim, type = "network", at = 1, col.status = TRUE,
#     main = "Prevalence at t1")
#plot(sim, type = "network", at = 500, col.status = TRUE,
#     main = "Prevalence at t500")
