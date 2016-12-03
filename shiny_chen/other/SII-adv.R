library(EpiModel)
library(dplyr)

#network statistics 
#formation
#number of nodes 
networkSize <- 550
deg0 <- 181
deg1 <- 154
deg2 <- 54
deg3 <- 28
deg4 <- 23
deg5 <- 15
deg6 <- 15
deg7 <- 14
deg8 <- 9
deg9 <- 9 
deg10 <- 10 
deg11 <- 5
deg12 <- 5
deg13 <- 5
deg14 <- 4
deg15 <- 1
deg16 <- 3
deg17 <- 4
deg19 <- 2
deg22 <- 1 
deg24 <- 1 
deg26 <- 1 
deg28 <- 1 
deg34 <- 1
deg35 <- 1
deg47 <- 1 
deg50 <- 1 
deg55 <- 1
#calculated parameters 
edges <- 220
#dissolution
#average duration of edge
avgEdgeDuration <- 365


#intervention parameters
#total number of nodes on prep
prep_number <- 15 
#prep start time days
prep_start_time <- 91
#prep rate (number people put on prep/day)
prep_rate <- 2 
#total number of nodes on art
art_number <- 50
#art start time days
art_start_time <- 5 
#art rate (number people put on art/day)
art_rate <- 2
#average frequency people exchange needles in SEP (days)
sep_exchange_frequency <- 2
#SEP start time days 
sep_start_time <- 50
#SEP compliance rate 
sep_compliance <- 0.9
#SEP number of participants 
sep_enrollment <- 0 
#total number of nodes in network


#node parameters 
#percent of nodes that are males
percentMales <- 0.577
#percent of nodes that are white
percentWhite <- 0.985
#pecent of nodes with income <$10,000
percentIncome10K <- 0.919
#percent of nodes previously incarcerated 
percentIncarcerated <- 0.542


#disease parameters 
#probability of infection per transmissible act in acute phase
acuteProb <- 0.8
#probability of infection per transmissible act in chronic phase
stableProb <- 0.12
#transition rate from acute to chronic 
transitRate <- 1/21 ##12 weeks of acute phase
#average number of transmissible acts per partnership per unit of time
actRate <- 2


#simulation parameters
#initial number of nodes infected acute
initAcute <- 0
#initial number of nodes infected chronic
initStable <- 1


#determine which intervention strategy to be implemented
whether_prep = 1
whether_art = 0
whether_sep = 0

##Disease phases (SII):
##"s": susceptible stage
##"i": acute phases of the disease (I1)
##"i2": stable phases of the disease (I2)
##"i3": virus suppression phases of the disease (I3)
## ----infection---------------------------------------------------
## Change the default state in the Epimodel
source("infect.R")
##----disease progression---------------------------------------------------
source("progress.R")

## ----ExtEx2-netest-------------------------------------------------------
nw <- network.initialize(n=networkSize, directed = FALSE)

# #sets gender attribute for nodes
# nw <- set.vertex.attribute(nw, "Gender", sample(c(0,1),size=networkSize,
#                                                 prob=c(1-percentMales,percentMales),replace=TRUE))
# #sets race attribute for nodes
# nw <- set.vertex.attribute(nw, "Race", sample(c(0,1),size=networkSize,
#                                               prob=c(1-percentWhite,percentWhite),replace=TRUE))
# 
# #sets income attribute for nodes
# nw <- set.vertex.attribute(nw, "Income", sample(c(0,1),size=networkSize,
#                                                 prob=c(1-percentIncome10K,percentIncome10K),replace=TRUE))
# #sets incarceration attribut for nodes
# nw <- set.vertex.attribute(nw, "Incarceration", sample(c(0,1),size=networkSize,
#                                                        prob=c(1-percentIncarcerated,percentIncarcerated),replace=TRUE))

#sets prep attribute for nodes
nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))

#sets art attribute for nodes
nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))


#formation formula
formation <- ~edges 

#target stats
target.stats <- c(edges)

#edge dissolution
#edge duration same for allx partnerships
coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)

#fit the model 
est <- netest(nw, formation, target.stats, coef.diss)
# est <- netest(nw, formation = ~edges, target.stats = 150,
#               coef.diss = dissolution_coefs(~offset(edges), 10))
##Simulations for models

## ----ExtEx2-params-------------------------------------------------------
param <- param.net(inf.probAcute = acuteProb, inf.probStable = stableProb, 
                   prep_efficacy = 0.735, art_efficacy = 0.96,
                   act.rate = actRate, i1i2.rate = transitRate)

##initialization of status
##need to find a way that removes duplicate of prep and art
art_vector = get.vertex.attribute(nw,"art_status")
vertex_ids <- c(1:networkSize)
nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
index_can_get_infected = setdiff(vertex_ids,nodes_i1)
nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
status_vector = c(rep("s",networkSize))
status_vector[nodes_i1] = "i"
status_vector[nodes_i2] = "i2"


init <- init.net(status.vector = status_vector)

## ----ExtEx2-control------------------------------------------------------
control <- control.net(type = "SI", nsteps = 365, nsims = 10, 
                       infection.FUN = infect, progress.FUN = progress, 
                       recovery.FUN = NULL, skip.check = TRUE, 
                       depend = FALSE, verbose.int = 0)

## ----ExtEx2-netsim, cache = TRUE, results = "hide"-----------------------
sim <- netsim(est, param, init, control)

## ----ExtEx2-plot1--------------------------------------------------------
#par(mar = c(3,3,1,1), mgp = c(2,1,0))
#plot(sim, y = c("s.num", "i.num", "i2.num","i1ANDi2.num"), popfrac = FALSE,
#mean.col = 1:4, qnts = 1, qnts.col = 1:4, leg = TRUE)

#plot(sim, y = c("s.num", "i.num","i2.num", "i1ANDi2.num"), popfrac = FALSE,
#mean.col = 1:4, qnts = 1, qnts.col = 1:4, leg = TRUE)

#actualIndiana <- read.csv("Actual Indiana Outbreak.csv")




