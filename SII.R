library("EpiModel")
library("dplyr")

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
edges <- 840
#dissolution
#average duration of edge
avgEdgeDuration <- 365


#intervention parameters
#total number of nodes on prep
prep_number <- 10 
#prep start time days
prep_start_time <- 8
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
acuteProb <- 1
#probability of infection per transmissible act in chronic phase
stableProb <- 0.12
#transition rate from acute to chronic 
transitRate <- 1/21 ##12 weeks of acute phase
#average number of transmissible acts per partnership per unit of time
actRate <- 2/3


#simulation parameters
#initial number of nodes infected acute
initAcute <- 1
#initial number of nodes infected chronic
initStable <- 0


#determine which intervention strategy to be implemented
whether_prep = 0
whether_art = 0
whether_sep = 0

##Disease phases (SII):
##"s": susceptible stage
##"i": acute phases of the disease (I1)
##"i2": stable phases of the disease (I2)
##"i3": virus suppression phases of the disease (I3)
## ----infection---------------------------------------------------
## Change the default state in the Epimodel
infect <- function (dat,at) {  ##dat = data, at = at timestamp
  
  #print(paste("This is infection function",at))
  
  # edges2 <- as.matrix.network.edgelist(dat$nw)
  # indices1 <- c(0)
  # indices2 <- c(0)
  # for (ids in nodes_i2) { 
  #   indices1 <- c(indices1,which(edges2[,1]==ids))
  #   indices2 <- c(indices2,which(edges2[,2]==ids))
  # }
  # indices1 <- indices1[-1]
  # indices2 <- indices2[-1]
  # edges3 <- edges2[indices1,]
  # edges4 <- edges2[indices2,]
  # edges5 <- rbind(edges3,edges4)
  # print(edges5)
  
  active <- dat$attr$active  #access attributes for people who are active (not dead).
  status <- dat$attr$status  #status will compartmentalize nodes in S, I1, I2, I3
  nw <- dat$nw  #network
  ##ids of people before intervention starts
  ids_sus <- which(active == 1 & status == "s")
  idsInf1 <- which(active == 1 & status == "i") #people who are active and status I1
  idsInf2 <- which(active == 1 & status == "i2") #people who are active and status I2
  idsInf3 <- which(active == 1 & status == "i3")
  
  ##ids of people with intervention implemented
  art_status <- get.vertex.attribute(nw,"art_status")
  ids_inf1_art <- which(active == 1 & status == "i" & art_status == 1)
  ids_inf1_noart <- which(active == 1 & status == "i" & art_status == 0)
  ids_inf2_art <- which(active == 1 & status == "i2" & art_status == 1)
  ids_inf2_noart <- which(active == 1 & status == "i2" & art_status == 0)
  
  ##summary statisitcs
  nActive <- sum(active == 1) #total number of active people
  nElig1 <- length(idsInf1) #Number of infectious people at acute phase
  nElig2 <- length(idsInf2) #Number of infectious people at stable infectious phase
  nElig3 <- length(idsInf3)
  
  # print("this is status")
  # print(status)
  # print("ids s ")
  # print(ids_sus)
  # print("ids i2 ")
  # print(idsInf2)
  if (whether_prep==0 & whether_sep==0 & whether_art==0) {
    num_newInf1_sus <- 0 
    num_newInf2_sus <- 0
    if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
      del9 <- discord_edgelist(dat, idsInf1, ids_sus, at)
      if (!(is.null(del9))) {
        num_newInf1_sus <- 0 
        for (edge in 1:nrow(del9)){
          trans_probability_sus_i1 <- dat$param$inf.probAcute
          nodeDegree <- get_degree(dat$nw)[del9[edge,3]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i1 <- rbinom(1,1,final_probability_sus_i1) #trasmit rate is a binary variable
          #infections <- del9[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
          if (transmit_sus_i1==1){
            ids_newInf1_sus <- del9[edge,2]
            num_newInf1_sus <- num_newInf1_sus +1
            dat$attr$status[ids_newInf1_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
          }
        }
      }
    }
    status2 <- dat$attr$status
    ids_sus <- which(active == 1 & status2 == "s")
    idsInf1 <- which(active == 1 & status2 == "i")
    idsInf2 <- which(active == 1 & status2 == "i2")
    idsInf3 <- which(active == 1 & status2 == "i3")
    nActive <- sum(active == 1)
    nElig1 <- length(idsInf1)
    nElig2 <- length(idsInf2)
    nElig3 <- length(idsInf3)
    if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {
      #from sus to Inf2
      edges <- as.matrix.network.edgelist(dat$nw)
      edges <- edges[order(edges[,1]),]
      indices1 <- c(0)
      indices2 <- c(0)
      for (ids in idsInf2) { 
        indices1 <- c(indices1,which(edges[,1]==ids))
        indices2 <- c(indices2,which(edges[,2]==ids))
      }
      indices1 <- indices1[-1]
      indices2 <- indices2[-1]
      fromedges <- edges[indices1,]
      # print("fromedges at the beginning")
      # print(fromedges)
      if (!(is.null(nrow(fromedges)))){
        fromsusindices <- c(0)
        for (ids in ids_sus){
          fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
        }
        fromsusindices <- fromsusindices[-1]
        fromedges <- fromedges[fromsusindices,]
        # print("fromedges from matrix loop ")
        # print(fromedges)
      }
      if (is.vector(fromedges)){
        fromsusindices <- c(0)
        for (ids in ids_sus){
          fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
        }
        fromsusindices <- fromsusindices[-1]
        if (length(fromsusindices)==0){
          fromedges <- fromedges[fromsusindices]
        }
        # print("fromedges from vector loop ")
        # print(fromedges)
      }
      # print("from edges")
      # print(fromedges)
      
      toedges <- edges[indices2,]
      if (!(is.null(nrow(toedges)))){
        tosusindices <- c(0)
        for (ids in ids_sus){
          tosusindices <- c(tosusindices,which(toedges[,1]==ids))
        }
        tosusindices <- tosusindices[-1]
        toedges <- toedges[tosusindices,]
        if(!(is.null(nrow(toedges)))){
          toedges <- toedges[,c(2,1)]
        }
      }
      if (is.vector(toedges)){
        tosusindices <- c(0)
        for (ids in ids_sus){
          tosusindices <- c(tosusindices,which(toedges[1]==ids))
        }
        tosusindices <- tosusindices[-1]
        toedges <- toedges[tosusindices]
        toedges <- c(toedges[2],toedges[1])
      }
      # print("toedges")
      # print(toedges)
      edges <- rbind(fromedges,toedges)
      
      if (!(is.null(nrow(edges)))) {
        num_newInf2_sus <- 0 
        for (edge in 1:nrow(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
          if (transmit_sus_i2==1){
            ids_newInf2_sus <- edges[edge,2]
            num_newInf2_sus <- num_newInf2_sus + 1 
            dat$attr$status[ids_newInf2_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
          }
        }
        # edges <- edges[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
        # if (!(is.null(nrow(edges)))) {
        #   ids_newInf2_sus <- unique(edges[,2])
        #   num_newInf2_sus <- length(ids_newInf2_sus)
        #   if (num_newInf2_sus > 0) {
        #     dat$attr$status[ids_newInf2_sus] <- "i" #status is active
        #     dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
        #   }
      }
      if (is.vector(edges)){
        trans_probability_sus_i2 <- dat$param$inf.probStable
        nodeDegree <- get_degree(dat$nw)[edges[1]]
        act_rate <- dat$param$act.rate/nodeDegree
        final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
        transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
        if (transmit_sus_i2==1){
          ids_newInf2_sus <- edges[edge,2]
          num_newInf2_sus <- num_newInf2_sus + 1 
          dat$attr$status[ids_newInf2_sus] <- "i" #status is active
          dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
        }
        # ids_newInf2_sus <- edges[2]
        # num_newInf2_sus <- length(ids_newInf2_sus)
        # if (num_newInf2_sus > 0) {
        #   dat$attr$status[ids_newInf2_sus] <- "i" #status is active
        #   dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
        #   }
      }
    }
     
    status3 <- dat$attr$status
    ids_sus <- which(active == 1 & status3 == "s")
    idsInf1 <- which(active == 1 & status3 == "i")
    idsInf2 <- which(active == 1 & status3 == "i2")
    idsInf3 <- which(active == 1 & status3 == "i3")
    nActive <- sum(active == 1)
    nElig1 <- length(idsInf1)
    nElig2 <- length(idsInf2)
    nElig3 <- length(idsInf3)
    if (at == 2) { ##time starts at 2
      dat$epi$i.num <- c(0, sum(active == 1 & status == "i"))
      dat$epi$i2.num <- c(0, sum(active == 1 & status == "i2"))
      dat$epi$s.num <- c(0, sum(active == 1 & status =="s"))
      dat$epi$si.flow <- c(0, num_newInf1_sus + num_newInf2_sus)
      
    }
    else {
      dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
      dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
      dat$epi$s.num[at] <- sum(active == 1 & status =="s")
      dat$epi$si.flow[at] <- num_newInf2_sus + num_newInf1_sus
    }
    dat$nw <- nw
  }
  if (whether_prep) {
    if (at < prep_start_time) {
      num_newInf1_sus <- 0 
      num_newInf2_sus <- 0
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del1 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del1))) {
          del1$trans_probability_sus_i1 <- dat$param$inf.probAcute
          del1$act_rate <- dat$param$act.rate
          del1$final_probability_sus_i1 <- 1 - (1 - del1$trans_probability_sus_i1)^del1$act_rate #final transmission prob, using binomial prob
          transmit_sus_i1 <- rbinom(nrow(del1),1,del1$final_probability_sus_i1) #trasmit rate is a binary variable
          del1 <- del1[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
          ids_newInf1_sus <- unique(del1$sus)
          num_newInf1_sus <- length(ids_newInf1_sus)
          if (num_newInf1_sus > 0) {
            dat$attr$status[ids_newInf1_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
          }
        }
      }
      status2 <- dat$attr$status
      ids_sus <- which(active == 1 & status2 == "s")
      idsInf1 <- which(active == 1 & status2 == "i")
      idsInf2 <- which(active == 1 & status2 == "i2")
      idsInf3 <- which(active == 1 & status2 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {
        #sus to Inf2
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in idsInf2) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges))) {
          trans_probability_sus_i2 <- dat$param$inf.probStable
          act_rate <- dat$param$act.rate
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(edges),1,final_probability_sus_i2) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf2_sus <- unique(edges[,2])
            num_newInf2_sus <- length(ids_newInf2_sus)
            if (num_newInf2_sus > 0) {
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- length(ids_newInf2_sus)
            if (num_newInf2_sus > 0) {
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
      } 
      if (at == 2) { ##time starts at 2
        dat$epi$i.num <- c(0,sum(active == 1 & status == "i"))
        dat$epi$i2.num <- c(0,sum(active == 1 & status == "i2"))
        dat$epi$s.num <- c(0,sum(active == 1 & status =="s"))
        dat$epi$si.flow <- c(0, num_newInf1_sus + num_newInf2_sus) #nInf1 is acute, nInf1 is susceptible
      }
      else {
        dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
        dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
        dat$epi$s.num[at] <- sum(active == 1 & status =="s")
        dat$epi$si.flow[at] <- num_newInf1_sus + num_newInf2_sus 
      }
      dat$nw <- nw
    }
    else {
      num_newInf1_from_sus_prep <- 0 
      num_newInf1_from_sus_noprep <- 0
      num_newInf2_from_sus_prep <- 0 
      num_newInf2_from_sus_noprep <- 0
      if (at <= prep_start_time+floor(prep_number/prep_rate)-1){
        prep_status <- get.vertex.attribute(nw,"prep_status") 
        ids_sus_old_prep <- which(active == 1 & status == "s" & prep_status == 1)
        ids_sus_new_prep <- sample(ids_sus,size = prep_rate, replace=FALSE)
        ids_sus_prep <- c(ids_sus_new_prep,ids_sus_old_prep) 
        ids_sus_noprep <- setdiff(ids_sus,ids_sus_prep)
        prep_status <- set.vertex.attribute(nw,"prep_status",c(1),v=ids_sus_prep)
      }
      else {
        prep_status <- get.vertex.attribute(nw,"prep_status")
        ids_sus_prep <- which(active == 1 & status == "s" & prep_status == 1) #people who are active and status S
        ids_sus_noprep <- which(active == 1 & status == "s" & prep_status == 0)
      }
      ##Identify the dyads between susceptible and acute phase patients  
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) { ##if people who are in acute infectious and that is less than total active
        ##Epimodel gives a vector of edges of people in acute phase, susceptible phase
        ##dat = data, at = at timestamp
        
        ##acute patient with susceptible on prep
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_sus_prep) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in idsInf1){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in idsInf1){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in idsInf1){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in idsInf1){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges))) {
          trans_probability_susprep_i1 <- (dat$param$inf.probAcute) * (1-dat$param$prep_efficacy)
          act_rate <- dat$param$act.rate
          final_probability_susprep_i1 <- 1 - (1 - trans_probability_susprep_i1)^act_rate #final transmission prob, using binomial prob
          transmit_susprep_i1 <- rbinom(nrow(edges),1,final_probability_susprep_i1) #trasmit rate is a binary variable
          edges <- edges[which(transmit_susprep_i1 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))){
            ids_newInf1_from_sus_prep <- unique(edges[,2])
            num_newInf1_from_sus_prep <- length(ids_newInf1_from_sus_prep)
            if (num_newInf1_from_sus_prep > 0) {
              dat$attr$status[ids_newInf1_from_sus_prep] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_from_sus_prep] <- at #timestamp
            } 
            
          }
        }
        
        ##acute patient with susceptible not on prep
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_sus_noprep) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in idsInf1){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in idsInf1){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in idsInf1){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in idsInf1){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges))) {
          trans_probability_susnoprep_i1 <- dat$param$inf.probAcute
          act_rate <- dat$param$act.rate
          final_probability_susnoprep_i1 <- 1 - (1 - trans_probability_susnoprep_i1)^act_rate
          transmit_susnoprep_i1 <- rbinom(nrow(edges),1,final_probability_susnoprep_i1)
          edges <- edges[which(transmit_susnoprep_i1 == 1),]
          if (!(is.null(nrow(edges)))){
            ids_newInf1_from_sus_noprep <- unique(edges[,2])
            num_newInf1_from_sus_noprep <- length(ids_newInf1_from_sus_noprep)
            if (num_newInf1_from_sus_noprep > 0) {
              dat$attr$status[ids_newInf1_from_sus_noprep] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_from_sus_noprep] <- at #timestamp
            }
          }
        }
      }
      status2 <- dat$attr$status
      ids_sus <- which(active == 1 & status2 == "s")
      idsInf1 <- which(active == 1 & status2 == "i")
      idsInf2 <- which(active == 1 & status2 == "i2")
      idsInf3 <- which(active == 1 & status2 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      ##identify the dyads between susceptible and stable phase patients
      if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {   #nodes that are infected from the stable phase
        ##chronic patients with susceptible no prep
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_sus_noprep) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in idsInf2){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in idsInf2){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in idsInf2){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in idsInf2){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges))) {
          trans_probability_sus_i2 <- dat$param$inf.probStable
          act_rate <- dat$param$act.rate
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(edges),1,final_probability_sus_i2) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf2_from_sus_noprep <- unique(edges[,2])
            num_newInf2_from_sus_noprep <- length(ids_newInf2_from_sus_noprep)
            if (num_newInf2_from_sus_noprep > 0) {
              dat$attr$status[ids_newInf2_from_sus_noprep] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_from_sus_noprep] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf2_from_sus_noprep <- edges[2]
            num_newInf2_from_sus_noprep <- length(ids_newInf2_from_sus_noprep)
            if (num_newInf2_from_sus_noprep > 0) {
              dat$attr$status[ids_newInf2_from_sus_noprep] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_from_sus_noprep] <- at #timestamp
            }
          }
        }
        
        ##chronic patients with susceptible prep
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_sus_prep) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in idsInf2){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in idsInf2){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in idsInf2){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in idsInf2){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges))) {
          trans_probability_sus_i2 <- dat$param$inf.probStable * (1-dat$param$prep_efficacy)
          act_rate <- dat$param$act.rate
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(edges),1,final_probability_sus_i2) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf2_from_sus_prep <- unique(edges[,2])
            num_newInf2_from_sus_prep <- length(ids_newInf2_from_sus_prep)
            if (num_newInf2_from_sus_prep > 0) {
              dat$attr$status[ids_newInf2_from_sus_prep] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_from_sus_prep] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf2_from_sus_prep <- edges[2]
            num_newInf2_from_sus_prep <- length(ids_newInf2_from_sus_prep)
            if (num_newInf2_from_sus_prep > 0) {
              dat$attr$status[ids_newInf2_from_sus_prep] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_from_sus_prep] <- at #timestamp
            }
          }
        }
      }
      if (at == 2) { ##time starts at 2
        dat$epi$i.num <- c(0,sum(active == 1 & status == "i")) 
        dat$epi$i2.num <- c(0,sum(active == 1 & status == "i2"))
        dat$epi$s.num <- c(0,sum(active == 1 & status =="s"))
        dat$epi$si.flow <- c(0, num_newInf1_from_sus_prep+num_newInf1_from_sus_noprep+ num_newInf2_from_sus_prep + num_newInf2_from_sus_noprep) #nInf1 is acute, nInf1 is susceptible
      }
      else {
        dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
        dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
        dat$epi$s.num[at] <- sum(active == 1 & status =="s")
        dat$epi$si.flow[at] <- num_newInf1_from_sus_prep+num_newInf1_from_sus_noprep+ num_newInf2_from_sus_prep + num_newInf2_from_sus_noprep
      }
      dat$nw <- nw
    }
  }
  if (whether_art) {
    if (at < art_start_time) {
      num_newInf1_sus <- 0 
      num_newInf2_sus <- 0
      num_newInf3_sus <- 0 
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del1 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del1))) {
          del1$trans_probability_sus_i1 <- dat$param$inf.probAcute
          del1$act_rate <- dat$param$act.rate
          del1$final_probability_sus_i1 <- 1 - (1 - del1$trans_probability_sus_i1)^del1$act_rate #final transmission prob, using binomial prob
          transmit_sus_i1 <- rbinom(nrow(del1),1,del1$final_probability_sus_i1) #trasmit rate is a binary variable
          del1 <- del1[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
          ids_newInf1_sus <- unique(del1$sus)
          num_newInf1_sus <- length(ids_newInf1_sus)
          if (num_newInf1_sus > 0) {
            dat$attr$status[ids_newInf1_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
          }
        }
      }
      status2 <- dat$attr$status
      ids_sus <- which(active == 1 & status2 == "s")
      idsInf1 <- which(active == 1 & status2 == "i")
      idsInf2 <- which(active == 1 & status2 == "i2")
      idsInf3 <- which(active == 1 & status2 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {
        #sus to Inf2
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in idsInf2) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges))) {
          trans_probability_sus_i2 <- dat$param$inf.probStable
          act_rate <- dat$param$act.rate
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(edges),1,final_probability_sus_i2) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf2_sus <- unique(edges[,2])
            num_newInf2_sus <- length(ids_newInf2_sus)
            if (num_newInf2_sus > 0) {
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- length(ids_newInf2_sus)
            if (num_newInf2_sus > 0) {
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
      } 
      if (at == 2) { ##time starts at 2
        dat$epi$i.num <- c(0,sum(active == 1 & status == "i")) 
        dat$epi$i2.num <- c(0,sum(active == 1 & status == "i2"))
        dat$epi$s.num <- c(0,sum(active == 1 & status =="s"))
        dat$epi$si.flow <- c(0, num_newInf1_sus + num_newInf2_sus) #nInf1 is acute, nInf1 is susceptible
      }
      else {
        dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
        dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
        dat$epi$s.num[at] <- sum(active == 1 & status =="s")
        dat$epi$si.flow[at] <- num_newInf1_sus + num_newInf2_sus 
      }
      dat$nw <- nw
    }
    else {
      num_newInf1_sus <- 0 
      num_newInf2_sus <- 0
      num_newInf3_sus <- 0 
      if (at <= art_start_time+floor(art_number/art_rate)-1){
        art_status <- get.vertex.attribute(nw,"art_status") 
        ids_old_art <- which(active == 1 & status == "i3" & art_status == 1)
        idsInf1AND2 <- c(idsInf1,idsInf2)
        ids_new_art <- sample(idsInf1AND2,size = min(art_rate,length(idsInf1AND2)), replace=FALSE)
        ids_inf_art <- c(ids_new_art,ids_old_art) 
        art_status <- set.vertex.attribute(nw,"art_status",c(1),v=ids_inf_art)
        art_status <- get.vertex.attribute(nw,"art_status") 
        ids_inf_noart1 <- which(active == 1 & status == "i" & art_status == 0)
        ids_inf_noart2 <- which(active == 1 & status == "i2" & art_status == 0)
      }
      else {
        art_status <- get.vertex.attribute(nw,"art_status")
        ids_inf_art <- which(active == 1 & status == "i3" & art_status == 1) #people who are active and status S
        ids_inf_noart1 <- which(active == 1 & status == "i" & art_status == 0)
        ids_inf_noart2 <- which(active == 1 & status == "i2" & art_status == 0)
      }
      
      dat$attr$status[ids_inf_art] <- "i3"
      
      #I1 and sus (no art)
      if (nElig1 > 0 && nElig1 < (nActive - (nElig2 + nElig3))) {
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_inf_noart1) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges))) {
          trans_probability_sus_i1 <- dat$param$inf.probAcute
          act_rate <- dat$param$act.rate
          final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i1 <- rbinom(nrow(edges),1,final_probability_sus_i1) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf1_sus <- unique(edges[,2])
            num_newInf1_sus <- length(ids_newInf1_sus)
            if (num_newInf1_sus > 0) {
              dat$attr$status[ids_newInf1_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf1_sus <- edges[2]
            num_newInf1_sus <- length(ids_newInf1_sus)
            if (num_newInf1_sus > 0) {
              dat$attr$status[ids_newInf1_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
            }
          }
        }
      }
      status2 <- dat$attr$status
      ids_sus <- which(active == 1 & status2 == "s")
      idsInf1 <- which(active == 1 & status2 == "i")
      idsInf2 <- which(active == 1 & status2 == "i2")
      idsInf3 <- which(active == 1 & status2 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      #I2 and sus (no art)
      if (nElig2 > 0 && nElig2 < (nActive - (nElig1 + nElig3))) {
        #sus to Inf2
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_inf2_noart) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges))) {
          trans_probability_sus_i2 <- dat$param$inf.probStable
          act_rate <- dat$param$act.rate
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(edges),1,final_probability_sus_i2) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf2_sus <- unique(edges[,2])
            num_newInf2_sus <- length(ids_newInf2_sus)
            if (num_newInf2_sus > 0) {
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- length(ids_newInf2_sus)
            if (num_newInf2_sus > 0) {
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
      }
      status3 <- dat$attr$status
      ids_sus <- which(active == 1 & status3 == "s")
      idsInf1 <- which(active == 1 & status3 == "i")
      idsInf2 <- which(active == 1 & status3 == "i2")
      idsInf3 <- which(active == 1 & status3 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      #I3 and sus (art)
      if (nElig3 > 0 && nElig3 < (nActive - (nElig1 + nElig2))) {
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in ids_inf_art) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromedges <- fromedges[fromsusindices]
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[,1]==ids))
          }
          toedges <- toedges[tosusindices,]
          if(!(is.null(nrow(toedges)))){
            toedges <- toedges[,c(2,1)]
          }
        }
        if (is.vector(toedges)){
          tosusindices <- c(0)
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          toedges <- toedges[tosusindices]
          toedges <- c(toedges[2],toedges[1])
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges))) {
          trans_probability_sus_i3 <- dat$param$inf.probStable * (1-dat$param$art_efficacy)
          act_rate <- dat$param$act.rate
          final_probability_sus_i3 <- 1 - (1 - trans_probability_sus_i3)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i3 <- rbinom(nrow(edges),1,final_probability_sus_i3) #trasmit rate is a binary variable
          edges <- edges[which(transmit_sus_i3 == 1),] #select all the nodes where transmit rate is 1
          if (!(is.null(nrow(edges)))) {
            ids_newInf3_sus <- unique(edges[,2])
            num_newInf3_sus <- length(ids_newInf3_sus)
            if (num_newInf3_sus > 0) {
              dat$attr$status[ids_newInf3_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf3_sus] <- at #timestamp
            }
          }
          if (is.vector(edges)){
            ids_newInf3_sus <- edges[2]
            num_newInf3_sus <- length(ids_newInf3_sus)
            if (num_newInf3_sus > 0) {
              dat$attr$status[ids_newInf3_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf3_sus] <- at #timestamp
            }
          }
        }
      }
      if (at == 2) { ##time starts at 2
        dat$epi$i.num <- c(0,sum(active == 1 & status == "i")) 
        dat$epi$i2.num <- c(0,sum(active == 1 & status == "i2"))
        dat$epi$s.num <- c(0,sum(active == 1 & status =="s"))
        dat$epi$i3.num <- c(0,sum(active==1 & status == "i3"))
        dat$epi$si.flow <- c(0, num_newInf1_sus + num_newInf2_sus + num_newInf3_sus) 
      }
      else {
        dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
        dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
        dat$epi$s.num[at] <- sum(active == 1 & status =="s")
        dat$epi$i3.num[at] <- sum(active == 1 & status =="i3")
        dat$epi$si.flow[at] <- num_newInf1_sus + num_newInf2_sus + num_newInf3_sus
      }
      dat$nw <- nw
    }
  }
  if (whether_sep) {
    if (at < sep_start_time){
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del9 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del9))) {
          del9$trans_probability_sus_i1 <- dat$param$inf.probAcute
          del9$act_rate <- dat$param$act.rate
          del9$final_probability_sus_i1 <- 1 - (1 - del9$trans_probability_sus_i1)^del9$act_rate #final transmission prob, using binomial prob
          transmit_sus_i1 <- rbinom(nrow(del9),1,del9$final_probability_sus_i1) #trasmit rate is a binary variable
          del9 <- del9[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
          ids_newInf1_sus <- unique(del9$sus)
          num_newInf1_sus <- length(ids_newInf1_sus)
          if (num_newInf1_sus > 0) {
            dat$attr$status[ids_newInf1_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
          }
        }
      }
      status2 <- dat$attr$status
      ids_sus <- which(active == 1 & status2 == "s")
      idsInf1 <- which(active == 1 & status2 == "i")
      idsInf2 <- which(active == 1 & status2 == "i2")
      idsInf3 <- which(active == 1 & status2 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {
        del10 <- discord_edgelist(dat, idsInf2, ids_sus, at)
        if (!(is.null(del10))) {
          del10$trans_probability_sus_i2 <- dat$param$inf.probStable
          del10$act_rate <- dat$param$act.rate
          del10$final_probability_sus_i2 <- 1 - (1 - del10$trans_probability_sus_i2)^del10$act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(del10),1,del10$final_probability_sus_i2) #trasmit rate is a binary variable
          del10 <- del10[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          ids_newInf2_sus <- unique(del10$sus)
          num_newInf2_sus <- length(ids_newInf2_sus)
          if (num_newInf2_sus > 0) {
            dat$attr$status[ids_newInf2_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
          }
        }
      } 
      if (at == 2) { ##time starts at 2
        dat$epi$i.num <- c(0,sum(active == 1 & status == "i")) 
        dat$epi$i2.num <- c(0,sum(active == 1 & status == "i2"))
        dat$epi$s.num <- c(0,sum(active == 1 & status =="s"))
        dat$epi$si.flow <- c(0, num_newInf1_sus + num_newInf2_sus) #nInf1 is acute, nInf1 is susceptible
      }
      else {
        dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
        dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
        dat$epi$s.num[at] <- sum(active == 1 & status =="s")
        dat$epi$si.flow[at] <- num_newInf1_sus + num_newInf2_sus
      }
      dat$nw <- nw
    }
    else {
      new_act_rate <- (sep_exchange_frequency * dat$param$act.rate * networkSize - sep_enrollment * sep_compliance)/networkSize
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del9 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del9))) {
          del9$trans_probability_sus_i1 <- dat$param$inf.probAcute
          del9$act_rate <- new_act_rate
          del9$final_probability_sus_i1 <- 1 - (1 - del9$trans_probability_sus_i1)^del9$act_rate #final transmission prob, using binomial prob
          transmit_sus_i1 <- rbinom(nrow(del9),1,del9$final_probability_sus_i1) #trasmit rate is a binary variable
          del9 <- del9[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
          ids_newInf1_sus <- unique(del9$sus)
          num_newInf1_sus <- length(ids_newInf1_sus)
          if (num_newInf1_sus > 0) {
            dat$attr$status[ids_newInf1_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
          }
        }
      }
      status2 <- dat$attr$status
      ids_sus <- which(active == 1 & status2 == "s")
      idsInf1 <- which(active == 1 & status2 == "i")
      idsInf2 <- which(active == 1 & status2 == "i2")
      idsInf3 <- which(active == 1 & status2 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
      if (nElig2 > 0 && nElig2 < (nActive - nElig1)) {
        del10 <- discord_edgelist(dat, idsInf2, ids_sus, at)
        if (!(is.null(del10))) {
          del10$trans_probability_sus_i2 <- dat$param$inf.probStable
          del10$act_rate <- new_act_rate
          del10$final_probability_sus_i2 <- 1 - (1 - del10$trans_probability_sus_i2)^del10$act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(nrow(del10),1,del10$final_probability_sus_i2) #trasmit rate is a binary variable
          del10 <- del10[which(transmit_sus_i2 == 1),] #select all the nodes where transmit rate is 1
          ids_newInf2_sus <- unique(del10$sus)
          num_newInf2_sus <- length(ids_newInf2_sus)
          if (num_newInf2_sus > 0) {
            dat$attr$status[ids_newInf2_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
          }
        }
      } 
      if (at == 2) { ##time starts at 2
        dat$epi$i.num <- c(0,sum(active == 1 & status == "i")) 
        dat$epi$i2.num <- c(0,sum(active == 1 & status == "i2"))
        dat$epi$s.num <- c(0,sum(active == 1 & status =="s"))
        dat$epi$si.flow <- c(0, num_newInf1_sus + num_newInf2_sus) #nInf1 is acute, nInf1 is susceptible
      }
      else {
        dat$epi$i.num[at] <- sum(active == 1 & status == "i") 
        dat$epi$i2.num[at] <- sum(active == 1 & status == "i2")
        dat$epi$s.num[at] <- sum(active == 1 & status =="s")
        dat$epi$si.flow[at] <- num_newInf1_sus + num_newInf2_sus
      }
      dat$nw <- nw
    }
  }
  #plot(dat$nw)
  #print(network.edgecount(dat$nw))
  return(dat)
}
##----disease progression---------------------------------------------------
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
  
  dat$attr$status <- status
  
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
formation <- ~edges #+ degree(c(0,1,2,3))

#target stats
target.stats <- c(edges)#,c(181,154,54,28))

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
control <- control.net(type = "SI", nsteps = 365, nsims = 3, 
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

simulationData <- as.data.frame(sim)
simulationData$i.num[1] <- initAcute
simulationData$i2.num[1] <- initStable
simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
simulationData <- simulationData %>% mutate("i1ANDi2.num"= i.num + i2.num)
y_range <- range(0,max(simulationData$i1ANDi2.num)) 
plot(simulationData$time, simulationData$i.num, type="l", col="blue", ylim=y_range)
#plot(sim,type="network")
lines(simulationData$time, simulationData$i2.num, type="l", col="red")
lines(simulationData$time, simulationData$i1ANDi2.num, type="l", col="black")
#lines(simulationData$time, simulationData$i3.num, type="l", col="purple")
legend(70,400,c("i.num","i2.num","i1ANDi2.num","i3.num"),col=c("blue","red","black","purple"),pch=21:22,lty=1:2)
