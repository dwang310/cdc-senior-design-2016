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
    # print(paste("this is number of infection 1 before infection starts",at,nElig1))
    # print(paste("this is number of infection 2 before infection starts",at,nElig2))
    if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
      del9 <- discord_edgelist(dat, idsInf1, ids_sus, at)
      if (!(is.null(del9))) {
        for (edge in 1:nrow(del9)){
          trans_probability_sus_i1 <- dat$param$inf.probAcute
          nodeDegree <- get_degree(dat$nw)[del9[edge,3]]
          #print(paste("this is the nodedegree of inf1",nodeDegree))
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate #final transmission prob, using binomial prob
          #print(paste("this is final probability related to nElig1",final_probability_sus_i1))
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
      }
      toedges <- edges[indices2,]
      if (!(is.null(nrow(toedges)))){
        #print("sus ids when toedges have multiple rows")
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
        #print("sus ids when toedges is a vector")
        for (ids in ids_sus){
          tosusindices <- c(tosusindices,which(toedges[1]==ids))
        }
        #print("this is tosusindices")
        #print(tosusindices)
        tosusindices <- tosusindices[-1]
        if (length(tosusindices)==0){
          toedges <- toedges[tosusindices]
        } else {
          toedges <- c(toedges[2],toedges[1])
        }
      }
      edges <- rbind(fromedges,toedges)
      # print("this is from edges")
      # print(fromedges)
      # print("this is to edges")
      # print(toedges)
      # print("this is edges")
      # print(edges)
      # print(nrow(edges))
      # print(is.vector(edges))
      
      if (!(is.null(nrow(edges))) & nrow(edges)!= 0 & ncol(edges)!=0) {
        #num_newInf2_sus <- 0 
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
      }
      if (is.vector(edges)){
        trans_probability_sus_i2 <- dat$param$inf.probStable
        nodeDegree <- get_degree(dat$nw)[edges[1]]
        act_rate <- dat$param$act.rate/nodeDegree
        final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
        transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
        if (transmit_sus_i2==1){
          ids_newInf2_sus <- edges[2]
          num_newInf2_sus <- num_newInf2_sus + 1 
          dat$attr$status[ids_newInf2_sus] <- "i" #status is active
          dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
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
      # print(paste("this is number of infection 1 before infection starts",at,nElig1))
      # print(paste("this is number of infection 2 before infection starts",at,nElig2))
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del9 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del9))) {
          for (edge in 1:nrow(del9)){
            trans_probability_sus_i1 <- dat$param$inf.probAcute
            nodeDegree <- get_degree(dat$nw)[del9[edge,3]]
            #print(paste("this is the nodedegree of inf1",nodeDegree))
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate #final transmission prob, using binomial prob
            #print(paste("this is final probability related to nElig1",final_probability_sus_i1))
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
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          #print("sus ids when toedges have multiple rows")
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
          #print("sus ids when toedges is a vector")
          for (ids in ids_sus){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          #print("this is tosusindices")
          #print(tosusindices)
          tosusindices <- tosusindices[-1]
          if (length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          } else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        # print("this is from edges")
        # print(fromedges)
        # print("this is to edges")
        # print(toedges)
        # print("this is edges")
        # print(edges)
        # print(nrow(edges))
        # print(is.vector(edges))
        
        if (!(is.null(nrow(edges))) & nrow(edges)!= 0 & ncol(edges)!=0) {
          #num_newInf2_sus <- 0 
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
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
          if (transmit_sus_i2==1){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- num_newInf2_sus + 1 
            dat$attr$status[ids_newInf2_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
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
    else {
      num_newInf1_from_sus_prep <- 0 
      num_newInf1_from_sus_noprep <- 0
      num_newInf2_from_sus_prep <- 0 
      num_newInf2_from_sus_noprep <- 0
      prep_status <- get.vertex.attribute(nw,"prep_status")
      ids_sus_prep <- which(active == 1 & status == "s" & prep_status == 0)
      ids_sus_noprep <- which(active == 1 & status == "s" & prep_status == 0)
      if (at <= prep_start_time+floor(prep_number/prep_rate)-1){
        prep_status <- get.vertex.attribute(nw,"prep_status") 
        ids_sus_old_prep <- which(active == 1 & status == "s" & prep_status == 1)
        ids_sus_new_prep <- sample(ids_sus_noprep,size = prep_rate, replace=FALSE)
        ids_sus_prep <- c(ids_sus_new_prep,ids_sus_old_prep) 
        ids_sus_noprep <- setdiff(ids_sus,ids_sus_prep)
        prep_status <- set.vertex.attribute(nw,"prep_status",c(1),v=ids_sus_prep)
        prep_status <- get.vertex.attribute(nw,"prep_status")
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
        for (ids in idsInf1) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus_prep){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus_prep){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if (length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus_prep){
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
          for (ids in ids_sus_prep){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          tosusindices <- tosusindices[-1]
          if (length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_susprep_i1 <- (dat$param$inf.probAcute) * (1-dat$param$prep_efficacy)
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_susprep_i1 <- 1 - (1 - trans_probability_susprep_i1)^act_rate #final transmission prob, using binomial prob
            transmit_susprep_i1 <- rbinom(1,1,final_probability_susprep_i1) #trasmit rate is a binary variable
            if (transmit_susprep_i1==1){
              ids_newInf1_from_sus_prep <- edges[edge,2]
              num_newInf1_from_sus_prep <- num_newInf1_from_sus_prep + 1
              dat$attr$status[ids_newInf1_from_sus_prep] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_from_sus_prep] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_susprep_i1 <- (dat$param$inf.probAcute) * (1-dat$param$prep_efficacy)
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_susprep_i1 <- 1 - (1 - trans_probability_susprep_i1)^act_rate 
          transmit_susprep_i1 <- rbinom(1,1,final_probability_susprep_i1)
          if(transmit_susprep_i1==1){
            ids_newInf1_from_sus_prep <- edges[2]
            num_newInf1_from_sus_prep <- num_newInf1_from_sus_prep + 1
            dat$attr$status[ids_newInf1_from_sus_prep] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_from_sus_prep] <- at #timestamp
          }
        }
        status4 <- dat$attr$status
        prep_status <- get.vertex.attribute(nw,"prep_status") 
        ids_sus_prep <- which(active == 1 & status4 == "s" & prep_status == 1)
        ids_sus_noprep <- which(active == 1 & status4 == "s" & prep_status == 0)
        idsInf1 <- which(active == 1 & status4 == "i")
        idsInf2 <- which(active == 1 & status4 == "i2")
        nActive <- sum(active == 1)
        nElig1 <- length(idsInf1)
        nElig2 <- length(idsInf2)
        
        ##acute patient with susceptible not on prep
        edges <- as.matrix.network.edgelist(dat$nw)
        edges <- edges[order(edges[,1]),]
        indices1 <- c(0)
        indices2 <- c(0)
        for (ids in idsInf1) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus_noprep){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus_noprep){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if (length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus_noprep){
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
          for (ids in ids_sus_noprep){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          tosusindices <- tosusindices[-1]
          if (length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_susnoprep_i1 <- dat$param$inf.probAcute
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_susnoprep_i1 <- 1 - (1 - trans_probability_susnoprep_i1)^act_rate
            transmit_susnoprep_i1 <- rbinom(1,1,final_probability_susnoprep_i1)
            if (transmit_susnoprep_i1==1){
              ids_newInf1_from_sus_noprep <- edges[edge,2]
              num_newInf1_from_sus_noprep <- num_newInf1_from_sus_noprep + 1
              dat$attr$status[ids_newInf1_from_sus_noprep] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_from_sus_noprep] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_susnoprep_i1 <- dat$param$inf.probAcute
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_susnoprep_i1 <- 1 - (1 - trans_probability_susnoprep_i1)^act_rate
          transmit_susnoprep_i1 <- rbinom(1,1,final_probability_susnoprep_i1)
          if (transmit_susnoprep_i1==1){
            ids_newInf1_from_sus_noprep  <- edges[2]
            num_newInf1_from_sus_noprep <- num_newInf1_from_sus_noprep + 1
            dat$attr$status[ids_newInf1_from_sus_noprep] <- "i" #status is active
            dat$attr$infTime[ids_newInf1_from_sus_noprep] <- at #timestamp
          }
        }
      }
      status2 <- dat$attr$status
      prep_status <- get.vertex.attribute(nw,"prep_status") 
      ids_sus_prep <- which(active == 1 & status2 == "s" & prep_status == 1)
      ids_sus_noprep <- which(active == 1 & status2 == "s" & prep_status == 0)
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
        for (ids in idsInf2) { 
          indices1 <- c(indices1,which(edges[,1]==ids))
          indices2 <- c(indices2,which(edges[,2]==ids))
        }
        indices1 <- indices1[-1]
        indices2 <- indices2[-1]
        fromedges <- edges[indices1,]
        if (!(is.null(nrow(fromedges)))){
          fromsusindices <- c(0)
          for (ids in ids_sus_noprep){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus_noprep){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if (length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus_noprep){
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
          for (ids in ids_sus_noprep){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          tosusindices <- tosusindices[-1]
          if (length(tosusindices)==0) {
            toedges <- toedges[tosusindices]
          }
          else{
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i2 <- dat$param$inf.probStable
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
            transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) 
            if (transmit_sus_i2==1){
              ids_newInf2_from_sus_noprep <- edges[edge,2]
              num_newInf2_from_sus_noprep <- num_newInf2_from_sus_noprep + 1 
              dat$attr$status[ids_newInf2_from_sus_noprep] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_from_sus_noprep] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) 
          if (transmit_sus_i2==1){
            ids_newInf2_from_sus_noprep <- edges[2]
            num_newInf2_from_sus_noprep <- num_newInf2_from_sus_noprep + 1 
            dat$attr$status[ids_newInf2_from_sus_noprep] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_from_sus_noprep] <- at #timestamp
          }
        }
        status3 <- dat$attr$status
        prep_status <- get.vertex.attribute(nw,"prep_status") 
        ids_sus_prep <- which(active == 1 & status3 == "s" & prep_status == 1)
        ids_sus_noprep <- which(active == 1 & status3 == "s" & prep_status == 0)
        idsInf1 <- which(active == 1 & status3 == "i")
        idsInf2 <- which(active == 1 & status3 == "i2")
        nActive <- sum(active == 1)
        nElig1 <- length(idsInf1)
        nElig2 <- length(idsInf2)
        ##chronic patients with susceptible prep
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
          for (ids in ids_sus_prep){
            fromsusindices <- c(fromsusindices,which(fromedges[,2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus_prep){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if(length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
        toedges <- edges[indices2,]
        if (!(is.null(nrow(toedges)))){
          tosusindices <- c(0)
          for (ids in ids_sus_prep){
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
          for (ids in ids_sus_prep){
            tosusindices <- c(tosusindices,which(toedges[1]==ids))
          }
          tosusindices <- tosusindices[-1]
          if(length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i2 <- dat$param$inf.probStable * (1-dat$param$prep_efficacy)
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
            transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2)
            if(transmit_sus_i2==1){
              ids_newInf2_from_sus_prep <- edges[edge,2]
              num_newInf2_from_sus_prep <- num_newInf2_from_sus_prep + 1 
              dat$attr$status[ids_newInf2_from_sus_prep] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_from_sus_prep] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable * (1-dat$param$prep_efficacy)
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2)
          if(transmit_sus_i2==1){
            ids_newInf2_from_sus_prep <- edges[2]
            num_newInf2_from_sus_prep <- num_newInf2_from_sus_prep + 1 
            dat$attr$status[ids_newInf2_from_sus_prep] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_from_sus_prep] <- at #timestamp
          }
        }
      }
      status3 <- dat$attr$status
      prep_status <- get.vertex.attribute(nw,"prep_status") 
      ids_sus_prep <- which(active == 1 & status3 == "s" & prep_status == 1)
      ids_sus_noprep <- which(active == 1 & status3 == "s" & prep_status == 0)
      idsInf1 <- which(active == 1 & status3 == "i")
      idsInf2 <- which(active == 1 & status3 == "i2")
      idsInf3 <- which(active == 1 & status3 == "i3")
      nActive <- sum(active == 1)
      nElig1 <- length(idsInf1)
      nElig2 <- length(idsInf2)
      nElig3 <- length(idsInf3)
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
    num_newInf1_sus <- 0 
    num_newInf2_sus <- 0
    num_newInf3_sus <- 0 
    if (at < art_start_time) {
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del1 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del1))) {
          for (edge in 1:nrow(del1)){
            trans_probability_sus_i1 <- dat$param$inf.probAcute
            nodeDegree <- get_degree(dat$nw)[del1[edge,3]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate
            transmit_sus_i1 <- rbinom(1,1,final_probability_sus_i1)
            if(transmit_sus_i1==1){
              ids_newInf1_sus <- del1[edge,2]
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
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if(length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
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
          if (length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else{
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i2 <- dat$param$inf.probStable
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
            transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) 
            if(transmit_sus_i2==1){
              ids_newInf2_sus <- edges[edge,2]
              num_newInf2_sus <- num_newInf2_sus + 1 
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) 
          if(transmit_sus_i2==1){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- num_newInf2_sus + 1 
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
      num_newInf1_sus <- 0 
      num_newInf2_sus <- 0
      num_newInf3_sus <- 0
      #print(at)
      if (at <= art_start_time+floor(art_number/art_rate)-1){
        art_status <- get.vertex.attribute(nw,"art_status")
        ids_old_art <- which(active == 1 & status == "i3" & art_status == 1)
        # print("this is the ids of old people on art")
        # print(ids_old_art)
        idsInf1AND2 <- c(idsInf1,idsInf2)
        # print("this is the ids of infected people")
        # print(idsInf1AND2)
        # print("this is the number of nodes infected")
        # print(length(idsInf1AND2))
        if (length(idsInf1AND2) == 1) {
          ids_new_art = idsInf1AND2
        } else {
          ids_new_art <- sample(idsInf1AND2,size = min(art_rate,length(idsInf1AND2)), replace=FALSE)
        }
        # print("this is the ids of new people on art") 
        # print(ids_new_art)
        ids_inf_art <- c(ids_new_art,ids_old_art) 
        # print("this is the ids of everyone on art")
        # print(ids_inf_art)
        art_status <- set.vertex.attribute(nw,"art_status",c(1),v=ids_inf_art)
        art_status <- get.vertex.attribute(nw,"art_status")
        ids_inf_noart1 <- which(active == 1 & status == "i" & art_status == 0)
        ids_inf_noart2 <- which(active == 1 & status == "i2" & art_status == 0)
      }
      else {
        art_status <- get.vertex.attribute(nw,"art_status")
        ids_inf_art <- which(active == 1 & status == "i3" & art_status == 1) #people who are active and status i3
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
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if(length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
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
          if(length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else{
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i1 <- dat$param$inf.probAcute
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate
            transmit_sus_i1 <- rbinom(1,1,final_probability_sus_i1)
            if(transmit_sus_i1==1){
              ids_newInf1_sus <- edges[edge,2] 
              num_newInf1_sus <- num_newInf1_sus + 1 
              dat$attr$status[ids_newInf1_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf1_sus] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i1 <- dat$param$inf.probAcute
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate
          transmit_sus_i1 <- rbinom(1,1,final_probability_sus_i1)
          if(transmit_sus_i1==1){
            ids_newInf1_sus <- edges[2]
            num_newInf1_sus <- num_newInf1_sus + 1 
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
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
        }
        if (is.vector(fromedges)){
          fromsusindices <- c(0)
          for (ids in ids_sus){
            fromsusindices <- c(fromsusindices,which(fromedges[2]==ids))
          }
          fromsusindices <- fromsusindices[-1]
          if(length(fromsusindices)==0){
            fromedges <- fromedges[fromsusindices]
          }
        }
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
          if(length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else{
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){ 
            trans_probability_sus_i2 <- dat$param$inf.probStable
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree 
            final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
            transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2)
            if(transmit_sus_i2==1){
              ids_newInf2_sus <- edges[edge,2]
              num_newInf2_sus <- num_newInf2_sus +1
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree 
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2)
          if(transmit_sus_i2==1){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- num_newInf2_sus + 1 
            dat$attr$status[ids_newInf2_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
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
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
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
        }
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
          if (length(tosusindices)==0){
            toedges <- toedges[tosusindices]
          }
          else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(edges)) & nrow(edges)!=0 & ncol(edges)!=0) {
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i3 <- dat$param$inf.probStable * (1-dat$param$art_efficacy)
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i3 <- 1 - (1 - trans_probability_sus_i3)^act_rate
            transmit_sus_i3 <- rbinom(1,1,final_probability_sus_i3) 
            if (transmit_sus_i3==1){
              ids_newInf3_sus <- edges[edge,2]
              num_newInf3_sus <- num_newInf3_sus +1
              dat$attr$status[ids_newInf3_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf3_sus] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i3 <- dat$param$inf.probStable * (1-dat$param$art_efficacy)
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i3 <- 1 - (1 - trans_probability_sus_i3)^act_rate
          transmit_sus_i3 <- rbinom(1,1,final_probability_sus_i3) 
          if (transmit_sus_i3==1){
            ids_newInf3_sus <- edges[2]
            num_newInf3_sus <- num_newInf3_sus + 1
            dat$attr$status[ids_newInf3_sus] <- "i" #status is active
            dat$attr$infTime[ids_newInf3_sus] <- at #timestamp
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
    num_newInf1_sus <- 0 
    num_newInf2_sus <- 0
    if (at < sep_start_time){
      if (nElig1 > 0 && nElig1 < (nActive - nElig2)) {
        del10 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del10))) {
          #num_newInf1_sus <- 0 
          for (edge in 1:nrow(del10)){
            trans_probability_sus_i1 <- dat$param$inf.probAcute
            nodeDegree <- get_degree(dat$nw)[del10[edge,3]]
            #print(paste("this is the nodedegree of inf1",nodeDegree))
            act_rate <- dat$param$act.rate/nodeDegree
            final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate #final transmission prob, using binomial prob
            #print(paste("this is final probability related to nElig1",final_probability_sus_i1))
            transmit_sus_i1 <- rbinom(1,1,final_probability_sus_i1) #trasmit rate is a binary variable
            #infections <- del9[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
            if (transmit_sus_i1==1){
              ids_newInf1_sus <- del10[edge,2]
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
          if (length(tosusindices)==0){
            tosusindices <- toedges[tosusindices]
          } else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        
        # print("toedges")
        # print(toedges)
        # print("this is from edges")
        # print(fromedges)
        # print("this is to edges")
        # print(toedges)
        # print("this is edges")
        # print(edges)
        # print(nrow(edges))
        # print(is.vector(edges))
        
        if (!(is.null(nrow(edges))) & nrow(edges)!= 0) {
          #num_newInf2_sus <- 0 
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i2 <- dat$param$inf.probStable
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- dat$param$act.rate/nodeDegree
            #print(paste("this is nodedegree related to inf2",nodeDegree))
            # print("this is edges related to nElig2 matrix")
            # print(edges)
            # print(paste("this is edge number related to nElig2 matrix",edge))
            # print(paste("this is edge related to nElig2 matrix",edges[edge]))
            # print(paste("this is node related to nElig2 matrix",edges[edge,1]))
            final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
            #print(paste("this is final probability related to nElig2 matrix",final_probability_sus_i2))
            transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
            if (transmit_sus_i2==1){
              ids_newInf2_sus <- edges[edge,2]
              num_newInf2_sus <- num_newInf2_sus + 1 
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- dat$param$act.rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          #print(paste("this is nodedegree for inf2",nodeDegree))
          #print(paste("this is final probability related to nElig2 vector",final_probability_sus_i2))
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
          if (transmit_sus_i2==1){
            ids_newInf2_sus <- edges[2]
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
        del11 <- discord_edgelist(dat, idsInf1, ids_sus, at)
        if (!(is.null(del11))) {
          #num_newInf1_sus <- 0 
          for (edge in 1:nrow(del11)){
            trans_probability_sus_i1 <- dat$param$inf.probAcute
            nodeDegree <- get_degree(dat$nw)[del11[edge,3]]
            #print(paste("this is the nodedegree of inf1",nodeDegree))
            act_rate <- new_act_rate/nodeDegree
            final_probability_sus_i1 <- 1 - (1 - trans_probability_sus_i1)^act_rate #final transmission prob, using binomial prob
            #print(paste("this is final probability related to nElig1",final_probability_sus_i1))
            transmit_sus_i1 <- rbinom(1,1,final_probability_sus_i1) #trasmit rate is a binary variable
            #infections <- del9[which(transmit_sus_i1 == 1),] #select all the nodes where transmit rate is 1
            if (transmit_sus_i1==1){
              ids_newInf1_sus <- del11[edge,2]
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
          fromsusindices <- fromsusindices[-1]
          fromedges <- fromedges[fromsusindices,]
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
        }
        
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
          if (length(tosusindices)==0){
            tosusindices <- toedges[tosusindices]
          } else {
            toedges <- c(toedges[2],toedges[1])
          }
        }
        edges <- rbind(fromedges,toedges)
        
        if (!(is.null(nrow(edges))) & nrow(edges)!= 0) {
          #num_newInf2_sus <- 0 
          for (edge in 1:nrow(edges)){
            trans_probability_sus_i2 <- dat$param$inf.probStable
            nodeDegree <- get_degree(dat$nw)[edges[edge,1]]
            act_rate <- new_act_rate/nodeDegree
            #print(paste("this is nodedegree related to inf2",nodeDegree))
            final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
            transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
            if (transmit_sus_i2==1){
              ids_newInf2_sus <- edges[edge,2]
              num_newInf2_sus <- num_newInf2_sus + 1 
              dat$attr$status[ids_newInf2_sus] <- "i" #status is active
              dat$attr$infTime[ids_newInf2_sus] <- at #timestamp
            }
          }
        }
        if (is.vector(edges)){
          trans_probability_sus_i2 <- dat$param$inf.probStable
          nodeDegree <- get_degree(dat$nw)[edges[1]]
          act_rate <- new_act_rate/nodeDegree
          final_probability_sus_i2 <- 1 - (1 - trans_probability_sus_i2)^act_rate #final transmission prob, using binomial prob
          #print(paste("this is nodedegree for inf2",nodeDegree))
          #print(paste("this is final probability related to nElig2 vector",final_probability_sus_i2))
          transmit_sus_i2 <- rbinom(1,1,final_probability_sus_i2) #trasmit rate is a binary variable
          if (transmit_sus_i2==1){
            ids_newInf2_sus <- edges[2]
            num_newInf2_sus <- num_newInf2_sus + 1 
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