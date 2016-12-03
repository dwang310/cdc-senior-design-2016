
################################### ONE #####################################
plotValM1 <- eventReactive( input$actionM1, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM1 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM1 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM1 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time1
  prep_rate <- input$prep_rate1
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time1
  art_rate <- input$art_rate1
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency1
  sep_start_time <- input$sep_start_time1
  sep_compliance <- input$sep_compliance1
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})


################################### TWO #####################################
plotValM2 <- eventReactive( input$actionM2, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM2 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM2 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM2 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time2
  prep_rate <- input$prep_rate2
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time2
  art_rate <- input$art_rate2
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency2
  sep_start_time <- input$sep_start_time2
  sep_compliance <- input$sep_compliance2
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### THREE #####################################
plotValM3 <- eventReactive( input$actionM3, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM3 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM3 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM3 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time3
  prep_rate <- input$prep_rate3
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time3
  art_rate <- input$art_rate3
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency3
  sep_start_time <- input$sep_start_time3
  sep_compliance <- input$sep_compliance3
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### FOUR #####################################
plotValM4 <- eventReactive( input$actionM4, {
  print("we made it!")
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM4 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM4 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM4 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time4
  prep_rate <- input$prep_rate4
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time4
  art_rate <- input$art_rate4
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency4
  sep_start_time <- input$sep_start_time4
  sep_compliance <- input$sep_compliance4
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### FIVE #####################################
plotValM5 <- eventReactive( input$actionM5, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM5 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM5 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM5 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time5
  prep_rate <- input$prep_rate5
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time5
  art_rate <- input$art_rate5
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency5
  sep_start_time <- input$sep_start_time5
  sep_compliance <- input$sep_compliance5
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### SIX #####################################
plotValM6 <- eventReactive( input$actionM6, {
  print("we made it!")
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM6 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM6 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM6 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time6
  prep_rate <- input$prep_rate6
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time6
  art_rate <- input$art_rate6
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency6
  sep_start_time <- input$sep_start_time6
  sep_compliance <- input$sep_compliance6
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### SEVEN #####################################
plotValM7 <- eventReactive( input$actionM7, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM7 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM7 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM7 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time7
  prep_rate <- input$prep_rate7
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time7
  art_rate <- input$art_rate7
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency7
  sep_start_time <- input$sep_start_time7
  sep_compliance <- input$sep_compliance7
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### EIGHT #####################################
plotValM8 <- eventReactive( input$actionM8, {
  print("we made it!")
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM8 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM8 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM8 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time8
  prep_rate <- input$prep_rate8
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time8
  art_rate <- input$art_rate8
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency8
  sep_start_time <- input$sep_start_time8
  sep_compliance <- input$sep_compliance8
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})

################################### NINE #####################################
plotValM9 <- eventReactive( input$actionM9, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM9 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM9 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM9 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time9
  prep_rate <- input$prep_rate9
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time9
  art_rate <- input$art_rate9
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency9
  sep_start_time <- input$sep_start_time9
  sep_compliance <- input$sep_compliance9
 #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})


################################### TEN #####################################
plotValM10 <- eventReactive( input$actionM10, {
  proggy <- Progress$new(session, min=1, max=15)
  on.exit(proggy$close())
  
  proggy$set(message = "Running simulation...", detail = "This may take a while...")
  for(i in 1:input$nsims){
    proggy$set(value = i)
  }
  
  source("constants.R", local = TRUE)
  
  
  networkSize <- 550  
  percentMales <- 0.577
  percentWhite <- 0.985
  percentIncome10K <- 0.919
  percentIncarcerated <- 0.542
  avgDegree <- 1.5
  avgEdgeDuration <- 365
  avgEdges <- (networkSize*avgDegree)/2
  
  
  # Intervention selection logic
  whether_prep <- 0
  whether_art <- 0
  whether_sep <- 0
  
  if(input$dropdownM10 == 1){
    whether_sep <- 1
    whether_art <- 0
    whether_prep <- 0
  } else if(input$dropdownM10 == 2) {
    whether_art <- 1
    whether_sep <- 0
    whether_prep <- 0
  } else if(input$dropdownM10 == 3) {
    whether_prep <- 1
    whether_art <- 0
    whether_sep <- 0
  }
  
  budget <- input$budget
  prep_cost <- 16260.23
  prep_number <- floor(budget/prep_cost)
  prep_start_time <- input$prep_start_time10
  prep_rate <- input$prep_rate10
  art_cost <- 20600
  art_number <- floor(budget/art_cost)
  art_start_time <- input$art_start_time10
  art_rate <- input$art_rate10
  sep_cost <- 171
  sep_exchange_frequency <- input$sep_exchange_frequency10
  sep_start_time <- input$sep_start_time10
  sep_compliance <- input$sep_compliance10
  #sep_enrollment <- floor(budget/sep_cost)
  sep_enrollment <- min(networkSize,floor(budget/sep_cost))
  total_cost = 0 
  
  if (whether_prep==1) {
    total_cost = prep_cost * prep_number
  }
  if (whether_art==1) {
    total_cost = art_cost * art_number
  }
  if (whether_sep==1) {
    total_cost = sep_cost * sep_enrollment
  }
  
  qaly_s = 1
  qaly_i1 = 0.74
  qaly_i2 = 0.78
  qaly_i3 = 0.78
  
  source("infect.R", local = TRUE)
  source("progress.R", local = TRUE)
  
  nw <- network.initialize(n=networkSize, directed = FALSE)
  nw <- set.vertex.attribute(nw,"prep_status",rep_len(0,networkSize))
  nw <- set.vertex.attribute(nw,"art_status",rep_len(0,networkSize))
  formation <- ~edges 
  target.stats <- c(edges)
  coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
  est <- netest(nw, formation, target.stats, coef.diss)
  param <- param.net(inf.probAcute = 1, inf.probStable = 0.12, prep_efficacy= 0.735,
                     art_efficacy = 0.96, act.rate = 2, i1i2.rate = 1/21)
  art_vector = get.vertex.attribute(nw,"art_status")
  vertex_ids <- c(1:networkSize)
  nodes_i1 = sample(vertex_ids,initAcute,replace = FALSE)
  index_can_get_infected = setdiff(vertex_ids,nodes_i1)
  nodes_i2 = sample(index_can_get_infected,initStable,replace = FALSE)
  status_vector = c(rep("s",networkSize))
  status_vector[nodes_i1] = "i"
  status_vector[nodes_i2] = "i2"
  init <- init.net(status.vector = status_vector)
  control <- control.net(type = "SI",
                         nsteps = input$nsteps,
                         nsims = input$nsims,
                         infection.FUN = infect,
                         progress.FUN = progress,
                         recovery.FUN = NULL,
                         skip.check = TRUE,
                         depend = FALSE,
                         verbose.int = 0)
  sim <- netsim(est, param, init, control)
})





