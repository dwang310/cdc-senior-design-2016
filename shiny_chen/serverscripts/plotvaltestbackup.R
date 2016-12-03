
observeEvent(input$addBtn, {
  btn <- input$addBtn
  btn.id <- paste0('actionM', btn)
  print(paste0("file: plotVal_test.R: btn.id = ", btn.id))
  
  assign(
    
    paste0("plotValM", btn),
    # input$actionMX <- eval(parse(text = paste0("input$actionM",btn)))
    observeEvent( eval(parse(text = paste0("input$",btn.id))), {
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
      
      if(input$dropdown == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
      } else if(input$dropdown == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
      } else if(input$dropdown == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
      }
      
      budget <- input$budget
      prep_cost <- 16260.23
      prep_number <- floor(budget/prep_cost)
      #prep_start_time <- input$prep_start_time
      prep_start_time <- eval(parse(text = paste0("input$prep_start_time",btn)))
      #prep_rate <- input$prep_rate
      prep_rate <- eval(parse(text = paste0("input$prep_rate",btn)))
      art_cost <- 20600
      art_number <- floor(budget/art_cost)
      #art_start_time <- input$art_start_time
      art_start_time <- eval(parse(text = paste0("input$art_start_time",btn)))
      #art_rate <- input$art_rate
      art_rate <- eval(parse(text = paste0("input$art_rate",btn)))
      sep_cost <- 171
      #sep_exchange_frequency <- input$sep_exchange_frequency
      sep_exchange_frequency <- eval(parse(text = paste0("input$sep_exchange_frequency",btn)))
      #sep_start_time <- input$sep_start_time
      sep_start_time <- eval(parse(text = paste0("input$sep_start_time",btn)))
      #sep_compliance <- input$sep_compliance
      sep_compliance <- eval(parse(text = paste0("input$sep_compliance",btn)))
      sep_enrollment <- floor(budget/sep_cost)
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
      # Final value
      sim <- netsim(est, param, init, control)
    })
    
  )
  
})



