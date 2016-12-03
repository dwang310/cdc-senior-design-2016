
#####################################################
#       Make sure to source dependencies first!     #
#                        DW                         #
#####################################################

source("init/init_libraries.R")
# http://www.htmlwidgets.org/showcase_datatables.html
# http://kateto.net/network-visualization

shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)
  init_var <- reactive({input$initstart})
  init_plotVal <- eventReactive(input$initstart, {
    # PUT NO INTERVENTION CODE HERE
  })
  
  
  # Dynamic UI Code Logic
  inserted <- c()
  inserted2 <- c()
  observeEvent(input$addBtn,{
    btn <- input$addBtn
    id <- paste0('dropdownM', btn)
    id2 <- paste0('actionM', btn)
    insertUI(
      selector = '#placeholder',
      #selector = '#addBtn',
      #where = "beforeBegin",
      ui = bootstrapPage(
        
        div(style = "display:inline-block", selectInput(id, label = h5(paste0("Intervention Strategy Module ", btn, " Simulation Parameters:")), 
                                                        choices = list("Syringe Exchange Program" = 1, 
                                                                       "Anti-Retoviral Therapy" = 2, 
                                                                       "Pre-Exposure Prophylaxis" = 3, 
                                                                       "No Intervention" = 4), 
                                                        selected = 4)  ),
        div(style = "display:inline-block", actionButton(id2, label = "Run", class = "btn-primary btn-sm")),
        tags$style(type="text/css", paste0("#",id2 ," { width:100%; margin-bottom: 25px;}")),
        
        # SEP
        conditionalPanel(
          condition = paste0("input.",id," == 1"),
          h4(paste0("SEP Intervention Module ", btn)),
          numericInput(paste0("sep_exchange_frequency",btn), label = "Average frequency people exchange needles (days)", min =0, value=2),
          sliderInput(paste0("sep_compliance",btn), label = "SEP Compliance Rate", min = 0, max = 1, value = 0.9),
          sliderInput(paste0("sep_start_time",btn), label = "SEP Start Time (Days)", min = 0, max = 365, value=50),
          numericInput(paste0("sep_enrollment",btn), label = "Number of people enrolled in SEP", min=0, value=0)
        ),
        
        # ART
        conditionalPanel(
          condition = paste0("input.",id," == 2"),
          h4(paste0("ART Intervention Module ", btn)),
          sliderInput(paste0("art_start_time",btn), label = "ART Start Time (Days)", min = 0, max = 365, value = 5),
          numericInput(paste0("art_number",btn), label = "Number of people put on ART", min=0, value=50),
          numericInput(paste0("art_rate",btn), label = "Number of people put on ART per Day", min = 0, value = 2)
        ),
        
        # PREP
        conditionalPanel(
          condition = paste0("input.",id," == 3"),
          h4(paste0("PREP Intervention Module ", btn)),
          numericInput(paste0("prep_number",btn), label = "Number of people put on PREP", min = 0, value = 10),
          sliderInput(paste0("prep_start_time",btn), label = "PREP Start Time (Days)", min=0, max=365, value=181),
          numericInput(paste0("prep_rate",btn), label = "Number of people put on PREP per Day", min=0,value=2)
        )
        
        
      )
    ) # END Insert UI
    
    inserted <<- c(id, inserted)
    print(paste0("before",inserted))
    inserted2 <<- c(id2, inserted2)
  })
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      #selector = paste0("$(",'#', inserted[length(inserted)],")")
      selector = "#dropdownM1"
    )
    inserted <<- inserted[-length(inserted)]
    #print(paste0("after",inserted))
  })
  
  # First sim value calculator
  plotVal <- eventReactive(input$actionM1, {
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
    } else if(input$dropdownM1 == 4) {
      whether_prep <- 0
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
  
  # User Greeting Logic
  output$textIntro <- renderText({
    
    if(input$radchoice==1){
      user.name = "Regular User"
    } else { user.name = "Advanced User"}
    
    paste("Welcome,", user.name)
  })
  
  #outputOptions(output, 'initUpload', suspendWhenHidden=FALSE)
  
  # Plot charts
  output$result <- renderPlotly({
    sim <- as.data.frame(plotVal())
    Time <- sim$time
    No.susceptable <- sim$s.num
    Prevalence_i1 <- sim$i.num
    Prevalence_i2 <- sim$i2.num
    No.population <- sim$num
    Incidence_si <- sim$si.flow
    Incidence_i1i2 <- sim$i1i2.flow
    
    x <- list(
      title = "Time (days)"
    )
    
    y <- list(
      title = "Metrics"
    )
    
    p1 <- plot_ly(y = ~Prevalence_i1, x=~Time, name="Stage I1 Prevalence", type = "scatter", mode = "lines") %>%
      add_trace(y = ~No.susceptable, name = "Susceptable Population", mode = "lines") %>%
      add_trace(y = ~Prevalence_i2, name = "Stage I2 Prevalence", mode = "lines") %>%
      add_trace(y = ~Incidence_si, name = "Sus. to I1 Incidence", mode = "lines") %>%
      layout(xaxis = x, yaxis = y )
    
  })
  
  # Plot network
  output$networkResult <- renderPlot({
    plot(plotVal(), type = "network", at = input$networkTime, col.status = TRUE)
  })
  
  # Render text summary
  output$networkText <- renderText({
    get_network(plotVal(), sim = input$networkSimSelect)
  })
  
  
  # Calculate total_cost (1)
  total_cost <- eventReactive(input$actionM1, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
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
    sep_enrollment <- floor(budget/sep_cost)
    total_cost = 0 
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
  })
  
  output$totalCostTextInit <- renderText({
    paste0("Total Cost: USD$",total_cost())
  })
  
  # Calculate QALY
  total_QALYs <- eventReactive(input$actionM1, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotVal())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    y_range <- range(0,max(simulationData$i1ANDi2)) 
    plot(simulationData$time, simulationData$i.num, type="l", col="blue", ylim=y_range)
    #plot(sim,type="network")
    lines(simulationData$time, simulationData$i2.num, type="l", col="red")
    lines(simulationData$time, simulationData$i1ANDi2, type="l", col="black")
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
      simulationData_by_week <- simulationData_by_week %>% mutate("QALYs"= 
                                                                    simulationData_by_week$s.num*qaly_s/52 + 
                                                                    simulationData_by_week$i.num*qaly_i1/52 +
                                                                    simulationData_by_week$i2.num*qaly_i2/52) 
      simulationData_by_week <- simulationData_by_week %>% mutate("Discounted QALYs"=
                                                                    simulationData_by_week$QALYs/(1.03)^(simulationData_by_week$time/7 - 1))
      total_QALYs <- sum(simulationData_by_week$`Discounted QALYs`)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
      simulationData_by_week <- simulationData_by_week %>% mutate("QALYs"= 
                                                                    simulationData_by_week$s.num*qaly_s/52 + 
                                                                    simulationData_by_week$i.num*qaly_i1/52 +
                                                                    simulationData_by_week$i2.num*qaly_i2/52 + 
                                                                    simulationData_by_week$i3.num*qaly_i3/52)
      simulationData_by_week <- simulationData_by_week %>% mutate("Discounted QALYs"=
                                                                    simulationData_by_week$QALYs/(1.03)^(simulationData_by_week$time/7 - 1))
      total_QALYs <- sum(simulationData_by_week$`Discounted QALYs`)
    }
  })
  
  output$totalQALYTextInit <- renderText({
    paste0("Total QALYs:",total_QALYs())
    print(total_QALYs())
  })
  
  
  
  
  
  # EXAMPLE ALERT
  #createAlert(session, "initAlert", title = "Oops!", content = "TEST ALERT")
  
  
})

## Important links
# http://shiny.rstudio.com/reference/shiny/latest/updateSliderInput.html