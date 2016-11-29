
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
        observeEvent(input$addBtn,{
          btn <- input$addBtn
          id <- paste0('dropdownM', btn)
          id2 <- paste0('actionM', btn)
          print(id)
          insertUI(
            selector = '#placeholder',
            #selector = '#addBtn',
            #where = "beforeBegin",
            ui = bootstrapPage(
              
              div(style = "display:inline-block", selectInput(id, label = h5("Select intervention strategy to simulate:"), 
                                                              choices = list("Syringe Exchange Program" = 1, "Anti-Retoviral Therapy" = 2, "Pre-Exposure Prophylaxis" = 3, "No Intervention" = 4), 
                                                              selected = 4)  ),
              div(style = "display:inline-block", actionButton(id2, label = "Run", class = "btn-primary btn-sm")),
              tags$style(type="text/css", paste0("#",id2 ," { width:100%; margin-bottom: 25px;}"))
              
                          )
          ) # END Insert UI
          
          inserted <<- c(id, inserted)
        })
        
        observeEvent(input$removeBtn, {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[length(inserted)])
          )
          inserted <<- inserted[-length(inserted)]
        })
        
        
        
        
        # First sim value calculator
        plotVal <- eventReactive(input$replot, {
          proggy <- Progress$new(session, min=1, max=15)
          on.exit(proggy$close())
          
          proggy$set(message = "Running simulation...", detail = "This may take a while...")
          for(i in 1:input$nsims){
            proggy$set(value = i)
          }
          
          source("constants.R", local = TRUE)
        
          if(input$radchoice==2){
            networkSize <- input$networkSize 
            percentMales <- input$percentMales
            percentWhite <- input$percentWhite
            percentIncome10K <- input$percentIncome10K
            percentIncarcerated <- input$percentIncarcerated
            avgDegree <- input$avgDegree
            avgEdgeDuration <- input$avgEdgeDuration
            avgEdges <- (networkSize*avgDegree)/2
          } else {
            networkSize <- 550 
            percentMales <- 0.577
            percentWhite <- 0.985
            percentIncome10K <- 0.919
            percentIncarcerated <- 0.542
            avgDegree <- 1.5
            avgEdgeDuration <- 365
            avgEdges <- (networkSize*avgDegree)/2
          }
          
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
          prep_start_time <- input$prep_start_time
          prep_rate <- input$prep_rate
          art_cost <- 20600
          art_number <- floor(budget/art_cost)
          art_start_time <- input$art_start_time
          art_rate <- input$art_rate
          sep_cost <- 171
          sep_exchange_frequency <- input$sep_exchange_frequency
          sep_start_time <- input$sep_start_time
          sep_compliance <- input$sep_compliance
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
        
        
        # Plot cost-effectivness
        output$ceResult <- renderPlotly({
          data <- read.csv("ce_analysos2.csv")
          x <- list(
            title = "Time (weeks)"
          )
          
          y <- list(
            title = "Cumulative QALY"
          )
          p1 <- plot_ly(y = ~data$PREP, x = ~data$Week, name = "PrEP", type = "scatter", mode = "lines") %>%
            add_trace(y = ~data$ART, name = "ART", mode = "lines") %>%
            add_trace(y = ~data$SEP, name = "SEP", mode = "lines") %>%
            add_trace(y = ~data$PF, name = "Pareto Frontier", mode = "lines") %>%
            layout( xaxis = x , yaxis = y)
        })
        # EXAMPLE ALERT
        #createAlert(session, "initAlert", title = "Oops!", content = "TEST ALERT")
        
        
})

## Important links
# http://shiny.rstudio.com/reference/shiny/latest/updateSliderInput.html
