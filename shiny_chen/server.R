
#####################################################
#       Make sure to source dependencies first!     #
#                        DW                         #
#####################################################

# Utility functions
first <- function(x) { head(x, n = 1) }

source("init/init_libraries.R", local = TRUE)
# http://www.htmlwidgets.org/showcase_datatables.html
# http://kateto.net/network-visualization
numID <- NULL
inserted <- NULL
action.inserted <- NULL
plot.inserted <- NULL

shinyServer(function(input, output, session) {

  
        session$onSessionEnded(stopApp)
        init_var <- reactive({input$initstart})
        init_plotVal <- eventReactive(input$initstart, {
          # PUT NO INTERVENTION CODE HERE
        })
        
        
        # Dynamic UI Code Logic
        source("serverscripts/insertbtn.R", local = TRUE)
        

        # First sim value calculator
        source("serverscripts/plotval1.R", local = TRUE)
        
        source("serverscripts/plotvaltest.R", local = TRUE)
        source("serverscripts/plotlytest.R", local = TRUE)

        # User Greeting Logic
        output$textIntro <- renderText({
          
          if(input$radchoice==1){
            user.name = "Regular User"
          } else { user.name = "Advanced User"}
          
         paste("Welcome,", user.name)
        })
        
        #outputOptions(output, 'initUpload', suspendWhenHidden=FALSE)
        
        # Plot charts
        source("serverscripts/plotly1.R", local = TRUE)
        
        
        
        # Calculate total_cost (1)
        total_cost <- eventReactive(input$replot, {
          qaly_s = 1
          qaly_i1 = 0.74
          qaly_i2 = 0.78
          qaly_i3 = 0.78
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
          sep_enrollment <- min(networkSize,floor(budget/sep_cost))
          total_cost = 0 
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
        total_QALYs <- eventReactive(input$replot, {
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
        

        #reactiveValuesToList(head(as.data.frame(get('plotValM1')))

        
        
        # EXAMPLE ALERT
        #createAlert(session, "initAlert", title = "Oops!", content = "TEST ALERT")
        
        
})

## Important links
# http://shiny.rstudio.com/reference/shiny/latest/updateSliderInput.html
