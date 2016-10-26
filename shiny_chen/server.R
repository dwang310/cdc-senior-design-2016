
#####################################################
#       Make sure to source dependencies first!     #
#                        DW                         #
#####################################################

source("init/init_libraries.R")
library(plotly)


shinyServer(function(input, output, session) {
        
        # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file.
        # file$datapath -> gives the path of the file
        
        
  # EACH TIME UNIT IS 7 DAYS 
        ##Epidemic Simulation
        param <- reactive({
          param.net(inf.prob = 0.05,
                    act.rate = 5,
                    inter.start = input$inter.start,
                    inter.eff = input$inter.eff)
        })
        init <- reactive({
          init.net(i.num = 11,
                   status.rand = TRUE)
        })
        control <- reactive({
          control.net(type = "SI",
                      nsims = input$nsims,
                      nsteps = input$nsteps,
                      verbose = FALSE)
        })
        
        
        # Running SIR EpiModel
        plotVal <- eventReactive(input$replot, {
            
            # Our simulated model
            networkSize <- 196 
            percentMales <- 0.577
            percentWhite <- 0.985
            percentIncome10K <- 0.919
            percentIncarcerated <- 0.542
            avgDegree <- 2
            avgEdgeDuration <- 90
            
            avgEdges <- (networkSize*avgDegree)/2
            
            nw <- network.initialize(n=networkSize, directed=FALSE)
            nw <- set.vertex.attribute(nw, "Gender", sample(c(0,1),size=networkSize,
                                                            prob=c(1-percentMales,percentMales),replace=TRUE))
            nw <- set.vertex.attribute(nw, "Race", sample(c(0,1),size=networkSize,
                                                          prob=c(1-percentWhite,percentWhite),replace=TRUE))
            nw <- set.vertex.attribute(nw, "Race", sample(c(0,1),size=networkSize,
                                                          prob=c(1-percentIncome10K,percentIncome10K),replace=TRUE))
            nw <- set.vertex.attribute(nw, "Incarceration", sample(c(0,1),size=networkSize,
                                                                   prob=c(1-percentIncarcerated, percentIncarcerated),replace=TRUE))
            formation <- ~edges
            
            target.stats <- c(avgEdges)
            coef.diss <- dissolution_coefs(~offset(edges), avgEdgeDuration)
            
            modelFit <- netest(nw, formation, target.stats, coef.diss)
            diagnositcs <- netdx(modelFit, nsims = input$nsims, nsteps =input$nsteps)
            sim <- netsim(modelFit, param(), init(), control())
            dataSim <- as.data.frame(sim)
            
            
        })
        
        plotVal2 <- eventReactive(input$replot, {
            # Data from Phillip Paper
            cumorigdata <- read.csv("cumorigdata.csv")
            dataOrig <- as.data.frame(cumorigdata)
        })
        
        plotVal3 <- eventReactive(input$replot, {
            origdata <- read.csv("origdata.csv")
            dataOrig.nc <- as.data.frame(origdata)
        })
        
        plotValNet <- eventReactive(input$replot, {
            networkData <- get_transmat(sim, sim = 1)
        })
        
        output$result <- renderPlotly({
            
            p1 <- plot_ly(plotVal3(), x=Time, y=Prevalence, name="actual")
            p1 %>% add_trace(y = plotVal()$si.flow, name="simulated")
            
            
            #par(mfrow = c(2,1))
            
            #plot(plotVal(), y = "i.num", col=2)
            #lines(plotVal2(), col=3)
            
            #plot(plotVal(), y = "si.flow", col=2)
            #lines(plotVal3(), col=3)
          
          
          #par(mfrow = c(1,2), mar = c(0,0,1,0))
          #plot(plotVal(), type = "network", at = 1, col.status = TRUE,
               #main = "Prevalence at t1")
          #plot(plotVal(), type = "network", at = 100, col.status = TRUE,
               #main = "Prevalence at t500")
                
                
        })
        
        output$result2 <- renderPlotly({
            p2 <- plot_ly(plotVal2(), x=Time, y=Number.of.Infected, name="actual")
            p2 %>% add_trace(y=plotVal()$i.num, name="simulated")
        })
        
        
        
        #output$textSummary <- renderPrint({
         #   paste(summary(plotVal(), at=input$nsteps))
        #})
        
})

## Important links
# http://shiny.rstudio.com/reference/shiny/latest/updateSliderInput.html
