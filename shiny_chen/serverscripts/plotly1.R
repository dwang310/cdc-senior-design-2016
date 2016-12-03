simulationData <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotVal())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})

x <<- list( title = "Time (days)", rangeslider = list(type = "date"))
y <<- list( title = "Prevalence")

plotObjectInit <<- reactiveValues(p = NULL)
plotObjectInit$p <<- plot_ly(y = ~simulationData()$pTotal, x=~simulationData()$time, name="Prevalence M0", type = "scatter", mode = "lines") %>%
  layout(xaxis = x, yaxis = y)

output$result <- renderPlotly({
  #simulationData <- simulationData()
  #y_range <- range(0,max(simulationData$i1ANDi2))
  #plot(simulationData$time, simulationData$i.num, type="l", col="blue", ylim=y_range)
  #Time <- simulationData$time
  #Prevalence_i1 <- simulationData$i.num 
  #Prevalence_i2 <- simulationData$i2.num
  # Only need to plot this per (since i3 is empty)
  #Prevalence_i1ANDi2 <- simulationData$i1ANDi2
  #x <- list( title = "Time (days)", rangeslider = list(type = "date"))
  #y <- list( title = "Prevalence")
  plotObjectInit$p
})
