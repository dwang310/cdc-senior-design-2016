

# first experiment 
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



# second experiment 
output$result <- renderPlotly({
  source("constants.R")
  simulationData <- as.data.frame(plotVal())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  i1ANDi2 <- simulationData$i.num + simulationData$i2.num
  simulationData <- cbind(simulationData, i1ANDi2)
  y_range <- range(0,max(simulationData$i1ANDi2))
  
  plot(simulationData$time, simulationData$i.num, type="l", col="blue", ylim=y_range)
  
  Time <- simulationData$time
  #Prevalence_i1 <- simulationData$i.num 
  #Prevalence_i2 <- simulationData$i2.num
  
  # Only need to plot this per (since i3 is empty)
  Prevalence_i1ANDi2 <- simulationData$i1ANDi2
  
  
  x <- list(
    title = "Time (days)"
  )
  
  y <- list(
    title = "Metrics"
  )
  
  p1 <- plot_ly(y = ~Prevalence_i1, x=~Time, name="Stage I1 Prevalence", type = "scatter", mode = "lines") %>%
    add_trace(y = ~Prevalence_i1ANDi2, name = "Stage I1 and I2 Prevalence", mode = "lines") %>%
    add_trace(y = ~Prevalence_i2, name = "Stage I2 Prevalence", mode = "lines") %>%
    layout(xaxis = x, yaxis = y )
  
})