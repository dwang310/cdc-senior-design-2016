# plotValM1
# plotValM2
# plotValM3 etc...

# plotValMX = eval(parse(text = paste0("plotValM", btn)))

observeEvent(input$addBtn, {
  btn <- input$addBtn
  btn.id <- paste0('actionM', btn)
  
  
  assign(
    
    paste0("output$resultM", btn), # e.g.) output$resultM1 , output$resultM2 , etc...
    renderPlotly({
      source("constants.R", local = TRUE)
      simulationData <- as.data.frame(eval(parse(text = paste0("plotValM", btn))))
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
        title = "Time (days)",
        rangeslider = list(type = "date")
      )
      
      y <- list(
        title = "Prevalence"
      )
      
      p1 <- plot_ly(y = ~Prevalence_i1ANDi2, x=~Time, name="All-stage Prevalence", type = "scatter", mode = "lines") %>%
        layout(xaxis = x, yaxis = y)
    })
    
  )
  
  
})