# M1
DataM1 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM1())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})

name1Val <- reactive({
  if(input$dropdownM1 == 1){
    nameVal = "Prev. (SEP) M1"
  } else if (input$dropdownM1 == 2){
    nameVal = "Prev. (ART) M1"
  } else if (input$dropdownM1 == 3){
    nameVal = "Prev. (PREP) M1"
  }
})
observeEvent(input$actionM1, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM1()$pTotal, x=~DataM1()$time, 
                                type = "scatter", mode = "lines", name = name1Val())
})

  
# M2
DataM2 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM2())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name2Val <- reactive({
  if(input$dropdownM2 == 1){
    nameVal = "Prev. (SEP) M2"
  } else if (input$dropdownM2 == 2){
    nameVal = "Prev. (ART) M2"
  } else if (input$dropdownM2 == 3){
    nameVal = "Prev. (PREP) M2"
  }
})
observeEvent(input$actionM2, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM2()$pTotal, x=~DataM2()$time, 
                                type = "scatter", mode = "lines", name = name2Val())
})

# M3
DataM3 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM3())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name3Val <- reactive({
  if(input$dropdownM3 == 1){
    nameVal = "Prev. (SEP) M3"
  } else if (input$dropdownM3 == 2){
    nameVal = "Prev. (ART) M3"
  } else if (input$dropdownM3 == 3){
    nameVal = "Prev. (PREP) M3"
  }
})
observeEvent(input$actionM3, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM3()$pTotal, x=~DataM3()$time, 
                                type = "scatter", mode = "lines", name = name3Val())
})

# M4
DataM4 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM4())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name4Val <- reactive({
  if(input$dropdownM4 == 1){
    nameVal = "Prev. (SEP) M4"
  } else if (input$dropdownM4 == 2){
    nameVal = "Prev. (ART) M4"
  } else if (input$dropdownM4 == 3){
    nameVal = "Prev. (PREP) M4"
  }
})
observeEvent(input$actionM4, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM4()$pTotal, x=~DataM4()$time, 
                                type = "scatter", mode = "lines", name = name4Val())
})

# M5
DataM5 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM5())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name5Val <- reactive({
  if(input$dropdownM5 == 1){
    nameVal = "Prev. (SEP) M5"
  } else if (input$dropdownM5 == 2){
    nameVal = "Prev. (ART) M5"
  } else if (input$dropdownM5 == 3){
    nameVal = "Prev. (PREP) M5"
  }
})
observeEvent(input$actionM5, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM5()$pTotal, x=~DataM5()$time, 
                                type = "scatter", mode = "lines", name = name5Val())
})

# M6
DataM6 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM6())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name6Val <- reactive({
  if(input$dropdownM6 == 1){
    nameVal = "Prev. (SEP) M6"
  } else if (input$dropdownM6 == 2){
    nameVal = "Prev. (ART) M6"
  } else if (input$dropdownM6 == 3){
    nameVal = "Prev. (PREP) M6"
  }
})
observeEvent(input$actionM6, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM6()$pTotal, x=~DataM6()$time, 
                                type = "scatter", mode = "lines", name = name6Val())
})

# M7
DataM7 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM7())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name7Val <- reactive({
  if(input$dropdownM7 == 1){
    nameVal = "Prev. (SEP) M7"
  } else if (input$dropdownM7 == 2){
    nameVal = "Prev. (ART) M7"
  } else if (input$dropdownM7 == 3){
    nameVal = "Prev. (PREP) M7"
  }
})
observeEvent(input$actionM7, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM7()$pTotal, x=~DataM7()$time, 
                                type = "scatter", mode = "lines", name = name7Val())
})

# M8
DataM8 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM8())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name8Val <- reactive({
  if(input$dropdownM8 == 1){
    nameVal = "Prev. (SEP) M8"
  } else if (input$dropdownM8 == 2){
    nameVal = "Prev. (ART) M8"
  } else if (input$dropdownM8 == 3){
    nameVal = "Prev. (PREP) M8"
  }
})
observeEvent(input$actionM8, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM8()$pTotal, x=~DataM8()$time, 
                                type = "scatter", mode = "lines", name = name8Val())
})

# M9
DataM9 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM9())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name9Val <- reactive({
  if(input$dropdownM9 == 1){
    nameVal = "Prev. (SEP) M9"
  } else if (input$dropdownM9 == 2){
    nameVal = "Prev. (ART) M9"
  } else if (input$dropdownM9 == 3){
    nameVal = "Prev. (PREP) M9"
  }
})
observeEvent(input$actionM9, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM9()$pTotal, x=~DataM9()$time, 
                                type = "scatter", mode = "lines", name = name9Val())
})

# M10
DataM10 <- reactive({
  source("constants.R", local = TRUE)
  simulationData <- as.data.frame(plotValM10())
  simulationData$i.num[1] <- initAcute
  simulationData$i2.num[1] <- initStable
  simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
  if (!(is.null(simulationData$i3.num))) {
    pTotal <- simulationData$i3.num + simulationData$i.num + simulationData$i2.num
  } else {pTotal <- simulationData$i.num + simulationData$i2.num}
  simulationData <- cbind(simulationData, pTotal)
})
name10Val <- reactive({
  if(input$dropdownM10 == 1){
    nameVal = "Prev. (SEP) M10"
  } else if (input$dropdownM10 == 2){
    nameVal = "Prev. (ART) M10"
  } else if (input$dropdownM10 == 3){
    nameVal = "Prev. (PREP) M10"
  }
})
observeEvent(input$actionM10, {
  plotObjectInit$p <- add_trace(plotObjectInit$p, y = ~DataM10()$pTotal, x=~DataM10()$time, 
                                type = "scatter", mode = "lines", name = name10Val())
})



