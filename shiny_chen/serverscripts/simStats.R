
final <<- reactiveValues(df = NULL)

caseNum <- eventReactive(input$replot, {
    networkSize <- 550
    simData <- as.data.frame(plotVal())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

observeEvent(input$replot,{
    trial <- c(as.integer(0))
    numCases <- c(caseNum())
    avertedCases <- c(0)
    dPrev <- c(0) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(0) # Q_x - Q_o
    totalCost <- c(totalCost())
    totalQalys <- c(totalQalys())
    #final.table <- data.frame(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys)
    final$df <- data.frame(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys)
})

output$table <- renderTable({
    names(final$df)[names(final$df)=="trial"] <- "Scenario"
    names(final$df)[names(final$df)=="numCases"] <- "Number of Cases"
    names(final$df)[names(final$df)=="avertedCases"] <- "Number of Averted Cases"
    names(final$df)[names(final$df)=="dPrev"] <- "Change in Prevalence"
    names(final$df)[names(final$df)=="qalyGain"] <- "Gain in Total QALYs"
    names(final$df)[names(final$df)=="totalCost"] <- "Total Cost ($USD)"
    names(final$df)[names(final$df)=="totalQalys"] <- "Tota QALYs"
    final$df
})


# M1
caseNumM1 <- eventReactive(input$actionM1, {
    networkSize <- 550
    simData <- as.data.frame(plotValM1())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM1 <- eventReactive(input$actionM1, {
    value = caseNum() - caseNumM1()
})

observeEvent(input$actionM1, {
    trial <- c(as.integer(1))
    numCases <- c(caseNumM1())
    avertedCases <- c(caseAvertedM1())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM1() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM1())
    totalQalys <- c(totalQalysM1())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})



# M2
caseNumM2 <- eventReactive(input$actionM2, {
    networkSize <- 550
    simData <- as.data.frame(plotValM2())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM2 <- eventReactive(input$actionM2, {
    value = caseNum() - caseNumM2()
})

observeEvent(input$actionM2, {
    trial <- c(as.integer(2))
    numCases <- c(caseNumM2())
    avertedCases <- c(caseAvertedM2())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM2() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM2())
    totalQalys <- c(totalQalysM2())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})



# M3
caseNumM3 <- eventReactive(input$actionM3, {
    networkSize <- 550
    simData <- as.data.frame(plotValM3())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM3 <- eventReactive(input$actionM3, {
    value = caseNum() - caseNumM3()
})

observeEvent(input$actionM3, {
    trial <- c(as.integer(3))
    numCases <- c(caseNumM3())
    avertedCases <- c(caseAvertedM3())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM3() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM3())
    totalQalys <- c(totalQalysM3())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M4
caseNumM4 <- eventReactive(input$actionM4, {
    networkSize <- 550
    simData <- as.data.frame(plotValM4())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM4 <- eventReactive(input$actionM4, {
    value = caseNum() - caseNumM4()
})

observeEvent(input$actionM4, {
    trial <- c(as.integer(4))
    numCases <- c(caseNumM4())
    avertedCases <- c(caseAvertedM4())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM4() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM4())
    totalQalys <- c(totalQalysM4())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M5
caseNumM5 <- eventReactive(input$actionM5, {
    networkSize <- 550
    simData <- as.data.frame(plotValM5())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM5 <- eventReactive(input$actionM5, {
    value = caseNum() - caseNumM5()
})

observeEvent(input$actionM5, {
    trial <- c(as.integer(5))
    numCases <- c(caseNumM5())
    avertedCases <- c(caseAvertedM5())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM5() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM5())
    totalQalys <- c(totalQalysM5())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M6
caseNumM6 <- eventReactive(input$actionM6, {
    networkSize <- 550
    simData <- as.data.frame(plotValM6())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM6 <- eventReactive(input$actionM6, {
    value = caseNum() - caseNumM6()
})

observeEvent(input$actionM6, {
    trial <- c(as.integer(6))
    numCases <- c(caseNumM6())
    avertedCases <- c(caseAvertedM6())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM6() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM6())
    totalQalys <- c(totalQalysM6())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M7
caseNumM7 <- eventReactive(input$actionM7, {
    networkSize <- 550
    simData <- as.data.frame(plotValM7())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM7 <- eventReactive(input$actionM7, {
    value = caseNum() - caseNumM7()
})

observeEvent(input$actionM7, {
    trial <- c(as.integer(7))
    numCases <- c(caseNumM7())
    avertedCases <- c(caseAvertedM7())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM7() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM7())
    totalQalys <- c(totalQalysM7())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M8
caseNumM8 <- eventReactive(input$actionM8, {
    networkSize <- 550
    simData <- as.data.frame(plotValM8())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM8 <- eventReactive(input$actionM8, {
    value = caseNum() - caseNumM8()
})

observeEvent(input$actionM8, {
    trial <- c(as.integer(8))
    numCases <- c(caseNumM8())
    avertedCases <- c(caseAvertedM8())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM8() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM8())
    totalQalys <- c(totalQalysM8())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M9
caseNumM9 <- eventReactive(input$actionM9, {
    networkSize <- 550
    simData <- as.data.frame(plotValM9())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM9 <- eventReactive(input$actionM9, {
    value = caseNum() - caseNumM9()
})

observeEvent(input$actionM9, {
    trial <- c(as.integer(9))
    numCases <- c(caseNumM9())
    avertedCases <- c(caseAvertedM9())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM9() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM9())
    totalQalys <- c(totalQalysM9())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})




# M10
caseNumM10 <- eventReactive(input$actionM10, {
    networkSize <- 550
    simData <- as.data.frame(plotValM10())
    sNum <- simData$s.num[input$nsteps]
    caseNum <- networkSize - sNum
})

caseAvertedM10 <- eventReactive(input$actionM10, {
    value = caseNum() - caseNumM10()
})

observeEvent(input$actionM10, {
    trial <- c(as.integer(10))
    numCases <- c(caseNumM10())
    avertedCases <- c(caseAvertedM10())
    dPrev <- c(avertedCases/550) # Number of Cases averted / Number of nodes in network
    qalyGain <- c(totalQalysM10() - totalQalys()) # Q_x - Q_o
    totalCost <- c(totalCostM10())
    totalQalys <- c(totalQalysM10())
    final$df <- rbind(final$df, c(trial, numCases, avertedCases, dPrev, qalyGain, totalCost, totalQalys))
})







