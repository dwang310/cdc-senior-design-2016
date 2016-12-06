# Initial
totalCost <- eventReactive(input$replot, {
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
    } else if(input$dropdown == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})

totalQalys <- eventReactive(input$replot, {
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
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})






# M1
totalCostM1 <- eventReactive(input$actionM1, {
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
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
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
    } else if(input$dropdownM1 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM1 <- eventReactive(input$actionM1, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM1())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})

# M2
totalCostM2 <- eventReactive(input$actionM2, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time2
    prep_rate <- input$prep_rate2
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time2
    art_rate <- input$art_rate2
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency2
    sep_start_time <- input$sep_start_time2
    sep_compliance <- input$sep_compliance2
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM2 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM2 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM2 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM2 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM2 <- eventReactive(input$actionM2, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM2())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]* qaly_s +simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})


# M3
totalCostM3 <- eventReactive(input$actionM3, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time3
    prep_rate <- input$prep_rate3
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time3
    art_rate <- input$art_rate3
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency3
    sep_start_time <- input$sep_start_time3
    sep_compliance <- input$sep_compliance3
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM3 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM3 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM3 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM1 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM3 <- eventReactive(input$actionM3, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM3())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})


# M4
totalCostM4 <- eventReactive(input$actionM4, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time4
    prep_rate <- input$prep_rate4
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time4
    art_rate <- input$art_rate4
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency4
    sep_start_time <- input$sep_start_time4
    sep_compliance <- input$sep_compliance4
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM4 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM4 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM4 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM4 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM4 <- eventReactive(input$actionM4, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM4())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})



# M5
totalCostM5 <- eventReactive(input$actionM5, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time5
    prep_rate <- input$prep_rate5
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time5
    art_rate <- input$art_rate5
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency5
    sep_start_time <- input$sep_start_time5
    sep_compliance <- input$sep_compliance5
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM5 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM5 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM5 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM5 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM5 <- eventReactive(input$actionM5, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM5())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})

# M6
totalCostM6 <- eventReactive(input$actionM6, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time6
    prep_rate <- input$prep_rate6
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time6
    art_rate <- input$art_rate6
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency6
    sep_start_time <- input$sep_start_time6
    sep_compliance <- input$sep_compliance6
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM6 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM6 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM6 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM6 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM6 <- eventReactive(input$actionM6, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM6())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})


# M7
totalCostM7 <- eventReactive(input$actionM7, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time7
    prep_rate <- input$prep_rate7
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time7
    art_rate <- input$art_rate7
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency7
    sep_start_time <- input$sep_start_time7
    sep_compliance <- input$sep_compliance7
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM7 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM7 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM7 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM7 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM7 <- eventReactive(input$actionM7, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM7())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})


# M8
totalCostM8 <- eventReactive(input$actionM8, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time8
    prep_rate <- input$prep_rate8
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time8
    art_rate <- input$art_rate8
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency8
    sep_start_time <- input$sep_start_time8
    sep_compliance <- input$sep_compliance8
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM8 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM8 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM8 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM8 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM8 <- eventReactive(input$actionM8, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM8())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})



# M9
totalCostM9 <- eventReactive(input$actionM9, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time9
    prep_rate <- input$prep_rate9
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time9
    art_rate <- input$art_rate9
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency9
    sep_start_time <- input$sep_start_time9
    sep_compliance <- input$sep_compliance9
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM9 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM9 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM9 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM9 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM9 <- eventReactive(input$actionM9, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM9())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s +simulationData_by_week$i.num[52]*qaly_i1 +
            simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))) {
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
            simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})


# M10
totalCostM10 <- eventReactive(input$actionM10, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    budget <- input$budget
    prep_cost <- 16260.23
    prep_number <- floor(budget/prep_cost)
    prep_start_time <- input$prep_start_time10
    prep_rate <- input$prep_rate10
    art_cost <- 20600
    art_number <- floor(budget/art_cost)
    art_start_time <- input$art_start_time10
    art_rate <- input$art_rate10
    sep_cost <- 171
    sep_exchange_frequency <- input$sep_exchange_frequency10
    sep_start_time <- input$sep_start_time10
    sep_compliance <- input$sep_compliance10
    sep_enrollment <- min(networkSize,floor(budget/sep_cost))
    if(input$dropdownM10 == 1){
        whether_sep <- 1
        whether_art <- 0
        whether_prep <- 0
    } else if(input$dropdownM10 == 2) {
        whether_art <- 1
        whether_sep <- 0
        whether_prep <- 0
    } else if(input$dropdownM10 == 3) {
        whether_prep <- 1
        whether_art <- 0
        whether_sep <- 0
    } else if(input$dropdownM10 == 4){
        whether_prep <- 0 
        whether_art <- 0
        whether_sep <- 0
    }
    
    total_cost = 0 
    if (whether_prep==1) {
        total_cost = prep_cost * prep_number
    } else if (whether_art==1) {
        total_cost = art_cost * art_number
    } else if (whether_sep==1) {
        total_cost = sep_cost * sep_enrollment
    } else {total_cost = 0}
    
})
totalQalysM10 <- eventReactive(input$actionM10, {
    qaly_s = 1
    qaly_i1 = 0.74
    qaly_i2 = 0.78
    qaly_i3 = 0.78
    simulationData <- as.data.frame(plotValM10())
    simulationData$i.num[1] <- initAcute
    simulationData$i2.num[1] <- initStable
    simulationData$s.num[1] <- networkSize - simulationData$i2.num[1] - simulationData$i.num[1]
    i1ANDi2 <- simulationData$i.num + simulationData$i2.num
    simulationData <- cbind(simulationData, i1ANDi2)
    simulationData_by_week <- simulationData[c(7,14,21,28,35,42,49,56,63,70,77,84,91,
                                               98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,
                                               224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,
                                               350,357,364),]
    if (is.null(simulationData_by_week$i3.num)){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 +
                        simulationData_by_week$i2.num[52]*qaly_i2
        return(total_QALYs)
    }
    if (!(is.null(simulationData_by_week$i3.num))){
        total_QALYs <- simulationData_by_week$s.num[52]*qaly_s + simulationData_by_week$i.num[52]*qaly_i1 + 
                        simulationData_by_week$i2.num[52]*qaly_i2 + simulationData_by_week$i3.num[52]*qaly_i3
        return(total_QALYs)
    }
})










