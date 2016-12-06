#PLOT M0

# FOR LEGEND NAMING
legendNameM0 <- reactive({
    if (input$dropdown == 1) {
        legendNameM0 = "SEP M0"
    } else if (input$dropdown == 2) {
        legendNameM0 = "ART M0"
    } else if (input$dropdown == 3) {
        legendNameM0 = "PREP M0"
    } else {legendNameM0 = "NI  M0"}
    
})

xc <<- list( title = "Total Cost (USD)")
yc <<- list( title = "Total QALYs")

cPlotObjectInit <<- reactiveValues(p = NULL)
observe(
    cPlotObjectInit$p <<- plot_ly(y = ~totalQalys(), x = ~totalCost(), name = legendNameM0(), size = 1, 
                                  type = "scatter", mode = "markers",
                                  marker = list(size = 15, bgcolor = "#D3D3D3")) %>%
        layout(title = "Total Cost vs. Total QALYs", xaxis = xc, yaxis = yc)
)


output$plotCostVal <- renderPlotly({
    cPlotObjectInit$p
})


#PLOT M1
legendNameM1 <- reactive({
    if (input$dropdownM1 == 1) {
        legendNameM1 = "SEP M1"
    } else if (input$dropdownM1 == 2) {
        legendNameM1 = "ART M1"
    } else if (input$dropdownM1 == 3) {
        legendNameM1 = "PREP M1"
    } else {legendNameM1 = "NI  M1"}
})

observeEvent(input$actionM1, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM1(), x=~totalCostM1(), 
                                  type = "scatter", name = legendNameM1())
})


#PLOT M2
legendNameM2 <- reactive({
    if (input$dropdownM2 == 1) {
        legendNameM2 = "SEP M2"
    } else if (input$dropdownM2 == 2) {
        legendNameM2 = "ART M2"
    } else if (input$dropdownM2 == 3) {
        legendNameM2 = "PREP M2"
    } else {legendNameM2 = "NI  M2"}
})

observeEvent(input$actionM2, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM2(), x=~totalCostM2(), 
                                   type = "scatter", name = legendNameM2())
})



#PLOT M3
legendNameM3 <- reactive({
    if (input$dropdownM3 == 1) {
        legendNameM1 = "SEP M3"
    } else if (input$dropdownM3 == 2) {
        legendNameM1 = "ART M3"
    } else if (input$dropdownM3 == 3) {
        legendNameM1 = "PREP M3"
    } else {legendNameM1 = "NI  M3"}
})

observeEvent(input$actionM3, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM3(), x=~totalCostM3(), 
                                   type = "scatter", name = legendNameM3())
})



#PLOT M4
legendNameM4 <- reactive({
    if (input$dropdownM4 == 1) {
        legendNameM1 = "SEP M4"
    } else if (input$dropdownM4 == 2) {
        legendNameM1 = "ART M4"
    } else if (input$dropdownM4 == 3) {
        legendNameM1 = "PREP M4"
    } else {legendNameM1 = "NI  M4"}
})

observeEvent(input$actionM4, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM4(), x=~totalCostM4(), 
                                   type = "scatter", name = legendNameM4())
})


#PLOT M5
legendNameM5 <- reactive({
    if (input$dropdownM5 == 1) {
        legendNameM1 = "SEP M5"
    } else if (input$dropdownM5 == 2) {
        legendNameM1 = "ART M5"
    } else if (input$dropdownM5 == 3) {
        legendNameM1 = "PREP M5"
    } else {legendNameM1 = "NI  M5"}
})

observeEvent(input$actionM5, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM5(), x=~totalCostM5(), 
                                   type = "scatter", name = legendNameM5())
})


#PLOT M6
legendNameM6 <- reactive({
    if (input$dropdownM6 == 1) {
        legendNameM1 = "SEP M6"
    } else if (input$dropdownM6 == 2) {
        legendNameM1 = "ART M6"
    } else if (input$dropdownM6 == 3) {
        legendNameM1 = "PREP M6"
    } else {legendNameM1 = "NI  M6"}
})

observeEvent(input$actionM6, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM6(), x=~totalCostM6(), 
                                   type = "scatter", name = legendNameM6())
})



#PLOT M7
legendNameM7 <- reactive({
    if (input$dropdownM7 == 1) {
        legendNameM1 = "SEP M7"
    } else if (input$dropdownM7 == 2) {
        legendNameM1 = "ART M7"
    } else if (input$dropdownM7 == 3) {
        legendNameM1 = "PREP M7"
    } else {legendNameM1 = "NI M7"}
})

observeEvent(input$actionM7, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM7(), x=~totalCostM7(), 
                                   type = "scatter", name = legendNameM7())
})



#PLOT M8
legendNameM8 <- reactive({
    if (input$dropdownM8 == 1) {
        legendNameM1 = "SEP M8"
    } else if (input$dropdownM8 == 2) {
        legendNameM1 = "ART M8"
    } else if (input$dropdownM8 == 3) {
        legendNameM1 = "PREP M8"
    } else {legendNameM1 = "NI M8"}
})

observeEvent(input$actionM8, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM8(), x=~totalCostM8(), 
                                   type = "scatter", name = legendNameM8())
})



#PLOT M9
legendNameM9 <- reactive({
    if (input$dropdownM9 == 1) {
        legendNameM1 = "SEP M9"
    } else if (input$dropdownM9 == 2) {
        legendNameM1 = "ART M9"
    } else if (input$dropdownM9 == 3) {
        legendNameM1 = "PREP M9"
    } else {legendNameM1 = "NI M9"}
})

observeEvent(input$actionM9, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM9(), x=~totalCostM9(), 
                                   type = "scatter", name = legendNameM9())
})



#PLOT M10
legendNameM10 <- reactive({
    if (input$dropdownM10 == 1) {
        legendNameM1 = "SEP M10"
    } else if (input$dropdownM10 == 2) {
        legendNameM1 = "ART M10"
    } else if (input$dropdownM10 == 3) {
        legendNameM1 = "PREP M10"
    } else {legendNameM1 = "NI  M10"}
})

observeEvent(input$actionM10, {
    cPlotObjectInit$p <- add_trace(cPlotObjectInit$p, y = ~totalQalysM10(), x=~totalCostM1(), 
                                   type = "scatter", name = legendNameM10())
})


