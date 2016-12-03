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











lapply(1:6, function(i) {
  conditionalPanel(
    condition = paste0(paste0("input.dropdownM",i)," == 1"),
    h4(paste0("SEP Intervention Module ", i)),
    numericInput("sep_exchange_frequency", label = "Average frequency people exchange needles (days)", min =0, value=2),
    sliderInput("sep_compliance", label = "SEP Compliance Rate", min = 0, max = 1, value = 0.9),
    sliderInput("sep_start_time", label = "SEP Start Time (Days)", min = 0, max = 365, value=50),
    numericInput("sep_enrollment", label = "Number of people enrolled in SEP", min=0, value=0)
  )
}),