#EXAMPLE
source("init/init_libraries.R")
library(plotly)

shinyUI(fluidPage(
  headerPanel("Network Modeling"), # end headerPanel
  sidebarLayout(
    
    sidebarPanel(
      h2("Input parameters below."),
      
      
      numericInput("nsims", label=h5("Number of simulations:"), value=10),
      
      sliderInput("nsteps", label=h5("Number of timesteps:"), min=1, max=200, value=50),
      
      sliderInput("inter.eff", label=h5("Intervention efficacy:"), min=0, max=1, value=0),
      
      sliderInput("inter.start", label=h5("Intervention Start Time:"), min=0, max=200, value=26),
      
      actionButton("replot",icon = icon("bar-chart", "fa-3x"), label=h3("PLOT RESULTS"))
      
      
    ), # end sidebarPanel
    
    
    mainPanel(
        tabsetPanel(type = 'tabs',
                    tabPanel("Prevalence vs. Time", plotlyOutput("result", height="auto")),
                    tabPanel("No. of Infected vs. Time", plotlyOutput("result2", height="auto")),
                    tabPanel("Network Visualization",numericInput("time", label=h5("Time:"), value=2))
                    
                    )
      
    )# end mainPanel
  )# end sidebarLayout
  
  
  
  
  
  
)# end fluidPage
)# end shinyUI