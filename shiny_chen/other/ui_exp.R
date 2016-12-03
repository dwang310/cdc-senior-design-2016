
source("init/init_libraries.R")

shinyUI(
  navbarPage(theme = shinytheme("lumen"), title = strong("HIV Prevention Simulation Interface (Beta)"),
             tabPanel("Application",
                      sidebarLayout(
                        sidebarPanel(
                          textOutput("textIntro"),
                          selectInput("radchoice", label = h5("User Type"),
                                      choices = list("Regular User" = 1, "Advanced User" = 2), selected = 1),
                          
                          # CONDITIONAL PANEL
                          conditionalPanel(
                            condition = "input.radchoice == 2",
                            numericInput("budget", label = "Budget (USD)", value = 500000)
                          ),
                          # END CONDITIONAL PANEL
                          
                          
                          numericInput("nsims", label=h5("Number of simulations"), value=1),
                          numericInput("nsteps", label=h5("Number of days"), min=1, value=365),
                          
                          
                          tags$div(id = 'placeholder'), 
                          
                          # MORE CONDITIONAL PANELS
                          
                          
                          # OMG
                          br(),
                          br(),
                          actionButton("addBtn", "Add Intervention"),
                          actionButton("removeBtn", "Remove Intervention")
                          #actionButton("plotBtn", "Update Simulation", class = "btn-primary"),
                          
                          
                          
                          
                          
                          #actionButton("replot", "RUN INITIAL MODEL", class = "btn-primary"),
                          #bsTooltip("replot", "Click to run the simulation and plot the output.", trigger="hover")
                          
                        ), # End sidebarPanel
                        mainPanel(
                          tabsetPanel(type = 'tabs',
                                      tabPanel("Plot Visualization", 
                                               bsAlert("initAlert"),
                                               plotlyOutput("result")),
                                      tabPanel("Network Visualization",
                                               numericInput("networkTime", "Timestep", min=1, value = 1),
                                               plotOutput("networkResult"),
                                               numericInput("networkSimSelect", "Simulation summary", min = 1, value = 1),
                                               textOutput("networkText")),
                                      tabPanel("Cost Effectiveness Results", 
                                               textOutput("totalCostTextInit"))
                                      
                          )
                        ) # End mainPanel
                      ) # End sidebarLayout
             ) # End tabPanel (2)
             
  ) # End navbarPage
) # End shinyUi


