
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
                          
                          
                          div(style = "display:inline-block", selectInput("dropdown", label = h4("Intervention Strategy Module 0"), 
                                                                          choices = list("Syringe Exchange Program" = 1, 
                                                                                         "Anti-Retoviral Therapy" = 2, 
                                                                                         "Pre-Exposure Prophylaxis" = 3, 
                                                                                         "No Intervention" = 4), 
                                                                          selected = 4)  ),
                          div(style = "display:inline-block", actionButton("replot", label = "Run", class = "btn-primary btn-sm")),
                          tags$style(type="text/css", paste0("#","replot" ," { width:100%; margin-bottom: 25px;}")),
                          
                          # SEP CONDITIONAL PANEL
                          conditionalPanel(
                            condition = "input.dropdown == 1",
                            h4(paste0("SEP Intervention Module 0")),
                            numericInput("sep_exchange_frequency", label = "Average frequency people exchange needles (days)", min =0, value=2),
                            sliderInput("sep_compliance", label = "SEP Compliance Rate", min = 0, max = 1, value = 0.9),
                            sliderInput("sep_start_time", label = "SEP Start Time (Days)", min = 0, max = 365, value=50),
                            numericInput("sep_enrollment", label = "Number of people enrolled in SEP", min=0, value=0)
                          ),
                          # END SEP CONDITIONAL PANEL
                          
                          
                          # ART CONDITIONAL PANEL
                          conditionalPanel(
                            condition = "input.dropdown == 2",
                            h4(paste0("ART Intevention Module 0")),
                            sliderInput("art_start_time", label = "ART Start Time (Days)", min = 0, max = 365, value = 5),
                            numericInput("art_number", label = "Number of people put on ART", min=0, value=50),
                            numericInput("art_rate", label = "Number of people put on ART per Day", min = 0, value = 2)
                          ),
                          # END ART CONDITIONAL PANEL
                          
                          
                          
                          # PREP CONDITIONAL PANEL
                          conditionalPanel(
                            condition = "input.dropdown == 3",
                            h4(paste0("PREP Intevention Module 0")),
                            numericInput("prep_number", label = "Number of people put on PREP", min = 0, value = 10),
                            sliderInput("prep_start_time", label = "PREP Start Time (Days)", min=0, max=365, value=181),
                            numericInput("prep_rate", label = "Number of people put on PREP per Day", min=0,value=2)
                          ),
                          # END PREP CONDITIONAL PANEL
                    
                          
                          tags$div(id = 'placeholder'), 
                          
                          # MORE CONDITIONAL PANELS

                          
                          # OMG
                          br(),
                          br(),
                          actionButton("addBtn", "Add Intervention")
                          #actionButton("removeBtn", "Remove Intervention")
                          #actionButton("plotBtn", "Update Simulation", class = "btn-primary"),

                          

                          
                          
                          #actionButton("replot", "RUN INITIAL MODEL", class = "btn-primary"),
                          #bsTooltip("replot", "Click to run the simulation and plot the output.", trigger="hover")
                          
                        ), # End sidebarPanel
                        mainPanel(
                          tabsetPanel(type = 'tabs',
                                      tabPanel("Plot Visualization", 
                                               bsAlert("initAlert"),
                                               plotlyOutput("result"),
                                               plotlyOutput("result1")),
                                      tabPanel("Network Visualization",
                                               numericInput("networkTime", "Timestep", min=1, value = 1),
                                               numericInput("networkSimSelect", "Simulation summary", min = 1, value = 1)),
                                      tabPanel("Cost Effectiveness Results", 
                                               textOutput("totalCostTextInit"))
                                      
                          )
                        ) # End mainPanel
                      ) # End sidebarLayout
                      ) # End tabPanel (2)

             ) # End navbarPage
) # End shinyUi


