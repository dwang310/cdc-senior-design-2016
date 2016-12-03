
observeEvent(input$addBtn,{
  
  btn <- input$addBtn
  id <- paste0('dropdownM', btn)
  action.id <- paste0('actionM', btn)
  plot.id <- paste0('plotValM', btn)
  insertUI(
    selector = '#placeholder',
    #selector = '#addBtn',
    #where = "beforeBegin",
    ui = bootstrapPage(
      
      div(style = "display:inline-block", selectInput(id, label = h4(paste0("Intervention Strategy Module ", btn)), 
                                                      choices = list("Syringe Exchange Program" = 1, 
                                                                     "Anti-Retoviral Therapy" = 2, 
                                                                     "Pre-Exposure Prophylaxis" = 3, 
                                                                     "No Intervention" = 4), 
                                                      selected = 4)  ),
      div(style = "display:inline-block", actionButton(action.id, label = "Run", class = "btn-primary btn-sm")),
      tags$style(type="text/css", paste0("#",action.id ," { width:100%; margin-bottom: 25px;}")),
      
      # SEP
      conditionalPanel(
        condition = paste0("input.",id," == 1"),
        h4(paste0("SEP Intervention Module ", btn)),
        numericInput(paste0("sep_exchange_frequency",btn), label = "Average frequency people exchange needles (days)", min =0, value=2),
        sliderInput(paste0("sep_compliance",btn), label = "SEP Compliance Rate", min = 0, max = 1, value = 0.9),
        sliderInput(paste0("sep_start_time", btn), label = "SEP Start Time (Days)", min = 0, max = 365, value=50),
        numericInput(paste0("sep_enrollment", btn), label = "Number of people enrolled in SEP", min=0, value=0)
      ),
      
      # ART
      conditionalPanel(
        condition = paste0("input.",id," == 2"),
        h4(paste0("ART Intervention Module ", btn)),
        sliderInput(paste0("art_start_time", btn), label = "ART Start Time (Days)", min = 0, max = 365, value = 5),
        numericInput(paste0("art_number", btn), label = "Number of people put on ART", min=0, value=50),
        numericInput(paste0("art_rate", btn), label = "Number of people put on ART per Day", min = 0, value = 2)
      ),
      
      # PREP
      conditionalPanel(
        condition = paste0("input.",id," == 3"),
        h4(paste0("PREP Intervention Module ", btn)),
        numericInput(paste0("prep_number", btn), label = "Number of people put on PREP", min = 0, value = 10),
        sliderInput(paste0("prep_start_time", btn), label = "PREP Start Time (Days)", min=0, max=365, value=181),
        numericInput(paste0("prep_rate", btn), label = "Number of people put on PREP per Day", min=0,value=2)
      )
    )
  ) # END Insert UI
  
  numID <<- c(btn, numID)
  inserted <<- c(id, inserted)
  action.inserted <<- c(action.id, action.inserted)
  plot.inserted <<- c(plot.id, plot.inserted)
  
  
  
  print(numID)
  print(plot.inserted)
  print(paste0("first index = ", first(plot.inserted)))

})