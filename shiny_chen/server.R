
#####################################################
#       Make sure to source dependencies first!     #
#                        DW                         #
#####################################################

# Utility functions
first <- function(x) { head(x, n = 1) }
disableActionButton <- function(id,session) {
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
}


source("init/init_libraries.R", local = TRUE)
# http://www.htmlwidgets.org/showcase_datatables.html
# http://kateto.net/network-visualization
numID <- NULL
inserted <- NULL
action.inserted <- NULL
plot.inserted <- NULL

shinyServer(function(input, output, session) {

  
        session$onSessionEnded(stopApp)
        init_var <- reactive({input$initstart})

        
        # Dynamic UI Code Logic
        source("serverscripts/insertbtn.R", local = TRUE)
        source("serverscripts/plotvalinit.R", local = TRUE)
        source("serverscripts/plotlyinit.R", local = TRUE)
        source("serverscripts/plotvalmult.R", local = TRUE)
        source("serverscripts/plotlymult.R", local = TRUE)
        source("serverscripts/totalcosts.R", local = TRUE)
        source("serverscripts/plotlycosteff.R", local = TRUE)
        source("serverscripts/disablebtn.R", local = TRUE)
        source("serverscripts/simStats.R", local = TRUE)

        # User Greeting Logic
        output$textIntro <- renderText({
          
          if(input$radchoice==1){
            user.name = "Regular User"
          } else { user.name = "Advanced User"}
          
         paste("Welcome,", user.name)
        })
        
        #outputOptions(output, 'initUpload', suspendWhenHidden=FALSE)
            
        


        
        
        
        # EXAMPLE ALERT
        #createAlert(session, "initAlert", title = "Oops!", content = "TEST ALERT")
        
        
})

## Important links
