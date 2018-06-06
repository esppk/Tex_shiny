library(DT) 
analy_page <- function(){
    fluidPage(
        titlePanel("Text Analysis"),
        
        tags$div(class = "main_section",
                 
                 tags$div(class = "content_sec",
                          
                          textAreaInput('text', label = "", 
                                        width = "100%", height = "200px", 
                                        placeholder = "Please type in text for analysis here..."
                          ),
                          actionButton("ok", "Analysis", icon = icon("code")),
                          uiOutput("taboutput")
                          
                          
                          )
                 

                 
                 )
        

    
    )
}




navbarPage("Shiny Text",
           tabPanel("Text Analysis", analy_page()),
           tabPanel("Knowledge Base", h2("Under Development...")),
           
           theme = "style.css",inverse = TRUE)