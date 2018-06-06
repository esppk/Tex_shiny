library(DT) 
analy_page <- function(){
    fluidPage(
        titlePanel("Text Analysis"),
        
        textAreaInput('text', label = "Text for Analysis", 
                      width = "100%", height = "200px", 
                      placeholder = "Please type in text for analysis here..."
        ),
        actionButton("ok", "Analysis", icon = icon("code")),
        uiOutput("taboutput")
    
    )
}




navbarPage("Shiny Text",
           tabPanel("Text Analysis", analy_page()),
           tabPanel("Knowledge Base", h2("Under Development...")))