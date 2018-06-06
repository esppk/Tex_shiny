library(shiny)
library(udpipe)
library(tidytext)
library(dplyr)
library(ggplot2)

server <- function(input, output){
    en_mod <- udpipe_load_model(file = "D:/Program Files/RStudio/english-ud-2.0-170801.udpipe")
    
    res <- reactive({
        
        req(input$text)
        x <- udpipe_annotate(en_mod, x = input$text)
        
        x <- as.data.frame(x) 
        n_para <- length(unique(x$paragraph_id))
        x <- x %>% select(-ends_with("id")) %>% 
            select(-.data$deps, -.data$dep_rel, -.data$misc)
        
        n_sent <- length(unique(x$sentence))
        
        n_word <- length(unique(x$token))
        list(x, n_sent, n_para, n_word)
    })
    
    observeEvent(input$ok,{
        output$taboutput <- renderUI({
            
            tabsetPanel(
                tabPanel("Part of Speech", 
                         
                         textOutput("textSum"),
                         shiny::br(),
                         DT::dataTableOutput('result') 
                         
                ), 
                tabPanel("sentiment", plotOutput("sentiplot"))
            )})
    })
    
    


    
    output$textSum <- renderText({
        input$ok
        isolate(paste0("Your text has ",res()[[3]], " paragraphs, ", res()[[2]], " sentences",
                       ", and ", res()[[4]], " unique words."))})
    
    
    output$result <- DT::renderDataTable({
        input$ok
        isolate( res()[[1]] )}, options = list(pageLength = 6))

    
    
    output$sentiplot <- renderPlot({
        
        withProgress(
            input$text %>% tibble(txt = .) %>% unnest_tokens(word, txt) %>%
                count(word, sort = T) %>% 
                inner_join(get_sentiments("nrc")) %>% 
                group_by(sentiment) %>%  slice(1:20) %>% 
                ungroup() %>% 
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n, fill = sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y") +
                labs(y = "Contribution to sentiment",
                     x = NULL) +
                coord_flip() + labs(caption = "Sentiment info come from NRC") + 
                theme(plot.caption = element_text(face = "italic", color = "gray70")),
            message = "Need some time..."
        )
        
        
        
       
    })
}