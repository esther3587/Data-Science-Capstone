library(shiny)
library(NLP)
library(tm)
library(RWeka)
source("NLP.R")


shinyServer(
    function(input, output) {
        output$inputValue <- renderPrint({input$Tcir})
        output$prediction <- renderPrint({wordproc(input$Tcir)})
        
       
        
    }
)
