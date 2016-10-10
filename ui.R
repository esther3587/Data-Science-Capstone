library(shiny)

fluidPage(
    
    
    mainPanel(
        h3("Introduction:"),
        h5("This application takes your string and predict the next world"),
        h3("Method:"),
        h5("Use MLE of n-gram algorithm"),
        
        textInput("Tcir",label=h3("Type your sentence here:")),
        submitButton('Submit'),
        h4('string you entered : '),
        verbatimTextOutput("inputValue"),
        h4('next word :'),
        verbatimTextOutput("prediction")
        
    )
)
