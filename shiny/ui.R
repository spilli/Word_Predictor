# ui.R

library(shiny) 

shinyUI(pageWithSidebar(
    headerPanel("Smart Keyboard - Word predictor"), 
    sidebarPanel(
        textInput(inputId = "textIn", label="Enter text below"),
        tags$br(),tags$br(),tags$br(),
        p('Author : Sridhar Pilli'),
        p('Email  : spilli27@gmail.com')
    ),
    mainPanel(
        h4('Summary'),
        h5('This application lets you predict the next word given a sequence of words. 
           This is done via building language model using n-grams and picking the best possible word'),
        
        tags$br(),
        h4('Text entered'),
        htmlOutput('textIn'),
        
        tags$br(),
        h4('Predicted word sequence'),
        htmlOutput('textOut')
        ) 
))
