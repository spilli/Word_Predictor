# ui.R

library(shiny) 

shinyUI(pageWithSidebar(
    headerPanel("Smart Keyboard - Word predictor"), 
    sidebarPanel(
        textInput(inputId = "textIn", label="Enter text below"),
        h6('Eg., '),
        h6('Wish you a Happy birthday'),
        h6('05 May is cinco de mayo'),
        h6('This is United States of America'),
        tags$br(),tags$br(),tags$br(),
        p('Author : Sridhar Pilli'),
        p('Email  : spilli27@gmail.com')
    ),
    mainPanel(
        h4('Summary'),
        h5('This application lets you predict the next word given a sequence of words. 
           This is done via language model built using n-grams and picking the highly probable word'),
        
        tags$br(),
        h4('Text entered'),
        htmlOutput('textIn'),
        
        tags$br(),
        h4('Predicted word sequence'),
        htmlOutput('textOut')
        ) 
))
