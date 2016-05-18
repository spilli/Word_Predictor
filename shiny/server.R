# server.r

library(shiny) 
load("Model.RData")

ngram_tokenize <- function(data) {
    for(i in 1:length(data)) {
        temp <- data[i]
        temp <- gsub("[,.-]"," ", temp) # replace . - with space
        temp <- gsub("[[:punct:]]","",temp) # remove punctuation
        temp <- gsub("[0-9]","",temp) # remove numbers
        data[i] <- tolower(temp) # to lower case
    }
    data <- lapply(data, function(x) unlist(strsplit(x,split=" ")) ) # into words
    data <- lapply(data, function(x) grep("^[a-z]+$",x,value=TRUE) ) # select only english
    
    # Remove profane and remove stop words
    profanity <- readLines("bad-words.txt"); 
    remWords <- profanity[-1]
    data <- lapply(data, function(x) { x[!(x %in% remWords)] })
}

pred2 <- function(P) {
    iP <- which(dict==P)
    pred <- dict[m2[ (m2[,1]==iP) , 2]]
}
pred3 <- function(P,Q) {
    iP <- which(dict==P); iQ <- which(dict==Q)  
    pred <- dict[m3[ (m3[,1]==iP)&(m3[,2]==iQ) , 3]]
}
pred4 <- function(P,Q,R) {
    iP <- which(dict==P); iQ <- which(dict==Q); iR <- which(dict==R) 
    pred <- dict[m4[ (m4[,1]==iP)&(m4[,2]==iQ)&(m4[,3]==iR) , 4]]
}
pred5 <- function(P,Q,R,S) {
    iP <- which(dict==P); iQ <- which(dict==Q); iR <- which(dict==R); iS <- which(dict==S)
    pred <- dict[m5[ (m5[,1]==iP)&(m5[,2]==iQ)&(m5[,3]==iR)&(m5[,4]==iS) , 5]]
}
ngram_predict <- function(data) {
    D <- ngram_tokenize(data) # Tokenize
    D <- unlist(D)
    L <- length(D)
    
    if (L==0) { pred_val <- dict[1]}
    if (L==1) { 
        P <- D[L]; pred_val <- pred2(P)
    }
    if (L==2) { 
        P <- D[L-1]; Q <- D[L]; pred_val <- pred3(P,Q)
        pred_val <- if(length(pred_val)==1) { pred_val } else { pred2(Q) }
    }
    if (L==3) { 
        P <- D[L-2]; Q <- D[L-1]; R <- D[L]; pred_val <- pred4(P,Q,R)
        pred_val <- if(length(pred_val)==1) { pred_val } else { pred3(Q,R) }
        pred_val <- if(length(pred_val)==1) { pred_val } else { pred2(R) }
    }
    if (L>=4) { 
        P <- D[L-3]; Q <- D[L-2]; R <- D[L-1]; S <- D[L]; pred_val <- pred5(P,Q,R,S)
        pred_val <- if(length(pred_val)==1) { pred_val } else { pred4(Q,R,S) }
        pred_val <- if(length(pred_val)==1) { pred_val } else { pred3(R,S) }
        pred_val <- if(length(pred_val)==1) { pred_val } else { pred2(S) }
    }
    pred_val <- if(length(pred_val)==1) { pred_val } else { "UNK" }
}


shinyServer(
    function(input, output) {
        
        output$textIn  <- renderText({ 
            paste("<font color=\"blue\">", input$textIn, "</font>") })
        pred <- reactive({ ngram_predict(input$textIn) })
        output$textOut <- renderText({ 
                paste("<font color=\"blue\">", input$textIn, "</font>",
                      "<font color=\"green\">","<b><u>", pred() , "</u></b>", "</font>")
        })
        
    } 
)