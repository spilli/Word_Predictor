# server.r

library(shiny) 
load("Model.RData")
ngram_tokenize <- function(data) {
    for(i in 1:length(data)) {
        temp <- data[i]
        temp <- gsub("[.-]"," ", temp) # replace . - with space
        temp <- gsub("[[:punct:]]","",temp) # remove punctuation
        temp <- gsub("[0-9]","",temp) # remove numbers
        data[i] <- tolower(temp) # to lower case
    }
    data <- lapply(data, function(x) unlist(strsplit(x,split=" ")) ) # into words
    data <- lapply(data, function(x) grep("^[a-z]+$",x,value=TRUE) ) # select only english
    
    # Remove profane and remove stop words
    profanity <- readLines("bad-words.txt"); 
    stopWords <- readLines("stop-words.txt"); stopWords <- gsub("[[:punct:]]","",stopWords)
    remWords <- c(profanity[-1],stopWords)
    data <- lapply(data, function(x) { x[!(x %in% remWords)] })
}
ngram_predict <- function(data) {
    #load("Model.RData")
    
    D <- ngram_tokenize(data) # Tokenize
    D <- unlist(D); D[!(D %in% rownames(u.model))] <- "UNK" # mark the ones not in dictionary as UNK
    L <- length(D)
    
    if(L==0) { P <- "UNK"; Q <- "UNK"}
    if(L==1) { P <- "UNK"; Q <- D[L] }
    if(L>1) { P <- D[L-1]; Q <- D[L] }
    R <- rownames(u.model); R <- R[-length(R)] # Map against dictionary
    
    PQR <- paste(P,Q,R)
    id1 <- (PQR %in% rownames(t.model))
    PQ <- paste(P,Q)
    QR <- paste(Q,R)
    id2 <- (!id1) & (PQ %in% rownames(b.model))    &   (QR %in% rownames(b.model))
    id3 <- (!id1) & (PQ %in% rownames(b.model))    & (!(QR %in% rownames(b.model)))
    id4 <- (!id1) & (!(PQ %in% rownames(b.model))) &   (QR %in% rownames(b.model))
    id5 <- (!id1) & (!(PQ %in% rownames(b.model))) & (!(QR %in% rownames(b.model)))
    
    a <- matrix(NA,length(PQR),1)
    if(sum(id1)>0) { a[id1] <- t.model[PQR[id1],2] }
    if(sum(id2)>0) { a[id2] <- b.model[PQ,2] + b.model[QR[id2],2] + log(2*0.4) }
    if(sum(id3)>0) { a[id3] <- b.model[PQ,2] + u.model[Q,2] + u.model[R[id3],2] + log(3*0.4) }
    if(sum(id4)>0) { a[id4] <- u.model[P,2] + u.model[Q,2] + b.model[QR[id4],2] + log(3*0.4) }
    if(sum(id5)>0) { a[id5] <- u.model[P,2] + u.model[Q,2] + u.model[R[id5],2] + log(4*0.4) }
    
    pred_word <- rownames(u.model)[which.max(a)]
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