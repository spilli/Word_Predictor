ngram_tokenize <- function(data) {
    for(i in 1:length(data)) {
        temp <- data[i]
        temp <- gsub("[,.-]"," ", temp) # replace with space
        temp <- gsub("[[:punct:]]","",temp) # remove punctuation
        temp <- gsub("[0-9]","",temp) # remove numbers
        data[i] <- tolower(temp) # to lower case
    }
    data <- lapply(data, function(x) unlist(strsplit(x,split=" ")) ) # into words
    data <- lapply(data, function(x) grep("^[a-z']+$",x,value=TRUE) ) # select only english
    
    # Remove profane and remove stop words
    profanity <- readLines("bad-words.txt"); 
    data <- lapply(data, function(x) { x[!(x %in% profanity[-1])] })
}

ngram_compute <- function(data,n) {
    data <- ngram_tokenize(data) # Tokenize the data
    # Create n-grams
    if(n==2) { # Bigrams
        idx2 <- sapply(data, function(x) ifelse(length(x)>1,TRUE,FALSE)) # rows with atleast 2 words
        data <- lapply(data[idx2],function(x) { paste(x[1:(length(x)-1)],x[2:length(x)]) })
    }
    if(n==3) { # Trigrams
        idx3 <- sapply(data, function(x) ifelse(length(x)>2,TRUE,FALSE)) # rows with atleast 3 words
        data <- lapply(data[idx3],function(x) { paste(x[1:(length(x)-2)],x[2:(length(x)-1)],x[3:length(x)]) })
    }
    if(n==4){ # 4-gram
        idx4 <- sapply(data, function(x) ifelse(length(x)>3,TRUE,FALSE)) # rows with atleast 4 words
        data <- lapply(data[idx4],function(x) { 
            LX <- length(x); paste(x[1:(LX-3)],x[2:(LX-2)],x[3:(LX-1)],x[4:LX]) })
    }
    if(n==5){ # 5-gram
        idx5 <- sapply(data, function(x) ifelse(length(x)>4,TRUE,FALSE)) # rows with atleast 5 words
        data <- lapply(data[idx5],function(x) { 
            LX <- length(x); paste(x[1:(LX-4)],x[2:(LX-3)],x[3:(LX-2)],x[4:(LX-1)],x[5:LX]) })
    }
    
    # Count ngrams
    ngram <- sort(table(unlist(data)),decreasing = TRUE)
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

#------------------------ Script starts here -------------------

setwd("~/Documents/Git/Github/Word_Predictor/")
# Process each input file. 
Nlines <- 600000
d.b <- readLines("final/en_US/en_US.blogs.txt",Nlines)
d.n <- readLines("final/en_US/en_US.news.txt",Nlines)
d.t <- readLines("final/en_US/en_US.twitter.txt",Nlines)

# Split data into training, validation and test sets
set.seed(100)
idx <- sample(seq(Nlines)); N1 <- round(0.6*Nlines); N2 <- round(0.8*Nlines)
train <- c(d.b[idx[1:N1]],          d.n[idx[1:N1]],          d.t[idx[1:N1]])
valid <- c(d.b[idx[(N1+1):N2]],     d.n[idx[(N1+1):N2]],     d.t[idx[(N1+1):N2]])
test  <- c(d.b[idx[(N2+1):Nlines]], d.n[idx[(N2+1):Nlines]], d.t[idx[(N2+1):Nlines]])
rm(d.b,d.n,d.t)

#---------------------- Compute ngrams -------------------------

M <- 1000000
u.count <- ngram_compute(train[1:M],1)   # Unigram model
b.count <- ngram_compute(train[1:M],2)   # Bigram model
t.count <- ngram_compute(train[1:M],3)   # Trigram model
M <- 100000
q.count <- ngram_compute(train[1:M],4)   # Quadgram model
p.count <- ngram_compute(train[1:M],5)   # Pentagram model

x <- data.frame(c("1-grams","2-grams","3-grams","4-grams","5-grams"),
                prettyNum(c(sum(u.count),sum(b.count),sum(t.count),sum(q.count),sum(p.count)),big.mark = ","),
                prettyNum(c(length(u.count),length(b.count),length(t.count),length(q.count),length(p.count)),big.mark = ",")
)
names(x) <- c("N-gram","Number of ngrams", "Unique no. of ngrams")
library(knitr);kable(x,align="c")

#--------------- Build word prediction model ---------------------
dict <- names(u.count)
dictionary <- matrix(1:length(dict)); rownames(dictionary) <- dict

library(dplyr)

# 2-grams
PQ <- unlist(strsplit(names(b.count),split = " "))
df2 <- data.frame(w1 = PQ[seq(1,length(PQ),by=2)],
                  w2 = PQ[seq(2,length(PQ),by=2)], c=b.count, row.names = NULL)
df2 <- df2 %>% group_by(w1) %>% arrange(desc(c)) %>% slice(1) %>% as.data.frame()
df2$w1 <- as.character(df2$w1)
df2$w2 <- as.character(df2$w2)
df2$c <- as.integer(df2$c)
df2 <- arrange(df2,desc(c))
m2 <- cbind(dictionary[df2$w1,1],dictionary[df2$w2,1])
rownames(m2) <- NULL
rm(PQ,df2)

# 3-grams
PQR <- unlist(strsplit(names(t.count),split = " "))
df3 <- data.frame(w1 = PQR[seq(1,length(PQR),by=3)],
                  w2 = PQR[seq(2,length(PQR),by=3)],
                  w3 = PQR[seq(3,length(PQR),by=3)], c=t.count, row.names = NULL)
df3 <- df3 %>% group_by(w1) %>% arrange(desc(c)) %>% slice(1) %>% as.data.frame()
df3$w1 <- as.character(df3$w1)
df3$w2 <- as.character(df3$w2)
df3$w3 <- as.character(df3$w3)
df3$c <- as.integer(df3$c)
df3 <- arrange(df3,desc(c))
m3 <- cbind(dictionary[df3$w1,1],dictionary[df3$w2,1],dictionary[df3$w3,1])
rownames(m3) <- NULL
rm(PQR,df3)

# 4-grams
PQRS <- unlist(strsplit(names(q.count),split = " "))
df4 <- data.frame(w1 = PQRS[seq(1,length(PQRS),by=4)],
                  w2 = PQRS[seq(2,length(PQRS),by=4)],
                  w3 = PQRS[seq(3,length(PQRS),by=4)],
                  w4 = PQRS[seq(4,length(PQRS),by=4)], c=q.count, row.names = NULL)
df4 <- df4 %>% group_by(w1) %>% arrange(desc(c)) %>% slice(1) %>% as.data.frame()
df4$w1 <- as.character(df4$w1)
df4$w2 <- as.character(df4$w2)
df4$w3 <- as.character(df4$w3)
df4$w4 <- as.character(df4$w4)
df4$c <- as.integer(df4$c)
df4 <- arrange(df4,desc(c))
m4 <- cbind(dictionary[df4$w1,1],dictionary[df4$w2,1],dictionary[df4$w3,1],dictionary[df4$w4,1])
rownames(m4) <- NULL
rm(PQRS,df4)

# 5-grams
PQRST <- unlist(strsplit(names(p.count),split = " "))
df5 <- data.frame(w1 = PQRST[seq(1,length(PQRST),by=5)], 
                  w2 = PQRST[seq(2,length(PQRST),by=5)], 
                  w3 = PQRST[seq(3,length(PQRST),by=5)],
                  w4 = PQRST[seq(4,length(PQRST),by=5)],
                  w5 = PQRST[seq(5,length(PQRST),by=5)], c=p.count, row.names = NULL)
df5 <- df5 %>% group_by(w1) %>% arrange(desc(c)) %>% slice(1) %>% as.data.frame()
df5$w1 <- as.character(df5$w1)
df5$w2 <- as.character(df5$w2)
df5$w3 <- as.character(df5$w3)
df5$w4 <- as.character(df5$w4)
df5$w5 <- as.character(df5$w5)
df5$c <- as.integer(df5$c)
df5 <- arrange(df5,desc(c))
m5 <- cbind(dictionary[df5$w1,1],dictionary[df5$w2,1],dictionary[df5$w3,1],dictionary[df5$w4,1],dictionary[df5$w5,1])
rownames(m5) <- NULL
rm(PQRST,df5)

save(dict,m2,m3,m4,m5,file="Model.RData")
save(u.count,b.count,t.count,q.count,p.count,m2,m3,m4,m5,file="Model_All.RData")


# ----------- Test the model -----------

print(ngram_predict("thank you"))
print(ngram_predict("i would like"))
print(ngram_predict("couple of weeks"))
print(ngram_predict("being number one in"))
print(ngram_predict("despite the fact"))
print(ngram_predict("being number one in the"))

print(ngram_predict("cant wait to have"))
print(ngram_predict("united states of"))
print(ngram_predict("cinco de"))
print(ngram_predict("president barack"))
print(ngram_predict("Wish you a Happy Mother's"))
