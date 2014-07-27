
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(tm)

#load file with words
words <- read.csv('labMTwords-english.csv', header = FALSE, col.names = c('word'))

#load file with words weights
wordsWeights <- read.csv('labMTscores-english.csv', header = FALSE, col.names = c('weight'))

#normalize weights on scale 0:10 (worst word has 1.3 value, best - 8.5)
wordsWeights <- (wordsWeights - 1.3) / (8.5 - 1.3) * 10

dict <- data.frame(cbind(words, wordsWeights))

getWordMatrix <- function(text) {
    #create corpus from text
    corpus <- VCorpus(VectorSource(text))
    #remove white spaces
    corpus <- tm_map(corpus, stripWhitespace)
    #lowercase
    corpus <- tm_map(corpus, content_transformer(tolower))
    #remove stop words
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    #remove punctuaction
    corpus <- tm_map(corpus, removePunctuation)
    
    #create Document Term Matrix
    dtm <- DocumentTermMatrix(corpus, list(dictionary = as.vector(dict$word)))
    #convert to list
    wordMatrix <- t(as.matrix(dtm))[,1]
    #convert to data frame
    wordMatrix <- data.frame(cbind(names(wordMatrix), as.vector(wordMatrix)))
    
    #remove words which not present
    wordMatrix <- wordMatrix[!(apply(wordMatrix, 1, function(y) any(y == 0))),]
    names(wordMatrix) <- c('word', 'count')
    
    for (i in 1:nrow(wordMatrix)) {
        word <- wordMatrix[i,1]
        weight <- as.double(dict[dict$word == word,]$weight)
        wordMatrix[i,3] <- weight
    }
    names(wordMatrix) <- c('word', 'count', 'weight')
    wordMatrix
}

getPositivity <- function(wordMatrix) {
    totalCount <- 0
    totalWeight <- 0
    
    for (i in 1:nrow(wordMatrix)) {
        word <- wordMatrix[i,1]
        count <- as.double(wordMatrix[i,2])
        weight <- wordMatrix[i,3]
        totalWeight <- totalWeight + weight*count
        totalCount <- totalCount + count
    }
    

    totalWeight/totalCount
}

getTopPositiveWords <- function(wordMatrix) {
    wordMatrix[order(-wordMatrix$weight),][1:min(nrow(wordMatrix),5),]
}

getTopNegativeWords <- function(wordMatrix) {
    wordMatrix[order(wordMatrix$weight),][1:min(nrow(wordMatrix),5),]
}

shinyServer(function(input, output) {
    wordMatrix <- reactive({getWordMatrix(input$inputText)})
    output$positivity <- renderPrint({getPositivity(wordMatrix())})
    output$topPositive <- renderPrint({getTopPositiveWords(wordMatrix())})
    output$topNegative <- renderPrint({getTopNegativeWords(wordMatrix())})
})
