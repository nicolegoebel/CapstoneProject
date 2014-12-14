# server.R
# runApp("shiny_NLP", display.mode = "showcase")
# shiny::runApp('shiny_NLP')
library(shiny)
library(data.table)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"rmarkdown"), "data.table")
library(ggplot2)
#library(stringi)
options(shiny.maxRequestSize = 512000000)
load(file="ngrams_4_3_2_1_COCA_orig_combo.RData")

backoffCondition <- function(term) {
  terms <- tolower(term)
  terms <- strsplit(terms, " ")
  n<-length(terms[[1]])
  if (n>3) {
    terms<-strsplit(paste(terms[[1]][n-2], terms[[1]][n-1], terms[[1]][n]), " ")
    n<-length(terms[[1]])
  }  
  tmpList<-NaN
  if (n==3) {
    tmpList<-N4T[list(terms[[1]][1], terms[[1]][2], terms[[1]][3])]
    if (all(is.na(tmpList[,freq]))) {
      tmpList<-N3T[list(terms[[1]][2], terms[[1]][3])]
    } else if (all(is.na(tmpList[,freq]))) {
      tmpList<-N2T[list(terms[[1]][3])]
    } 
  } else if (n==2) {
    tmpList<-N3T[list(terms[[1]][1], terms[[1]][2])]
    if (all(is.na(tmpList[,freq]))) {
      tmpList<-N2T[list(terms[[1]][2])]
    } 
  } else if (n==1) {
    tmpList<-N2T[list(terms[[1]][1])]
    if (all(is.na(tmpList[,freq]))) { tmpList <- NA}
  } #else {tmpList <- NA}
  if (all(is.na(tmpList))) {
    return(as.data.table(tmpList))
  } else {
    tmpList[,Pr:=tmpList[,freq]/tmpList[,sum(freq)]] #Pr calc
    tmpList<-as.data.table(tmpList[order(-rank(freq))])
    if (dim(tmpList)[2]==7) {setcolorder(tmpList, c("w1", "w2", "w3", "w4", "w5", "freq", "Pr"))
                            setnames(tmpList, c("word 1", "word 2", "word3", "word 4", "word 5", "prediction", "frequency", "probability"))
    } else if (dim(tmpList)[2]==6) {setcolorder(tmpList, c("w1", "w2", "w3", "w4","freq", "Pr"))
                            setnames(tmpList, c("word 1", "word 2", "word3", "prediction", "frequency", "probability"))
    } else if (dim(tmpList)[2]==5) {setcolorder(tmpList, c("w1", "w2", "w3", "freq", "Pr"))
                            setnames(tmpList, c("word 1", "word 2",  "prediction", "frequency", "probability"))
    } else if (dim(tmpList)[2]==4) {setcolorder(tmpList, c("w1", "w2", "freq", "Pr"))
                            setnames(tmpList, c("word 1", "prediction", "frequency", "probability"))
    } else if (dim(tmpList)[2]==3) {setcolorder(tmpList, c("w1","freq", "Pr"))
                            setnames(tmpList, c("word 1", "prediction", "frequency", "probability"))
    } else if (dim(tmpList)[2]==2) {setcolorder(tmpList, c( "freq", "Pr"))
                            setnames(tmpList, c("prediction", "frequency", "probability"))
    }
  }
  return(tmpList)
}

shinyServer(
  function(input, output) {
    # get inputted data from ui.r
    textInput <- reactive({
      if (!is.null(input$text) && input$text != "") {
        t <- backoffCondition(input$text)
      }
    })
    
    # You can access the value of the widget with input$text, e.g.
    #output$value <- renderPrint({ input$text })
    output$predtext <- renderDataTable({ 
      textInput()
    })
    
    output$barPlot <- renderPlot({
      #wfo<-wf[order(-wf$freq),]  #order by descending frequencies
      wftmp <- data.table(textInput()) #output$predtext() 
      setkeyv(wftmp,  c('probability','prediction'))
      wfo <- wftmp[,.(prediction, probability)]
      wfo <- wfo[order(-wfo$probability)]
      wfo$prediction <- factor(wfo$prediction, levels = wfo$prediction[order(-wfo$probability)])
      ggplot(wfo[1:20,], aes(prediction, probability)) + geom_bar(stat="identity") + 
        xlab('Predicted Word') + ylab('Probability') + 
        theme(axis.text.x=element_text(angle=45, hjust=1, size=16),
              axis.text=element_text(size=18)) + scale_fill_brewer(palette="Spectral") #scale_fill_hue()
      #aes(fill="red"), 
    })
  }
)