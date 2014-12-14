# ui.R
#wordcloud(N1T[["w1"]], N1T[["freq"]], min.freq=500000, colors=brewer.pal(6, "Dark2"))
shinyUI(fluidPage(
  titlePanel("Basic Word Prediction Based on the Previously Known Three Words"),
  sidebarLayout(
    sidebarPanel( 
      textInput("text",
                label = h3("Type text here:", style = "color: red"), 
                value = "i love"),
      hr(),
      p(em("This might take a few seconds to load initially.", style = "font-size: 120%")),
      br(),
      strong(h3("See the figure and table of the most probable, subsequent predicted words on the right.", style = "color:green")),
      strong(h3("In the table search box, discover the associated frequency and probability of less or more frequent words.", style = "color:green")),
      br(),
      img(src = "N1TwordCloud_30000combo.png", height="100%", width="100%"), 
      #img(src = "N1TwordCloud_30000combo.png", height = 300, width = 300),
      p(em("The figure above is a word cloud created from a 63,871 word corpus used to train the word prediction algorithm used here. The word cloud represents words with a frequency greater than 30,000 (maximum frequency is 5,422,868 for the word 'the').", style = "font-size: 120%")),
      em(HTML("The predictions are based on a simple quadgram backoff modeling approach, trained on quadgrams derived from the following two sources: (1) ~5.5 million quadgrams were derived from a corpus consisting of blogs, news, and twitter feeds found at the <a href='www.corpora.heliohost.org/'> HC Corpus website </a> and (2) ~1 million of the most frequent quadgrams were derived from the <a href='http://www.ngrams.info/'> Corpus of Contemporary English (COCA) </a> by Mark Davies (2011)."), style = "font-size: 120%")
  ),
    mainPanel(
      h3("Top Predicted Words by Probability", style = "color:blue", align="center"),
      plotOutput("barPlot"),
      h3("Predicted terms, frequencies and probabilities:",style = "color:blue", align="center"),
      #dataTableOutput("predtext")
      div(dataTableOutput("predtext"), style = "font-size:120%")
  )
)
))