rm(list=ls()) 
library(data.table)
library(tm)
#library(ggplot2)
#require(RWeka)
options(mc.cores=1)
options( java.parameters = "-Xmx3g" )
library(RWeka)
#library(wordcloud)
#library(Rgraphviz)
library(knitr)
library(SnowballC)
library(stringi)
library(stylo)

## 1. download zip file
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##   a. set working directory
dirName='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/'
dirNameFull='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/'
dirNameFullTrain='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/trainSample/'
dirNameFullTest='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/testSample/'
setwd(dirNameFull)
# attempt to clean up txt file first, so that can count word frequencies
# and replace least frequent words with UNK

## 2. Break up larger texf files into smaller text files
## break into 6000 line files:  csplit -f "nw" -n 3 -k en_US.news.txt 6000 '{6000}'
#
## 3. process subsampled text files one by one....
if (file.exists("comboN4gramsOnly.RData")){
  load("comboN4gramsOnly.Rdata"){
  } else {
code="tw"
for (i in 1:393){
  filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
  inputfile <- paste(dirNameFull,code,"splits/", filename,sep="")
  Lines <- readLines(inputfile, skipNul = TRUE)
  Lines = gsub ("[[:digit:]]", "", Lines)
  Lines = gsub ("[[:punct:]]", "", Lines)
  Lines = gsub ("[[:cntrl:]]", "", Lines)
  Lines = gsub ("[^[:alnum:][:space:]']", " ", Lines)
  Lines = gsub ("[^[:alnum:]// ]", " ", Lines)
  Lines = tolower (Lines)
  size4 <- unlist(lapply(Lines[grep("[^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]*", Lines)], function(Lines) make.ngrams(txt.to.words(Lines, splitting.rule = "[ \t\n]+"), ngram.size = 4)))
  outputfile <- paste("n4grams/", filename,sep="")
  write.table(size4, outputfile , quote=FALSE, row.names = FALSE, col.names = FALSE)
  rm(size4)
  rm(Lines)
}
code="nw"
for (i in 1:168){
  filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
  inputfile <- paste(dirNameFull,code,"splits/", filename,sep="")
  Lines <- readLines(inputfile, skipNul = TRUE)
  Lines = gsub ("[[:digit:]]", "", Lines)
  Lines = gsub ("[[:punct:]]", "", Lines)
  Lines = gsub ("[[:cntrl:]]", "", Lines)
  Lines = gsub ("[^[:alnum:][:space:]']", " ", Lines)
  Lines = gsub ("[^[:alnum:]// ]", " ", Lines)
  Lines = tolower (Lines)
  size4 <- unlist(lapply(Lines[grep("[^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]*", Lines)], function(Lines) make.ngrams(txt.to.words(Lines, splitting.rule = "[ \t\n]+"), ngram.size = 4)))
  outputfile <- paste("n4grams/", filename,sep="")
  write.table(size4, outputfile , quote=FALSE, row.names = FALSE, col.names = FALSE)
  rm(size4)
  rm(Lines)
}
code="bl"
for (i in 1:150){
  filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
  inputfile <- paste(dirNameFull,code,"splits/", filename,sep="")
  Lines <- readLines(inputfile, skipNul = TRUE)
  Lines = gsub ("[[:digit:]]", "", Lines)
  Lines = gsub ("[[:punct:]]", "", Lines)
  Lines = gsub ("[[:cntrl:]]", "", Lines)
  Lines = gsub ("[^[:alnum:][:space:]']", " ", Lines)
  Lines = gsub ("[^[:alnum:]// ]", " ", Lines)
  Lines = tolower (Lines)
  size4 <- unlist(lapply(Lines[grep("[^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]*", Lines)], function(Lines) make.ngrams(txt.to.words(Lines, splitting.rule = "[ \t\n]+"), ngram.size = 4)))
  outputfile <- paste("n4grams/", filename,sep="")
  write.table(size4, outputfile , quote=FALSE, row.names = FALSE, col.names = FALSE)
  rm(size4)
  rm(Lines)
}
#increments <- 6000
#for (i in 1:increments){
#  i
#  for(code in c("tw","nw","bl")){
#    code
#    filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
#    inputfile <- paste(dirNameFull,code,"splits/", filename,sep="")
#    Lines <- readLines(inputfile, skipNul = TRUE)
#    Lines = gsub ("[[:digit:]]", "", Lines)
#    Lines = gsub ("[[:punct:]]", "", Lines)
#    Lines = gsub ("[[:cntrl:]]", "", Lines)
#    Lines = tolower (Lines)
#    #...do stuff to the read in files
#    #maybe some cleanup of the data in between depending on what you wanted to keep or get rid of
#    size4 <- unlist(lapply(Lines[grep("[^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]*", Lines)], function(Lines) make.ngrams(txt.to.words(Lines, splitting.rule = "[ \t\n]+"), ngram.size = 4)))
#    outputfile <- paste("n4grams/", filename,sep="")
#    write.table(size4, outputfile , quote=FALSE, row.names = FALSE, col.names = FALSE)
#    rm(size4)
#    rm(Lines)
#  }
#}
#browser()
## 4. concatenate 4 grams and make a table...
code="tw"
filename <- paste(code,formatC(1, width = 3, format = "d", flag = "0"),sep="")
inputfile <- paste(dirNameFull,"n4grams/", filename,sep="")
tmp<- (readLines(inputfile, skipNul = TRUE))
comboLines <- as.data.table(table(tmp))
setnames(comboLines, 'N', 'freq')
comboLines[,tmp:=strsplit(tmp, '\\s')]
comboLines[, `:=`(w1=sapply(tmp, function(s) s[1]),
           w2=sapply(tmp, function(s) s[2]),
           w3=sapply(tmp, function(s) s[3]),
           w4=sapply(tmp, function(s) s[4]),
           tmp=NULL)]
setkeyv(comboLines, c('w1', 'w2', 'w3','freq'))
rm(tmp)
for (i in 2:393){
    filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
    inputfile <- paste(dirNameFull,"n4grams/", filename,sep="")
    tmp<- readLines(inputfile, skipNul = TRUE)
    newLines <- as.data.table(table(tmp))
    setnames(newLines, 'N', 'freq')
    newLines[,tmp:=strsplit(tmp, '\\s')]
    newLines[, `:=`(w1=sapply(tmp, function(s) s[1]),
                    w2=sapply(tmp, function(s) s[2]),
                    w3=sapply(tmp, function(s) s[3]),
                    w4=sapply(tmp, function(s) s[4]),
                    tmp=NULL)]
    setkeyv(newLines, c('w1', 'w2', 'w3','freq'))
    comboLines <- rbind(comboLines,newLines)
    comboLines <- comboLines[, sum(freq), by=c('w1', 'w2', 'w3', 'w4')]
    setnames(comboLines, c('V1'), c('freq'))
    setcolorder(comboLines, c('freq','w1','w2', 'w3','w4'))
    setkeyv(comboLines, c('w1','w2', 'w3','freq'))  
    rm(tmp)
}
code="bl"
for (i in 1:150){
  filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
  inputfile <- paste(dirNameFull,"n4grams/", filename,sep="")
  tmp<- readLines(inputfile, skipNul = TRUE)
  newLines <- as.data.table(table(tmp))
  setnames(newLines, 'N', 'freq')
  newLines[,tmp:=strsplit(tmp, '\\s')]
  newLines[, `:=`(w1=sapply(tmp, function(s) s[1]),
                  w2=sapply(tmp, function(s) s[2]),
                  w3=sapply(tmp, function(s) s[3]),
                  w4=sapply(tmp, function(s) s[4]),
                  tmp=NULL)]
  setkeyv(newLines, c('w1', 'w2', 'w3','freq'))
  comboLines <- rbind(comboLines,newLines)
  comboLines <- comboLines[, sum(freq), by=c('w1', 'w2', 'w3', 'w4')]
  setnames(comboLines, c('V1'), c('freq'))
  setcolorder(comboLines, c('freq','w1','w2', 'w3','w4'))
  setkeyv(comboLines, c('w1','w2', 'w3','freq'))  
  rm(tmp)
}
code="nw"
for (i in 1:168){
  filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
  inputfile <- paste(dirNameFull,"n4grams/", filename,sep="")
  tmp<- readLines(inputfile, skipNul = TRUE)
  newLines <- as.data.table(table(tmp))
  setnames(newLines, 'N', 'freq')
  newLines[,tmp:=strsplit(tmp, '\\s')]
  newLines[, `:=`(w1=sapply(tmp, function(s) s[1]),
                  w2=sapply(tmp, function(s) s[2]),
                  w3=sapply(tmp, function(s) s[3]),
                  w4=sapply(tmp, function(s) s[4]),
                  tmp=NULL)]
  setkeyv(newLines, c('w1', 'w2', 'w3','freq'))
  comboLines <- rbind(comboLines,newLines)
  comboLines <- comboLines[, sum(freq), by=c('w1', 'w2', 'w3', 'w4')]
  setnames(comboLines, c('V1'), c('freq'))
  setcolorder(comboLines, c('freq','w1','w2', 'w3','w4'))
  setkeyv(comboLines, c('w1','w2', 'w3','freq'))  
  rm(tmp)
}
print("finished combining n4grams")
save(comboLines, file="combinedn4grams.RData")
browser()
} #end of ifelse load
  
## 5. now get rid of least frequent ngrams
comboLines1 <- comboLines[!(freq == 1), ]
save(comboLines1, file="comboN4grams1.Rdata")
comboLines2 <- comboLines1[!(freq == 2), ]
save(comboLines2, file="comboN4grams2.Rdata")

## Previous code that attempted to use "tm" and "Rweka"
#if (file.exists("corpusTrain01percentBlogs.RData")){
#  load("corpusTrain01percentBlogs.RData")
#} else {
#  dirNameTrain='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/trainSample30percent/'
#  files <- DirSource(directory=dirNameTrain, encoding='latin1')
#  corpus <- VCorpus(x=files)
#  #save(corpus, file="corpusTrain01percentBlogs.RData")
##}
## Transforms
#toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
##corpus <- Corpus(VectorSource(Rawtextn)); 
#corpus <- tm_map(corpus, toSpace, "[^0-9A-Za-z// ]") #removes special characters  
#corpus <- tm_map(corpus, toSpace, "/|@|\\|")
#badWords <- c("acrotomophilia", "anal", "anilingus", "anus", "arsehole", "ass", "asses", "asshole", "assholes", "assmunch", "auto erotic", "autoerotic", "babeland", "baby batter", "ball gag", "ball gravy", "ball kicking", "ball licking", "ball sack", "ball sucking", "bangbros", "bareback", "barely legal", "barenaked", "bastardo", "bastinado", "bbw", "bdsm", "beaver cleaver", "beaver lips", "bestiality", "bi curious", "big black", "big breasts", "big knockers", "big tits", "bimbos", "birdlock", "bitch", "bitches", "black cock", "blonde action", "blonde on blonde action", "blow j", "blow your l", "blue waffle", "blumpkin", "bollocks", "bondage", "boner", "boob", "boobs", "booty call", "brown showers", "brunette action", "bukkake", "bulldyke", "bullet vibe", "bung hole", "bunghole", "busty", "butt", "butts", "buttcheeks", "butthole", "camel toe", "cameltoe", "cameltoes", "camgirl", "camslut", "camwhore", "carpet muncher", "carpetmuncher", "chocolate rosebuds", "circlejerk", "cleveland steamer", "clit", "clitoris", "clover clamps", "clusterfuck", "cock", "cocks", "coprolagnia", "coprophilia", "cornhole", "cum", "cumming", "cunnilingus", "cunt", "cunts", "darkie", "date rape", "daterape", "deep throat", "deepthroat", "dick", "dicks", "dildo", "dirty pillows", "dirty sanchez", "doggie style", "doggiestyle", "doggy style", "doggystyle", "dog style", "dolcett", "dominatrix", "dommes", "donkey punch", "double dong", "double penetration", "dp action", "eat my ass", "ecchi", "ejaculation", "erotic", "erotism", "escort", "ethical slut", "eunuch", "faggot", "fecal", "felch", "fellatio", "feltch", "female squirting", "femdom", "figging", "fingering", "fisting", "foot fetish", "footjob", "frotting", "fuck", "fuck off", "fuckit", "fucked", "fucker", "fuckers", "fuckin", "fucking", "fuckyou", "futanari", "gang bang", "gay sex", "genitals", "giant cock", "girl on", "girls gone wild", "gokkun", "goodpoop", "goo girl", "grope", "group sex", "g-spot", "guro", "hand job", "handjob", "hard core", "hardcore", "hentai", "hooker", "hot chick", "huge fat", "humping", "incest", "intercourse", "jack off", "jail bait", "jailbait", "jerk off", "jigaboo", "jiggaboo", "jiggerboo", "jizz", "juggs", "kike", "kinbaku", "kinkster", "kinky", "knobbing", "leather restraint", "leather straight jacket", "lemon party", "lolita", "lovemaking", "make me come", "male squirting", "masturbate", "menage a trois", "milf", "missionary position", "motherfucker", "motherfuckers", "mound of venus", "mr hands", "muff diver", "muffdiving", "nambla", "nawashi", "negro", "neonazi", "nigga", "niggas", "nigger", "niggers", "nig nog", "nimphomania", "nipple", "nipples", "nsfw images", "nude", "nudity", "nympho", "nymphomania", "octopussy", "omorashi", "orgasm", "orgy", "orgies", "paedophile", "panties", "panty", "pedobear", "pedophile", "pegging", "penis", "phone sex", "piece of shit", "pissing", "piss pig", "pisspig", "playboy", "pleasure chest", "pole smoker", "ponyplay", "poof", "poopchute", "porn", "porno", "pornography", "pthc", "pubes", "pussy", "queaf", "raghead", "rape", "raping", "rapist", "rectum", "rimjob", "rimming", "rosy palm", "rusty trombone", "sadism", "scat", "schlong", "scissoring", "semen", "sex", "sexo", "sexy", "shaved beaver", "shaved pussy", "shemale", "shibari", "shit", "shota", "shrimping", "slanteye", "slut", "sluts", "s&m", "smut", "snatch", "sodomize", "sodomy", "spic", "spooge", "spread legs", "strap on", "strapon", "strip club", "style doggy", "suck", "sucks", "suicide girls", "sultry women", "swastika", "swinger", "tea bagging", "threesome", "throating", "tied up", "tight white", "tit", "tits", "titties", "titty", "topless", "tosser", "tranny", "tribadism", "tub girl", "tubgirl", "tushy", "twat", "twink", "twinkie", "undressing", "upskirt", "urethra play", "urophilia", "vagina", "venus mound", "vibrator", "violet wand", "vorarephilia", "voyeur", "vulva", "wank", "wetback", "wet dream", "white power", "women rapping", "wrapping men", "xx", "xxx", "yaoi", "yiffy", "zoophilia")
#corpus <- tm_map(corpus,removeWords,badWords)
##corpus <- tm_map(corpus, stemDocument)
#corpus <- tm_map(corpus, tolower)  
#corpus <- tm_map(corpus, removePunctuation); 
#corpus <- tm_map(corpus, removeNumbers)
##corpus <- tm_map(corpus, removeWords,stopwords("english"))
#corpus <- tm_map(corpus, removeWords, c("a", "s", "d", "m", "t")) #remove odd letters
#corpus <- tm_map(corpus, stripWhitespace)
#corpusn <- tm_map(corpus, PlainTextDocument)
#corpusn <- Corpus(VectorSource(corpusn)) #corrects format after converting to lower case
##dtm <- DocumentTermMatrix(corpusn, control=list(wordLengths=c(1,Inf)))
##freq <- colSums(as.matrix(dtm)) #frequency of terms across all documents in corpus
#textn <- data.frame(text=unlist(sapply(corpusn, `[`, "content")), stringsAsFactors=F)
##gram3 <- NGramTokenizer(textn, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))
#gram4 <- NGramTokenizer(textn, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!"))
##gram5 <- NGramTokenizer(textn, Weka_control(min = 5, max = 5, delimiters = " \\r\\n\\t.,;:\"()?!"))
#
##sparseParam=0.95
##dtmS <- removeSparseTerms(dtm, sparseParam)
##freqS <- colSums(as.matrix(dtmS)) #frequency of terms across all documents in corpus
##ordS <- order(freqS, decreasing=TRUE)#order frequencies to list most and least frequent terms
##freqS[head(ordS, 10)]
#
##-------------- Find Associations -----------
##tmp<-findAssocs(dtm, c("but", "the", "defense"), c(0.75, 0.75, 0.75)) # unigram model
##tmp$the[which(names(tmp$the)=="struggling")]  #find association between 'struggling' and 'the' - bigram
##ngram
#
## ---------------- Backoff Function --------------------
##term3 <- "i love you"
##tmp3<-n3gramsTable[which(names(n3gramsTable)==term)]
##sort(n3gramsTable[grep('i love \\w+', names(n3gramsTable))], decreasing=TRUE )
#
#------------Make Data Tables----------------------
#N4T <- as.data.table(table(gram4))
#setnames(N4T, 'N', 'freq')
#N4T[,gram4:=strsplit(gram4, '\\s')]
#N4T[, `:=`(w1=sapply(gram4, function(s) s[1]),
#           w2=sapply(gram4, function(s) s[2]),
#           w3=sapply(gram4, function(s) s[3]),
#           w4=sapply(gram4, function(s) s[4]),
#           gram4=NULL)]
#setkeyv(N4T, c('w1', 'w2', 'w3','freq'))
##system.time(icegrams <- N4T[list('i', 'love', 'ice')])

## 6. Create ngrams from quadgrams
#can also sum up all the different 4-grams that start with the same 
# prefix to get the trigram counts, because the 4-grams are already in sorted order in memory
N4T <- comboLines1
rm(comboLines)
rm(comboLines1)
rm(comboLines2)
#remove duplicate, subsequent words
N4T <- N4T[!(w1 == w2), ]
N4T <- N4T[!(w2 == w3), ]
N4T <- N4T[!(w3 == w4), ]
save(N4T, file="n4gramsFreq2plus.Rdata")

N3T <- N4T[, sum(freq), by=c('w1', 'w2', 'w3')]
setnames(N3T, c('V1'), c('freq'))
setcolorder(N3T, c('freq','w1','w2', 'w3'))
setkeyv(N3T,  c('w1','w2', 'freq'))
#N3T[list('i', 'love')]

N2T <- N3T[, sum(freq), by=c('w1', 'w2')]
setnames(N2T, c('V1'), c('freq'))
setcolorder(N2T, c('freq','w1','w2'))
setkeyv(N2T,  c('w1', 'freq'))

N1T <- N2T[, sum(freq), by=c('w1')]
setnames(N1T, c('V1'), c('freq'))
setcolorder(N1T, c('freq','w1'))
setkeyv(N1T,  c('w1','freq'))
# ------------------UNK--------------------------------------------
#find low frequency words in N1T
#lowFreq <- N1T[freq<=3]
#wordList<-lowFreq[,w1]
#replace these words with 'unk' in N4T and recreate ngrams
#for (i in 1:length(wordList)) {
#  N4T <- as.data.table(sapply(N4T, gsub, pattern=wordList[i], replacement="unk"))
#}

#get rid of low frequency ngrams
#N4T <- N4T[!(freq == 1), ]
#N3T <- N3T[!(freq == 1), ]
#N2T <- N2T[!(freq == 1), ]
#N1T <- N1T[!(freq == 1), ]

save(N4T, N3T, N2T, N1T, file="ngrams_4_3_2_1.RData")

## Calculate perplexity metric
perplexity<- function(tmpList){
  #tmpList[,Pr:=tmpList[,freq]/tmpList[,sum(freq)]]
  ndim <- dim(tmpList)
  BiPrs<- NA
  # calc bigram Pr for each possibility
  for (nn in 1:ndim[1]) { # for each possibility
    #tmpTerms<-as.character(tmpList[nn,2:(ndim[2]-1), with=FALSE])
    tmpTerms<-as.character(tmpList[nn,2:(ndim[2]), with=FALSE])
    finalBiPr<-0
    for (mm in 2:length(tmpTerms)) {
      word <- tmpTerms[mm]
      word
      tmpPrList <- N2T[list(tmpTerms[mm-1])]
      tmpPrList
      finalBiPr<-finalBiPr+log10(tmpPrList[w2==word][[1]]/tmpPrList[,sum(freq)])
      tmpr<-log10(tmpPrList[w2==word][[1]]/tmpPrList[,sum(freq)])
      mm<-mm+1
    }
    BiPrs[nn] <- finalBiPr
    exp <- (-1/length(BiPrs))
    perplexities <- BiPrs^exp
    return(perplexities)
  }
}
## Backoff Algorithm
backoffCondition <- function(term) {
  terms <- tolower(term)
  terms <- strsplit(terms, " ")
  n<-length(terms[[1]])
  if (n>3) {
    terms<-strsplit(paste(terms[[1]][n-3], terms[[1]][n-2], terms[[1]][n-1], terms[[1]][n]), " ")
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
    return(tmpList)
  } else {
    #tmpList[,Pr:=tmpList[,freq]/tmpList[,sum(freq)]] #Pr calc
    tmpList<-tmpList[order(-rank(freq))]
    print(tmpList)  
    if (dim(tmpList)[2]==6) {return(tmpList[,w5][1:4])
    } else if (dim(tmpList)[2]==5) {return(tmpList[,w4][1:4], tmpList)
    } else if (dim(tmpList)[2]==4) {return(tmpList[,w3][1:4], tmpList)
    } else if (dim(tmpList)[2]==3) {return(tmpList[,w2][1:4], tmpList)
    }
  }
}
# TEST
term<-"I love"
backoffCondition(term)
#browser()
#perplexity(tmpList)