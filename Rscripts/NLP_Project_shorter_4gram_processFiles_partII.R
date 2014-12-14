# THis was an attempt to get rid of profanity. But I decided that it was not
# my place to judge which words should be omitted. So I left them in.

(list=ls()) 
library(data.table)
library(tm)
#library(ggplot2)
#require(RWeka)
options(mc.cores=1)
options( java.parameters = "-Xmx3g" )
library(RWeka)
#library(wordcloud)
#library(Rgraphviz)
#library(knitr)
#library(SnowballC)
library(stringi)
library(stylo)

dirNameFull='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/'
setwd(dirNameFull)

# once n4grams was created by using all the files, I then got the frequency
# of the unigrams in order to turn all freq<=3 to "unk"
# but I had to do this to the original files, as it seemed to be
# too cumbersome in R. 

# so I will load in the cleaned up ngrams:
#load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/shiny_NLP/ngrams_4_3_2_1.RData", envir=.GlobalEnv)
load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/wordList.RData")
load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/profanities.RData")

# calculate the terms to be replaced by unk
#find low frequency words in N1T
#lowFreq <- N1T[freq<=3]
#wordList<-lowFreq[,w1]
#rm(N1T)
#rm(lowFreq)
#wordList <- gsub('^', ' ', wordList) # put space in front of each word
#wordList<-gsub('$', ' ', wordList) # put space in front of each word

# and then process them by replacing the least frequent terms with unk
#replace these words with 'unk' in N4T and recreate ngrams
#for (i in 1:length(wordList)) {
#  N4T <- as.data.table(sapply(N4T, gsub, pattern=wordList[i], replacement="unk"))
#}
code="tw"
for (i in 1:393){
  filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
  inputfile <- paste(dirNameFull,code,"splits/", filename,sep="")
  Lines <- readLines(inputfile, skipNul = TRUE)
  #replace these words with 'unk' in N4T and recreate ngrams
  Lines = tolower (Lines)
  Lines <- gsub("[^\x20-\x7F]","", Lines)
  Lines <- gsub('\\.', ' ', Lines)
  Lines <- gsub(',', ' ', Lines)
  Lines <- gsub(' u ', ' you ', Lines)
  Lines <- gsub(' *& *', ' and ', Lines)
  Lines <- gsub(' &', ' and', Lines)
  Lines <- gsub('<3', '', Lines)
  Lines <- gsub('[0-9]', '', Lines)
  Lines <- gsub(' \\"', ' ', Lines)
  Lines <- gsub('"\\ ', ' ', Lines)
  #Lines <- gsub('"\\', ' ', Lines)
  Lines <- gsub('\\"', ' ', Lines)

  Lines <- gsub('\\" ', ' ', Lines)
  Lines <- gsub(' \\" ', ' ', Lines)
  Lines <- gsub(' \\, +', ' ', Lines)
  
  Lines <- gsub(' \\(', '', Lines)
  Lines <- gsub(':\\(', '', Lines)
  Lines <- gsub(':+)', '', Lines)
  Lines <- gsub(':*)', '', Lines)
  Lines <- gsub(':-', '', Lines)
  Lines <- gsub('\\!', ' ', Lines)
  Lines <- gsub('\\?', ' ', Lines)
  Lines <- gsub('\\?.', ' ', Lines)
  Lines <- gsub('-', '', Lines)
  Lines <- gsub(' --', '', Lines)
  Lines <- gsub(':', '', Lines)
  system.time(Lines[Lines %in% wordList]<-' unk ')
  system.time(Lines[Lines %in% profanities]<-'')
  #for (i in 1:length(wordList)) {
  #  Lines<-as.data.table(gsub(pattern=wordList[i], replacement=' unk', Lines))
  #  #Lines <- as.data.table(sapply(Lines, gsub, pattern=wordList[i], replacement=' unk'))
  #}
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
  Lines <- gsub("[^\x20-\x7F]","", Lines)
  Lines = tolower (Lines)
  Lines <- gsub('\\.', ' ', Lines)
  Lines <- gsub(',', ' ', Lines)
  Lines <- gsub(' u ', ' you ', Lines)
  Lines <- gsub(' *& *', ' and ', Lines)
  Lines <- gsub(' &', ' and', Lines)
  Lines <- gsub('<3', '', Lines)
  Lines <- gsub('[0-9]', '', Lines)
  Lines <- gsub(' \\"', ' ', Lines)
  Lines <- gsub('"\\ ', ' ', Lines)
  Lines <- gsub('\\"', ' ', Lines)
  Lines <- gsub('\\" ', ' ', Lines)
  Lines <- gsub(' \\" ', ' ', Lines)
  Lines <- gsub(' \\, +', ' ', Lines)
  Lines <- gsub(' \\(', '', Lines)
  Lines <- gsub(':\\(', '', Lines)
  Lines <- gsub('#', ' ', Lines)
  Lines <- gsub(':+)', '', Lines)
  Lines <- gsub(':*)', '', Lines)
  Lines <- gsub(':-', '', Lines)
  Lines <- gsub('\\!', ' ', Lines)
  Lines <- gsub('\\?', ' ', Lines)
  Lines <- gsub('\\?.', ' ', Lines)
  Lines <- gsub('-', '', Lines)
  Lines <- gsub(' --', '', Lines)
  Lines <- gsub(':', '', Lines)
  system.time(Lines[Lines %in% wordList]<-' unk ')
  system.time(Lines[Lines %in% profanities]<-'')
  #system.time(lapply(wordList, function(w) {my_dtb[grepl(pattern = w, x=my_word_column, ignore.case=T, perl=T),my_word:="unk"]}))
  #for (i in 1:length(wordList)) {
  #  Lines<-as.data.table(gsub(pattern=wordList[i], replacement=' unk', Lines))
  #  #Lines <- as.data.table(sapply(Lines, gsub, pattern=wordList[i], replacement=' unk'))
  #}
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
  Lines <- gsub("[^\x20-\x7F]","", Lines)
  Lines = tolower (Lines)
  Lines <- gsub('\\.', ' ', Lines)
  Lines <- gsub(',', ' ', Lines)
  Lines <- gsub(' u ', ' you ', Lines)
  Lines <- gsub(' *& *', ' and ', Lines)
  Lines <- gsub(' &', ' and', Lines)
  Lines <- gsub('<3', '', Lines)
  Lines <- gsub('[0-9]', '', Lines)
  Lines <- gsub(' \\"', ' ', Lines)
  Lines <- gsub('"\\ ', ' ', Lines)
  Lines <- gsub('\\"', ' ', Lines)
  Lines <- gsub('\\" ', ' ', Lines)
  Lines <- gsub(' \\" ', ' ', Lines)
  Lines <- gsub(' \\, +', ' ', Lines)
  Lines <- gsub(' \\(', '', Lines)
  Lines <- gsub(':\\(', '', Lines)
  Lines <- gsub('#', ' ', Lines)
  Lines <- gsub(':+)', '', Lines)
  Lines <- gsub(':*)', '', Lines)
  Lines <- gsub(':-', '', Lines)
  Lines <- gsub('\\!', ' ', Lines)
  Lines <- gsub('\\?', ' ', Lines)
  Lines <- gsub('\\?.', ' ', Lines)
  Lines <- gsub('-', '', Lines)
  Lines <- gsub(' --', '', Lines)
  Lines <- gsub(':', '', Lines)
  system.time(Lines[Lines %in% wordList]<-' unk ')
  system.time(Lines[Lines %in% profanities]<-'')
  #for (i in 1:length(wordList)) {
  #  Lines<-as.data.table(gsub(pattern=wordList[i], replacement=' unk', Lines))
  #  #Lines <- as.data.table(sapply(Lines, gsub, pattern=wordList[i], replacement=' unk'))
  #}
  size4 <- unlist(lapply(Lines[grep("[^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]*", Lines)], function(Lines) make.ngrams(txt.to.words(Lines, splitting.rule = "[ \t\n]+"), ngram.size = 4)))
  outputfile <- paste("n4grams/", filename,sep="")
  write.table(size4, outputfile , quote=FALSE, row.names = FALSE, col.names = FALSE)
  rm(size4)
  rm(Lines)
}

# concatenate 4 grams and make a table...
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
dirNameFull='~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/'
save(comboLines, file="combinedn4grams_unk_nobads.RData")
#load("~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/combinedn4grams_unk_nobads.RData")
N4T <- comboLines
rm(comboLines)
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
save(N4T, N3T, N2T, N1T, file="ngrams_4_3_2_1_unk3_nobads.RData")