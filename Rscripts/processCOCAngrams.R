#process coca files. make datatables for quad, tri , bi and unigrams
#rm(list=ls()[! ls() %in% c("N4T","N3T", "N2T", "N1T")])
#library(gdata)
#keep(a) #shows you which variables will be removed
#keep(a, sure = TRUE)

load("~/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/COCA")
N4T <- read.table("w4_.txt") #read txt file into data.table
N4T <- data.table(N4T)
setnames(N4T, c('V1', 'V2', 'V3', 'V4', 'V5'), c('freq', 'w1', 'w2', 'w3', 'w4')) #label columns
setkeyv(N4T,  c('w1','w2','w3', 'freq'))

# create other ngram data.tables
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
save(N4T, N3T, N2T, N1T, file="ngrams_4_3_2_1_COCA.RData")

#combine with my n4grams!
load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/COCA/ngrams_4_3_2_1_COCA.RData")
newLines<-N4T
load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/ngrams_4_3_2_1_unk3_nobads.RData")
load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/ngrams_4_3_2_1.RData")
comboLines<-N4T
rm(list=ls()[! ls() %in% c("comboLines", "newLines")])

#filename <- paste(code,formatC(1, width = 3, format = "d", flag = "0"),sep="")
#inputfile <- paste(dirNameFull,"n4grams/", filename,sep="")
#tmp<- (readLines(inputfile, skipNul = TRUE))
#comboLines <- as.data.table(table(tmp))
#setnames(comboLines, 'N', 'freq')
#comboLines[,tmp:=strsplit(tmp, '\\s')]
#comboLines[, `:=`(w1=sapply(tmp, function(s) s[1]),
#                  w2=sapply(tmp, function(s) s[2]),
#                  w3=sapply(tmp, function(s) s[3]),
#                  w4=sapply(tmp, function(s) s[4]),
#                  tmp=NULL)]
#setkeyv(comboLines, c('w1', 'w2', 'w3','freq'))

#filename <- paste(code,formatC(i, width = 3, format = "d", flag = "0"),sep="")#leading zeros
#inputfile <- paste(dirNameFull,"n4grams/", filename,sep="")
#tmp<- readLines(inputfile, skipNul = TRUE)
#newLines <- as.data.table(table(tmp))
#setnames(newLines, 'N', 'freq')
#newLines[,tmp:=strsplit(tmp, '\\s')]
#newLines[, `:=`(w1=sapply(tmp, function(s) s[1]),
#                w2=sapply(tmp, function(s) s[2]),
#                w3=sapply(tmp, function(s) s[3]),
#                w4=sapply(tmp, function(s) s[4]),
#                tmp=NULL)]
#setkeyv(newLines, c('w1', 'w2', 'w3','freq'))

comboLines <- rbind(comboLines,newLines)
comboLines <- comboLines[, sum(freq), by=c('w1', 'w2', 'w3', 'w4')]
setnames(comboLines, c('V1'), c('freq'))
setcolorder(comboLines, c('freq','w1','w2', 'w3','w4'))
setkeyv(comboLines, c('w1','w2', 'w3','freq'))

N4T<-comboLines
rm(comboLines)
# create other ngram data.tables
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
save(N4T, N3T, N2T, N1T, file="ngrams_4_3_2_1_COCA_orig_combo.RData")


