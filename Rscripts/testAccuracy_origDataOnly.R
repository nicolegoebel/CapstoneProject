#test accuracy of ngrams with COCA ngrams
library(data.table)
rm(list=ls())
#load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/final/en_US/ngrams_4_3_2_1.RData")
load(file="/Users/nicolegoebel/Dropbox/Courses/Coursera_Data_Science_2014/CapstoneProject/shiny_NLP_COCA/ngrams_4_3_2_1_COCA_orig_combo.RData")
test4 <- N4T
test3 <- N3T
test2 <- N2T
rm(list=ls()[ls() %in% c("N4T","N3T", "N2T", "N1T")])
#break up test4 into test and training sets
testNo <- round(dim(test4)[1]*.1)
vec <- c(1:dim(test4)[1])
sampleTest <- sample(vec, testNo, replace=FALSE)
gram4<-test4
rm(test4)#;rm(train4)
test4<-gram4[sampleTest,]
train4<-gram4[!sampleTest,]
rm(sampleTest)

testNo <- round(dim(test3)[1]*.4)
vec <- c(1:dim(test3)[1])
sampleTest <- sample(vec, testNo, replace=FALSE)
gram3<-test3
rm(test3)#;rm(train3)
#test3<-gram3[sampleTest,]
train3<-gram3[!sampleTest,]
rm(sampleTest)

testNo <- round(dim(test2)[1]*.3)
vec <- c(1:dim(test2)[1])
sampleTest <- sample(vec, testNo, replace=FALSE)
gram2<-test2
rm(test2)#;rm(train2)
#test2<-gram2[sampleTest,]
train2<-gram2[!sampleTest,]
rm(sampleTest)
rm("gram2", "gram3", "gram4", "testNo", "vec")
#train4<-test4
#setkeyv(train4, c('w1','w2', 'w3','freq'))
#train3<-test3
#setkeyv(train3, c('w1','w2', 'freq'))
#train2<-test2
#setkeyv(train2, c('w1','freq'))
yup <- 0
nope <-0
for (i in 1:dim(test4)[1]) {
    ans <- (test4[[5]][i])
    tmpList<-train4[list(test4[[2]][i], test4[[3]][i], test4[[4]][i])]
    if (!all(is.na(tmpList[,freq]))) {
        if (ans[[1]] %in% tmpList$w4[1:3]) { yup <- yup + 1 } else { nope <- nope+1 }
      } else {tmpList<-train3[list(test4[[3]][i], test4[[4]][i])]
              if (!all(is.na(tmpList[,freq]))) {
                if (ans[[1]] %in% tmpList$w3[1:3]) { yup <- yup+1 } else { nope <- nope+1 }
              } else {tmpList<-train2[list(test4[[4]][i])]
                      if (ans[[1]] %in% tmpList$w2[1:3]) { yup <- yup+1 } else { nope <- nope+1 }
              } 
      }
}
print(paste("n = ", nope))
print(paste("y = ", yup))
print(paste("accuracy = ", (yup/(yup+nope))*100))
browser()