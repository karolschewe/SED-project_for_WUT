#evaluating class (1 if United was better 2 if liverpool was better or equal)



this.dir<- dirname(parent.frame(2)$ofile)
setwd(this.dir)
df.utd<-read.csv(file = "starts_utd.csv")
#learning 16 elements; validation 5 elements; test 5 elements:
#shuffling
df.utd<-df.utd[sample(nrow(df.utd)),]

PU<-df.utd[1:16,2:15]
PW<-df.utd[17:21,2:15]
PT<-df.utd[22:26,2:15]

#naive Bayes
library(e1071)
class.nb <- naiveBayes(as.factor(class) ~ ., PU)
#tree 
library(rpart)
library(rpart.plot)
tree <- rpart(class ~ ., PU, minsplit = 1, minbucket = 1, cp = 0)
rpart.plot(tree, type = 1, extra = 1,main="full tree", cex.main=2)

#validation
prediction.nb <- predict(class.nb, PW)
prediction.tree<- predict(tree, newdata = PW, type = "vector")
