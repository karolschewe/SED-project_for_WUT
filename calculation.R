#evaluating class (1 if United was better 2 if liverpool was better or equal)
library(e1071)
library(rpart)
library(rpart.plot)

this.dir<- dirname(parent.frame(2)$ofile)
setwd(this.dir)
df.utd<-read.csv(file = "starts_utd.csv")
df.utd$class<-factor(df.utd$class)
#learning 16 elements; validation 5 elements; test 5 elements:
#shuffling
df.utd<-df.utd[sample(nrow(df.utd)),]


CV.nb <- function(data, K) {
  
  N <- nrow(data)
  
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudoprób
  sets <- sapply(1:K, function(i) ((i-1) * (N/K) + 1):(i * (N/K)))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla ka¿dej pseudopróby wyznaczamy liczbê pomy³ek
  res <- t(sapply(1:K, function(k) CV.main.nb(data.rnd[-c(sets[,k]),], data.rnd[sets[,k],])))
  
  res
}

CV.tree <- function(data, K) {
  
  N <- nrow(data)
  
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudoprób
  sets <- sapply(1:K, function(i) ((i-1) * (N/K) + 1):(i * (N/K)))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla ka¿dej pseudopróby wyznaczamy liczbê pomy³ek
  res <- t(sapply(1:K, function(k) CV.main.tree(data.rnd[-c(sets[,k]),], data.rnd[sets[,k],])))
  
  res
}


# G³ówna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudoprób) oraz PT
CV.main.nb <- function(learn, test) {
  
  learn.classifier <- naiveBayes(class ~ ., data = learn)
  test.pred <- predict(learn.classifier, newdata = test)
  
  # Macierz pomy³ek
  CM <- table(test$class, test.pred)
  
  # Liczba b³êdów
  sum(CM) - sum(diag(CM))
}

CV.main.tree <- function(learn, test) {
  
  learn.classifier <- naiveBayes(class ~ ., data = learn)
  test.pred <- predict(learn.classifier, newdata = test)
  
  # Macierz pomy³ek
  CM <- table(test$class, test.pred)
  
  # Liczba b³êdów
  sum(CM) - sum(diag(CM))
}




N <- nrow(df.utd)

# Dzielniki N
div <- which(!(N %% 1:N))

# Wykonanie wielokrotnej CV dla ró¿nych dzielników
mat <- sapply(div[-1], function(d) replicate(10, sum(CV.nb(df.utd, d)) / N))

# Wyznaczanie statystyk
cv.res <- as.data.frame(t(apply(mat, 2, function(x) c(mean(x), sd(x)))))
colnames(cv.res) <- c("mean", "sd")
cv.res$K <- div[-1]

library(Hmisc)

with(cv.res, errbar(K, mean, mean + sd, mean - sd, main = "Naive Bayes mean error"))



N <- nrow(df.utd)

# Dzielniki N
div <- which(!(N %% 1:N))

# Wykonanie wielokrotnej CV dla ró¿nych dzielników
mat <- sapply(div[-1], function(d) replicate(10, sum(CV.tree(df.utd, d)) / N))

# Wyznaczanie statystyk
cv.res <- as.data.frame(t(apply(mat, 2, function(x) c(mean(x), sd(x)))))
colnames(cv.res) <- c("mean", "sd")
cv.res$K <- div[-1]


with(cv.res, errbar(K, mean, mean + sd, mean - sd, main = "Tree mean error"))


df.utd<-df.utd[,3:15]
levels(df.utd$class)<-c("United","Liverpool")

# drzewo na pelnych danych
tree <- rpart(class ~ ., df.utd, minsplit = 1, minbucket = 1, cp = 0)
rpart.plot(tree, type = 1, extra = 1,main="full tree", cex.main=2)




votes.repub <- cluster::votes.repub