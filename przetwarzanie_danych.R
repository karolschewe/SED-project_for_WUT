#plan jest taki zeby wysumowac staty  manUtd bez 6 ostatnich meczow.
#na tej podstawie jeden sezon jest jednym punktem pomiarowym
# mamy dataset 25 lat
# PU - 20 PT - 5
# ze wszystkich mamy gole, kto gral u siebie, wynik
# od 95 (23lata danych mamy jeszcze wyniki po pierwszej po�owie)
# z ca�ych danych mozemy wyznaczyc statystyki na 6 meczow przed koncem:
#liczba punktow
#miejsce w tabeli #bedzie bez tego
#liczba zwyciestw
#liczba remisow
#liczba porazek
#liczba goli zdobytych
#liczba goli straconych
#te
#wszystkie
#staty
#u siebie
#oraz
#na wyjezdzie
#oraz 
#w ostatnich 5/10/15 meczach

#ja sie zdecydowalem na powyzsze statystyki w wersjach ca�y sezon oraz ostatnie 10 meczow
#razem to bedzie 6 * 2 statystyk - 12 kolumn + kolumna z nazwa klasy (czyli kto byl lepszy) + data = razem 14 kolumn


library(stringr)
#import danych 
this.dir<- dirname(parent.frame(2)$ofile)
datafiles <- list.files(path=this.dir, pattern="*.csv",recursive=FALSE)
setwd(this.dir)
datasets<-lapply(datafiles, read.csv)
#wyjecie danych o man utd 
library(dplyr)
utd.data<-lapply(datasets, filter, HomeTeam == "Man United" | AwayTeam == "Man United")
#wyrzucenie ostatnich 6 meczow z danych
cut.data<-lapply(utd.data,function(x){
  x[1:32,]
  
})

#wybranie set�w do statystyk z ostatnich 10 meczow
last.ten.data<-lapply(cut.data, function(x){
  x[23:32,]
})

#funkcja zwracajaca ramke danych ze statystykami wyliczonymi na podstawie datasetu dla druzyny w ciapkach

calcStats<-function(dataset, team = "Man United"){
  
  nWins<-sapply(dataset, function(x){
    homewins<-length(filter(x,HomeTeam == team & FTR == "H")[,1])
    awaywins<-length(filter(x, AwayTeam == team & FTR == "A")[,1])
    return(homewins+awaywins)
  })
  
  nDraws<-sapply(dataset, function(x){
    length(filter(x,FTR == "D")[,1])
  })
  
  nLoses<-sapply(dataset, function(x){
    homeloses<-length(filter(x,HomeTeam == team & FTR == "A")[,1])
    awayloses<-length(filter(x, AwayTeam == team & FTR == "H")[,1])
    return(homeloses+awayloses)
  })
  
  nPoints<-nWins*3+nDraws
  
  goalsLost<-sapply(dataset, function(x) {
    lostHome<-sum((filter(x,HomeTeam == team))$FTAG)
    lostAway<-sum((filter(x,AwayTeam == team))$FTHG)
    return(lostHome+lostAway)
  })
  
  goalsScored<-sapply(dataset, function(x) {
    lostHome<-sum((filter(x,HomeTeam == team))$FTHG)
    lostAway<-sum((filter(x,AwayTeam == team))$FTAG)
    return(lostHome+lostAway)
  })
  
  date<-sapply(cut.data, function(x){
    str_sub(x[1,2],-2,-1)
  })

  return(stats<-data.frame(date,nPoints,nWins,nDraws,nLoses,goalsScored,goalsLost))
   
}
#evaluating class (1 if United was better 2 if liverpool was better or equal)
stats.Utd<-calcStats(cut.data)
stats.Utd10<-calcStats(last.ten.data)
liv.data<-lapply(datasets, filter, HomeTeam == "Liverpool" | AwayTeam == "Liverpool")
stats.Liv<-calcStats(liv.data, team = "Liverpool")
stats.full.Utd<-calcStats(utd.data)

utd.liv.diff<-stats.full.Utd$nPoints-stats.Liv$nPoints
class<-sapply(utd.liv.diff, function(x){
  if (x>0) {
    1
  }
  else{
    2
  }
})
class<-factor(class)
#merger of a statssets

export<-merge(stats.Utd,stats.Utd10,by.x = "date", by.y = "date")
export$class<-class


write.csv(export, file = "starts_utd.csv")
