

data<-read.csv("E0.csv")

teams<-as.character(unique(data$HomeTeam))

count.stats<-function(dataset, team)
{
  df.home<-dataset[(dataset$HomeTeam == team),]
  df.away<-dataset[(dataset$AwayTeam == team),]
  homewins<-length(df.home[df.home$FTR=="H",1])
  awaywins<-length(df.away[df.away$FTR=="A",1])
  nWins<-homewins+awaywins
  homegoals<-sum(df.home$FTHG)
  awaygoals<-sum(df.away$FTAG)
  goals.lost<-sum(df.home$FTAG)+sum(df.away$FTHG)
  HTHG<-sum(df.home$HTHG)
  HTAG<-sum(df.away$HTAG)
  firsthalfgoalsratio<-(HTHG+HTAG)/(homegoals+awaygoals)
  yellowcards<-sum(df.home$HY)+sum(df.away$AY)
  allshots<-sum(df.home$HS)+sum(df.away$AS)
  shotsontargetratio<-(sum(df.home$HST)+sum(df.away$AST))/allshots
  foulsonteam<-sum(df.home$AF)+sum(df.away$HF)
  return(list(team = team,homewins=homewins,awaywins=awaywins,homegoals=homegoals,
              awaygoals=awaygoals,goals.lost=goals.lost,firsthalfgoalsratio=firsthalfgoalsratio,
              yellowcards=yellowcards,allshots=allshots,shotsontargetratio=shotsontargetratio,foulsonteam=foulsonteam))
}
all.teams.df<-data.frame(c())
for (i in teams)
{
  all.teams.df<-rbind(all.teams.df,data.frame(count.stats(data,i)))
}

#zmiana kolumny team na nazwe wiersza
rownames(all.teams.df)<-all.teams.df$team
all.teams.df<-all.teams.df[2:11]

all.teams.df.cmd <- cmdscale(dist(all.teams.df))

plot(all.teams.df.cmd, type = "n", xlab = "", ylab = "", axes = FALSE, main = "Styl gry druzyn w Premier League")
text(all.teams.df.cmd, labels = rownames(all.teams.df.cmd), cex = 0.9, xpd = TRUE)