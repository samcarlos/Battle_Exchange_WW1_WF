westernfront=read.csv("C:/Users/sa/Google Drive/wiki'd battles/western_front_major_battles.csv")


time.days=as.Date(westernfront$end.date, "%m/%d/%y")-as.Date(westernfront$begin.date, "%m/%d/%y")

library(zoo)
dates=seq.Date(as.Date("1870-1-1"), to=as.Date("1950-1-1"), by="day")

westernfront$casulties.rate=westernfront$casulties.numbers/(as.numeric(time.days)+1)

battle.rate.mat=matrix(0,length(dates), length(westernfront$begin.date))
time.days=as.numeric(time.days)
westernfront$begin.date=as.Date(westernfront$begin.date, "%m/%d/%y")
westernfront$begin.date=unlist(lapply(westernfront$begin.date, function(x) seq(x, length=2, by="-100 years")[2]))
westernfront$end.date=as.Date(westernfront$end.date, "%m/%d/%y")
westernfront$end.date=unlist(lapply(westernfront$end.date, function(x) seq(x, length=2, by="-100 years")[2]))
westernfront$begin.date=as.Date(westernfront$begin.date)
westernfront$end.date=as.Date(westernfront$end.date)




for(i in 1:dim(westernfront)[1]){
  battle.rate.mat[which(dates==(westernfront$begin.date)[i]):(which(dates==westernfront$begin.date[i]+time.days[i])),i]=westernfront$casulties.rate[i]
}

agg.battle.rate=aggregate(battle.rate.mat,by=list(as.yearmon(dates)),FUN=sum)
agg.battle.rate[is.na(agg.battle.rate)]=0
battle.rate.mat.ww1=agg.battle.rate[which(agg.battle.rate[,1]=="Aug 1914"):which(agg.battle.rate[,1]=="Dec 1918"),-1]

battle.rate.mat.ww1[is.na(battle.rate.mat.ww1)]=0
ts.plot(rowSums(battle.rate.mat.ww1))

countries.included=unique(westernfront$ParsedCountry)[-c(5:6)]
websites=unique(westernfront$website)

battle.array=array(0,c(length(unique(westernfront$website)),dim(battle.rate.mat.ww1)[1],length(countries.included)))
for(i in 1:length(countries.included)){
  for(j in 1:length(unique(westernfront$website))){
    index=intersect(which(westernfront$website %in%websites[j]) ,which(westernfront$ParsedCountry %in% countries.included[i]))
    if(length(index)!=0){battle.array[j,,i]=battle.rate.mat.ww1[,index]}    
  }
  
}



is.included=matrix(0,26,4)
for(i in 1:4){
  is.included[,i]=(rowSums(battle.array[,,i])>0)*1
}


battle.array=battle.array[,,(c(1,2,3,5,4))]
stan.data=list(T=53,nCountries=5,nBattles=26,battle_array=battle.array, num_battles=apply(battle.array,c(3), function(x) sum(rowSums(x)>0))[-5],is_included=is.included)

library(rstan)


###
stan.code.battle.rate3="
data{
  int T;
  int nBattles;
  int nCountries;
  matrix[T, nCountries] battle_array[nBattles]; 
  vector[nCountries-1] num_battles;
  matrix[nBattles,nCountries-1] is_included;
  
}
parameters{
  vector<lower=0,upper=5>[nCountries-1] betas[T];
  vector<lower=0.1>[nCountries-1] betasSigma;
  vector[nCountries-1] battleSigma;
  matrix[nBattles,nCountries-1] betasError;
  real<lower=0.1> battleError;
  vector<lower=0>[2] GermanNA;
}
transformed parameters{
  matrix[T,nBattles] allies_exchange;
  matrix[nBattles,nCountries-1] betasError1;
  matrix[nBattles,nCountries-1] betasErrorConstraint;
  matrix[T,nBattles] GermanCas;
  vector[(nCountries-1)] sumBetas;

  for(i in 1:nBattles)
    for(t in 1:T)
      GermanCas[t,i]<-battle_array[i,t,5];
  GermanCas[2,7]<-GermanNA[1];
  GermanCas[33,20]<-GermanNA[2];

  for(i in 1:nBattles)
    for(j in 1:(nCountries-1))
      betasError1[i,j]<-0;
  
  for(i in 1:nBattles)
    for(j in 1:(nCountries-1))
      if(is_included[i,j])
        betasError1[i,j]<-betasError[i,j];
  
  for(j in 1:(nCountries-1))
    for(i in 1:nBattles)
      sumBetas[j]<-0;
  
  
  for(j in 1:(nCountries-1))
    for(i in 1:nBattles)
      sumBetas[j]<-sumBetas[j]+betasError1[i,j];
  
  for(i in 1:nBattles)
    for(j in 1:(nCountries-1))
      betasErrorConstraint[i,j]<-betasError1[i,j]-sumBetas[j]/num_battles[j];
  
  for(t in 1:T)
    for(i in 1:nBattles)
      allies_exchange[t,i]<-1;
  
  
  for(t in 1:T)
    for(i in 1:nBattles)
      for(q in 1:(nCountries-1))
        allies_exchange[t,i]<-allies_exchange[t,i]+(betasErrorConstraint[i,q]+betas[t,q])*battle_array[i,t,q];

}

model{
  GermanNA[1]~normal(battle_array[7,2,2],3000);
  GermanNA[2]~normal(battle_array[20,33,3],3000);


  for(j in 1:(nCountries-1)) betas[1,j]~normal(1,.5);
  for(i in 2:T)
    for(j in 1:(nCountries-2)) 
      betas[i,j]~normal(betas[i-1,j],betasSigma[j]);
  
  betas[48,4]~normal(1,.1);
  for(i in 49:T)
    betas[i,4]~normal(betas[i-1,4],betasSigma[4]);
  
  for(i in 1:nBattles)
    for(j in 1:(nCountries-1))
      if(is_included[i,j])
        betasError[i,j]~normal(0,.1);
  
  for(i in 1:nBattles)
    for(t in 1:T) 
      GermanCas[t,i]~normal(allies_exchange[t,i],(GermanCas[t,i]*.1+1));
  
}

"



exfit2 <- stan(model_code = stan.code.battle.rate3,data=stan.data, iter = 100,chains=5)
save(exfit2,file="/Users/sweiss/Google Drive/wiki'd battles/fittedTSExchangeWF.Rdata")


mat1=as.matrix(exfit2)
boxplot(mat1[,grep("NA", colnames(mat1))])

exch=mat1[,grep("allies_exchange", colnames(mat1))]
boxplot(exch[,grep(",7",colnames(exch))])

betas=mat1[,grep("betas",colnames(mat1))]
betas1=betas[,grep(",1]",colnames(betas))]
betas2=betas[,grep(",2]",colnames(betas))]
betas3=betas[,grep(",3]",colnames(betas))]
betas4=betas[,grep(",4]",colnames(betas))]
ts.plot(colMeans(betas1)[1:52])
ts.plot(colMeans(betas2)[1:52], ylim=c(0,2))
lines(colMeans(betas3)[1:52])
lines(colMeans(betas1)[1:4])
lines(c(rep(NA,48),colMeans(betas4)[49:52]))



ts.plot(colMeans(betas4)[1:53])


ts.plot(colMeans(betas1))

