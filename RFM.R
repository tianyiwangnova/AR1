#get the data
sales=read.csv("C:/Users/Think/Documents/customers.csv",as.is = TRUE)
n=nrow(sales)

#get the last.order.dates
time1=sales$last.order.dat
time2=strsplit(time1,split=" ")
years=rep(0,n)
for(i in 1:n){
  years[i]=time2[[i]][4]
}
months=rep(0,n)
months.names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
for(i in 1:n){
  months[i]=which(months.names==time2[[i]][2])
}
dates=rep(0,n)
for(i in 1:n){
  dates[i]=as.numeric(strsplit(time2[[i]][3],split = ","))
}
time3=rep("n",n)
for(i in 1:n){
  time3[i]=paste(years[i],months[i],dates[i],sep="-")
}
time3=as.Date(time3)

#get the money
money1=sales$average.order.value
money2=rep(0,n)
for(i in 1:n){
  x=money1[i]
  len=nchar(x)
  money2[i]=as.numeric(substr(x,start=2,stop=len))
}

###for the numbers with the thousand sign ",", manually input the data
money2[2]=1237.5

#Get the Recency
calculate.recency=function(x,date){
  days=round(as.numeric(date-x))
  return(days)
}
date=as.Date("2017-5-19")
recency=calculate.recency(time3,date)

#Get the Frequency
frequency=sales$orders

#Get the Money
money=money2

#merge RFM to data frame
RFM=data.frame(recency,frequency,money)

#create RFM levels
getRFMinterval <- function(RFMdata, interval, original){
  #get min value
  maxData <- max(RFMdata)
  minData <- min(RFMdata)
  factorInterval <- (maxData-minData)/interval
  difference <- RFMdata - minData
  
  return(
    if(!original){
      ifelse(difference>0, ceiling(difference/factorInterval), 1)  
    }
    else {
      ifelse(difference>0, difference/factorInterval, 1)  
    }
  )
}

RFM$RankR <- 4-getRFMinterval(RFM$recency, 3, F)
RFM$RankF <- getRFMinterval(RFM$frequency, 3, F)
RFM$RankM <- getRFMinterval(RFM$money,3, F)

#create weight
RFM$Weight <- RFM$RankR*100+RFM$RankF*10+RFM$RankM



write.csv(RFM,"C:/Users/Think/Documents/rfm.csv")

summ=data.frame(rbind(summary(RFM$recency),summary(RFM$frequency),summary(RFM$money)))
rownames(summ)=c("R","F","M")
par(mfrow=c(1,3))
hist(RFM$recency)
hist(RFM$frequency)
hist(RFM$money)

summ1=data.frame(rbind(table(RFM$RankR),table(RFM$RankF),table(RFM$RankM)))
rownames(summ1)=c("R","F","M")
colnames(summ1)=c(1,2,3)

RFM[(RFM$RankF==3)|(RFM$RankM==3)|(RFM$RankM==2), ]
supervip=RFM[(RFM$RankF==3)|(RFM$RankM==3)|(RFM$RankM==2), ]

#Recalculate the RFM levels
num=which((RFM$RankF==3)|(RFM$RankM==3)|(RFM$RankM==2))
RFM1=RFM[-num,]
RFM1$RankR <- 4-getRFMinterval(RFM1$recency, 3, F)
RFM1$RankF <- getRFMinterval(RFM1$frequency, 3, F)
RFM1$RankM <- getRFMinterval(RFM1$money,3, F)
summ2=data.frame(rbind(table(RFM1$RankR),table(RFM1$RankF),table(RFM1$RankM)))
rownames(summ2)=c("R","F","M")
colnames(summ2)=c(1,2,3)



#combine the result
RFM1$type=paste(RFM1$RankR,RFM1$RankF,RFM1$RankM,sep="")
type.count=as.numeric(table(RFM1$type))
type=names(table(RFM1$type))
type.count.frame=data.frame(type,type.count)
write.csv(type.count.frame,"C:/Users/Think/Documents/type.csv")
plot(type.count)
#calculate the weighted score
##here we decide the weight as R:

write.csv(RFM1,"C:/Users/Think/Documents/rfm1.csv")

par(mfrow=c(1,3))
boxplot(RFM1$recency~RFM1$RankR,xlab="rank r",ylab="recency")
boxplot(RFM1$frequency~RFM1$RankR,xlab="rank r",ylab="frequency")
boxplot(RFM1$money~RFM1$RankR,xlab="rank r",ylab="money")

par(mfrow=c(1,3))
boxplot(RFM1$recency~RFM1$RankF,xlab="rank f",ylab="recency")
boxplot(RFM1$frequency~RFM1$RankF,xlab="rank f",ylab="frequency")
boxplot(RFM1$money~RFM1$RankF,xlab="rank f",ylab="money")

par(mfrow=c(1,3))
boxplot(RFM1$recency~RFM1$RankM,xlab="rank m",ylab="recency")
boxplot(RFM1$frequency~RFM1$RankM,xlab="rank m",ylab="frequency")
boxplot(RFM1$money~RFM1$RankM,xlab="rank m",ylab="money")

new.type=rep(0,nrow(RFM1))
for (i in 1:n){
  if (RFM1$RankR[i]==1 | RFM1$RankR[i]==2) {t=1}
  if (RFM1$type[i]=="311"|RFM1$type[i]=="312") {t=2}
  if (RFM1$type[i]=="313"|RFM1$type[i]=="321"|RFM1$type[i]=="322"|RFM1$type[i]=="323"|RFM1$type[i]=="321"|RFM1$type[i]=="331"|RFM1$type[i]=="332"){t=3}
  new.type[i]=t
}


x=data.frame(RFM1$recency,RFM1$frequency,RFM1$money)
kmean=kmeans(x,centers = 3)
type.kmean=kmean$cluster

table(new.type,type.kmean)

library(rpart)
library(rpart.plot)
tree=rpart(new.type~RFM1$recency+RFM1$frequency+RFM1$money)
par(mfrow=c(1,1))
rpart.plot(tree,type=3)

type1=RFM1[new.type==1,]
type2=RFM1[new.type==2,]
type3=RFM1[new.type==3,]

par(mfrow=c(1,3))
RFM1$new.type=new.type
boxplot(RFM1$recency~RFM1$new.type,xlab="new type",ylab="recency")
