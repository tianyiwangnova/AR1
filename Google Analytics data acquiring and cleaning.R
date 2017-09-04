require(devtools)
#install devtools package for downloading packages from github
devtools::install_github("Tatvic/RGoogleAnalytics")
#installing RGoogleAnalytics package from github


require(RGoogleAnalytics)
token <- Auth("524840298457-eo9fd54chjaruua3a71mp8sld7b572ml.apps.googleusercontent.com","IJ5igRIAgvyWI3vNPNEOIlyO")
# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created
# and saved
save(token,file="./token_file")
# Save the token object for future sessions
ValidateToken(token)
# In future sessions it can be loaded by running load("./token_file")
# Build a list of all the Query Parameters
query.list <- Init(start.date = "2017-08-28",
                    end.date = "2017-08-30",
                    metrics = c("ga:pageViews","ga:pageValue","ga:timeOnPage"),
                    dimensions = c("ga:date","ga:pagePath","ga:medium"),
                    sort="ga:date",
                    max.results = 10000,
                    table.id = "ga:65357171")

# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data and store it in a data-frame
ga.data <- GetReportData(ga.query, token)


#-------------------------------------------get the product views---------------------------------------------------
n=nrow(ga.data)
index1=rep(0,n)
for (i in 1:n){
  j=grep("products|collections",ga.data$pagePath[i])
  if(length(j)!=0){index1[i]=i}
}
index2=which(index1==0)
index3=index1[-index2]
ga.data.products=ga.data[index3,]
bag.name.match=c("morning","tote-classic","sunday-mini","luna","cher-tote","eleanor","olivia-e","olivia-z","madison","jules","anya","anne-shopper")
bag.names=c("Morning Cross-Body","Sunday Tote Classic","Sunday Mini","Luna Cross-Body","Cher-Tote","Eleanor Satchel","Olivia E Wallet","Olivia Z Wallet","Madison Backpack","Jules Bucket Bag II","Anya Cross-Body","Anne Shopper")
colors=c("forest-green","dusty-rose","mustard","powder-pink","bordeaux","ivory","black","mud-grey","dark-green","taupe","light-grey","pale-pink","creme","light-gray","mud-gray","red","russet")
create.index=function(x,n){
  r=rep(0,n) 
  for(i in 1:n){
    j=grep(x,ga.data.products$pagePath[i])
    if(length(j)!=0){r[i]=1}
  }
  return(r)
}
for (i in 1:length(bag.name.match)){
  x=bag.name.match[i]
  ga.data.products[,bag.names[i]]=create.index(x,nrow(ga.data.products))
}
for (i in 1:length(colors)){
  x=colors[i]
  ga.data.products[,colors[i]]=create.index(x,nrow(ga.data.products))
}
index4=NULL
for (i in 1:nrow(ga.data.products)){
  if(sum(ga.data.products[i,which(names(ga.data.products)==bag.names[1]):ncol(ga.data.products)])<2){
    index4=c(index4,i)
  }
}
ga.data.products=ga.data.products[-index4,]
type=rep("",nrow(ga.data.products))
color=rep("",nrow(ga.data.products))
begin_bag=which(names(ga.data.products)==bag.names[1])
end_bag=which(names(ga.data.products)==bag.names[length(bag.names)])
begin_color=which(names(ga.data.products)==colors[1])
end_color=which(names(ga.data.products)==colors[length(colors)])
for (i in 1:nrow(ga.data.products)){
  type[i]=bag.names[which(ga.data.products[i,][begin_bag:end_bag]==1)]
  color[i]=colors[which(ga.data.products[i,][begin_color:end_color]==1)]
}
for (i in 1:length(color)){
  if (color[i]=="light-grey"){color[i]="light-gray"}
  if (color[i]=="mud-grey"){color[i]="mud-gray"}
}
ga.data.products['type']=type
ga.data.products['color']=color
ga.data.products.final=data.frame(ga.data.products$date,ga.data.products$pagePath,ga.data.products$medium,
                                  ga.data.products$pageViews,ga.data.products$pageValue,ga.data.products$timeOnPage,
                                  ga.data.products$type,ga.data.products$color)
names(ga.data.products.final)=c("date","pagePath","medium","pageViews","pageValue","timeOnPage","type","color")

index6=rep("No",nrow(ga.data.products.final))
for (i in 1:nrow(ga.data.products.final)){
  j=grep("most-popular-bags",ga.data.products.final$pagePath[i])
  if(length(j)!=0){index6[i]="Yes"}
}
ga.data.products.final['from [the most popular bags] section?']=index6


formatdata=function(x){
  y=paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep = "/")
}
ga.data.products.final$date=formatdata(ga.data.products.final$date)

fullname=rep("",nrow(ga.data.products.final))
for (i in 1:nrow(ga.data.products.final)){
  fullname[i]=paste(ga.data.products.final$type[i],ga.data.products.final$color[i],sep="-")
}
ga.data.products.final["full name"]=fullname

filename1=paste("ga data products views",min(ga.data.products$date),max(ga.data.products$date),sep = " ")
filename1=paste(filename1,".csv")
write.csv(ga.data.products.final,filename1,row.names = FALSE)

#------------------------------get pageviews from [ABOUT] section----------------------------------------
index7=rep(0,n)
for (i in 1:n){
  j=grep("pages",ga.data$pagePath[i])
  if(length(j)!=0){index7[i]=i}
}
index8=which(index7==0)
index7=index7[-index8]
ga.data.about=ga.data[index7,]

index9=rep("",nrow(ga.data.about))
for(i in 1:length(index9)){
  x=ga.data.about$pagePath[i]
  index9[i]=substr(x,8,nchar(x))
}

ga.data.about['detail']=index9
ga.data.about$date=formatdata(ga.data.about$date)

filename2=paste("ga data about pages views",min(ga.data.products$date),max(ga.data.products$date),sep = " ")
filename2=paste(filename2,".csv")
write.csv(ga.data.about,filename2,row.names = FALSE)

#------------------------------get pageviews from affiliates------------------------------------------------

index10=NULL
for (i in 1:n){
  j=grep("\\/\\?ref\\=[0-9]+",ga.data$pagePath[i])
  if(length(j)!=0){index10=c(index10,i)}
}
ga.data.ref=ga.data[index10,]
refnum=rep(0,nrow(ga.data.ref))
for (i in 1:nrow(ga.data.ref)){
  x=ga.data.ref$pagePath[i]
  matches=gregexpr(pattern="[0-9]+",text=x)
  refnum[i]=regmatches(x,matches)[[1]][1]
}
refnum=as.numeric(refnum)
ga.data.ref['affiliate number']=refnum
aff=read.csv("affiliates_list.csv",as.is = TRUE)
ga.data.ref[,"affiliate"]=rep("unknown",nrow(ga.data.ref))
for (i in 1:nrow(ga.data.ref)){
  if (length(which(aff$ID==ga.data.ref$`affiliate number`[i]))!=0) {
    ga.data.ref$affiliate[i]=aff$Name[which(aff$ID==ga.data.ref$`affiliate number`[i])]
  }
}
ga.data.ref$date=formatdata(ga.data.ref$date)

filename3=paste("ga data affiliates",min(ga.data.products$date),max(ga.data.products$date),sep = " ")
filename3=paste(filename3,".csv")
write.csv(ga.data.ref,filename3,row.names = FALSE)

#-----------------------------------prepare several pivot tables---------------------------------------------------

library(reshape2)
library(dplyr)

pview.type=data.frame(ga.data.products.final$date,ga.data.products.final$type,ga.data.products.final$pageViews)
names(pview.type)=c("date","type","pageViews")
pview.type1=dcast(pview.type,date~type)
pview.type2=melt(pview.type1,variable.name = "type",value.name = "pageview",id.vars = "date")
pview.type2=arrange(pview.type2,date,type)
pview.color=data.frame(ga.data.products.final$date,ga.data.products.final$pageViews,ga.data.products.final$color)
names(pview.color)=c("date","pageViews","colors")
pview.color1=dcast(pview.color,date~color)
pview.color2=melt(pview.color1,variable.name="color",value.name = "pageview",id.vars="date")
pview.color2=arrange(pview.color2,date,color)
pview=data.frame(ga.data.products.final$pageViews,ga.data.products.final$type,ga.data.products.final$color)
names(pview)=c("pageViews","type","color")
pview1=dcast(pview,type~color)
pview2=melt(pview1,variable.name = "color",value.name = "pageview",id.vars = "type")

filename4=paste("pageview by type",min(ga.data$date),max(ga.data$date),sep = " ")
filename4=paste(filename4,".csv")
write.csv(pview.type2,filename4,row.names = FALSE)
filename5=paste("pageview by color",min(ga.data$date),max(ga.data$date),sep = " ")
filename5=paste(filename5,".csv")
write.csv(pview.color2,filename5,row.names = FALSE)
filename6=paste("pageview type and color",min(ga.data$date),max(ga.data$date),sep = " ")
filename6=paste(filename6,".csv")
write.csv(pview2,filename6,row.names = FALSE)

mostpop=ga.data.products.final[ga.data.products.final$`from [the most popular bags] section?`=="Yes",]
mostpop1=data.frame(mostpop$pageViews,mostpop$type,mostpop$color)
names(mostpop1)=c("pageViews","type","color")
mostpop2=dcast(mostpop1,type~color)
filename7=paste("most popular",min(ga.data$date),max(ga.data$date),sep = " ")
filename7=paste(filename7,".csv")
write.csv(mostpop2,filename7,row.names = FALSE)

channels=tapply(ga.data.products.final$pageViews,ga.data.products.final$medium,sum)
channels1=data.frame(names(channels),as.numeric(channels))
names(channels)=c("medium","pageViews")
filename8=paste("pageview by medium",min(ga.data$date),max(ga.data$date),sep = " ")
filename8=paste(filename8,".csv")
write.csv(channels1,filename8,row.names = FALSE)

fullnamepv=tapply(ga.data.products.final$pageViews,ga.data.products.final$`full name`,sum)
fullnamepv1=data.frame(names(fullnamepv),as.numeric(fullnamepv))
names(fullnamepv1)=c("product","pageviews")
filename9=paste("pageview by type+color",min(ga.data$date),max(ga.data$date),sep = " ")
filename9=paste(filename9,".csv")
write.csv(fullnamepv1,filename9,row.names = FALSE)

about=tapply(ga.data.about$pageViews,ga.data.about$detail,sum)
about1=data.frame(names(about),as.numeric(about))
names(about1)=c("name","pageviews")
about1=arrange(about1,pageviews)
filename10=paste("about pageview",min(ga.data$date),max(ga.data$date),sep = " ")
filename10=paste(filename10,".csv")
write.csv(about1,filename10,row.names = FALSE)

affiliatepv=tapply(ga.data.ref$pageViews,ga.data.ref$affiliate,sum)
affiliatepv1=data.frame(names(affiliatepv),as.numeric(affiliatepv))
names(affiliatepv1)=c("affiliate","pageviews")
affiliatepv1=arrange(affiliatepv1,pageviews)
affiliatepv1$affiliate=as.character(affiliatepv1$affiliate)
filename11=paste("affiliate pageviews",min(ga.data$date),max(ga.data$date),sep = " ")
filename11=paste(filename11,".csv")
write.csv(affiliatepv1,filename11,row.names = FALSE)

#---------------------prepare the pageview data this year and last year-----------------------------

query.list1 <- Init(start.date = "2017-08-01",
                   end.date = "2017-08-28",
                   metrics = c("ga:pageViews","ga:newUsers"),
                   dimensions = c("ga:date"),
                   sort="ga:date",
                   max.results = 10000,
                   table.id = "ga:65357171")

# Create the Query Builder object so that the query parameters are validated
ga.query1 <- QueryBuilder(query.list1)

# Extract the data and store it in a data-frame
ga.data1 <- GetReportData(ga.query1, token)

query.list2 <- Init(start.date = "2016-08-01",
                    end.date = "2016-08-28",
                    metrics = c("ga:pageViews","ga:newUsers"),
                    dimensions = c("ga:date"),
                    sort="ga:date",
                    max.results = 10000,
                    table.id = "ga:65357171")

# Create the Query Builder object so that the query parameters are validated
ga.query2 <- QueryBuilder(query.list2)

# Extract the data and store it in a data-frame
ga.data2 <- GetReportData(ga.query2, token)

comparedata=cbind(ga.data1,ga.data2$pageViews,ga.data2$newUsers)
names(comparedata)=c("date","pageView this month this year","newUsers this month this year","pageView this month last year","newUsers this month last year")
comparedata$date=formatdata(comparedata$date)
filename12=paste("comparedata",min(ga.data$date),max(ga.data$date),sep = " ")
filename12=paste(filename12,".csv")
write.csv(comparedata,filename12,row.names = FALSE)

#-------------------------------------clean the orders data----------------------------------------
orders=read.csv("C://angelaroi//08.29.17 updated.csv",as.is = TRUE)
library(tidyr)
library(dplyr)
orders=orders%>%separate(Paid.at,c('date','time','somesome'),sep=' ')
orders$date=as.Date(orders$date)
orders1=filter(orders,date>=as.Date("2017/08/01") & date<=as.Date("2017/08/28") & Financial.Status=="paid" & Fulfillment.Status=="fulfilled")
orders1=select(orders1,date,Total,Lineitem.name,Billing.City,Billing.Province,Shipping.Zip)
zip_format=function(x){
  if (nchar(x)==6){
    x=substr(x,2,6)
  }
  if (nchar(x)>6){
    x=substr(x,1,5)
  }
  return(x)
}
orders1$Shipping.Zip=zip_format(orders1$Shipping.Zip)
bag.name.match=c("morning","tote classic","sunday mini","luna","cher tote","eleanor","olivia e","olivia z","madison","jules","anya","anne")
bag.names=c("Morning Cross-Body","Sunday Tote Classic","Sunday Mini","Luna Cross-Body","Cher-Tote","Eleanor Satchel","Olivia E Wallet","Olivia Z Wallet","Madison Backpack","Jules Bucket Bag II","Anya Cross-Body","Anne Shopper")
colors=c("forest green","dusty rose","mustard","powder pink","bordeaux","ivory","black","mud grey","dark green","taupe","light grey","pale pink","creme","scarlet","light gray","mud gray","crème","russet")
create.index=function(x,n){
  r=rep(0,n)
  for(i in 1:n){
    j=grep(toupper(x),toupper(orders1$Lineitem.name)[i])
    if(length(j)!=0){r[i]=1}
  }
  return(r)
}
for (i in 1:length(bag.names)){
  x=bag.name.match[i]
  orders1[,bag.names[i]]=create.index(x,nrow(orders1))
}
for (i in 1:length(colors)){
  x=colors[i]
  orders1[,colors[i]]=create.index(x,nrow(orders1))
}

begin_bag=which(names(orders1)==bag.names[1])
end_bag=which(names(orders1)==bag.names[length(bag.names)])
begin_color=which(names(orders1)==colors[1])
end_color=which(names(orders1)==colors[length(colors)])
index12=NULL
for (i in 1:nrow(orders1)){
  h=sum(orders1[i,begin_bag:end_color])
  if (h!=2){index12=c(index12,i)}
}
orders1=orders1[-index12,]
type=rep("",nrow(orders1))
color=rep("",nrow(orders1))
for (i in 1:nrow(orders1)){
  type[i]=bag.names[which(orders1[i,][begin_bag:end_bag]==1)]
  color[i]=colors[which(orders1[i,][begin_color:end_color]==1)]
}
for (i in 1:length(color)){
  if (color[i]=="light-grey"){color[i]="light-gray"}
  if (color[i]=="mud-grey"){color[i]="mud-gray"}
  if (color[i]=="crème"){color[i]="creme"}
  if (color[i]=="scarlet"){color[i]="red"}
}
orders1=select(orders1,date,Total,Lineitem.name,Billing.City,Billing.Province,Shipping.Zip)
orders1[,'type']=type
orders1[,'color']=color
orders1[,'quantity']=1
orders2=select(orders1,type,color,quantity)
orders3=dcast(select(orders1,type,color,quantity),type+color~quantity)
names(orders3)[3]="quantity"
orders3['full name']=paste(orders3$type,orders3$color,sep=" ")
orders4=orders3%>%separate(col=color,into=c("part1","part2"),sep=" ")
for (i in 1:nrow(orders4)){
  if (is.na(orders4$part2[i])==FALSE){
    orders4$part1[i]=paste(orders4$part1[i],orders4$part2[i],sep="-")
  }
}
orders4=orders4[,-3]
names(orders4)[2]="color"

filename13=paste("sales",min(ga.data$date),max(ga.data$date),sep = " ")
filename13=paste(filename13,".csv")
write.csv(orders4,filename13,row.names = FALSE)

#--------------------------------combine pageviews and orders data------------------------------------------
orders4['full name']=paste(orders4$type,orders4$color,sep=" ")
pview2['full name']=paste(pview2$type,pview2$color,sep=" ")
pview.orders=merge(pview2,orders4,by=intersect('full name',"full name"))
names(pview.orders)[1]="fullname"

filename14=paste("pageviews and orders",min(ga.data$date),max(ga.data$date),sep = " ")
filename14=paste(filename14,".csv")
write.csv(pview.orders,filename14,row.names = FALSE)

#-----------------------------------create content-------------------------------------------------
filenames=data.frame(c(filename1,filename2,filename3,filename4,filename5,filename6,filename7,filename8,filename9,filename10,filename11,filename12,filename13,filename14))
write.csv(filenames,"filenames_August.csv")
