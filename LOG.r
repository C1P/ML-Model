library("RMySQL") ##读取Mysql
options(scipen=200)
setwd("d:/data")
con <- dbConnect(MySQL(),host="127.0.0.1",dbname="zx",user="root",password="root")
dbSendQuery(con,'SET NAMES gbk') 
a<-dbGetQuery(con, "SELECT ID,F_companyname FROM w_ssgsqylb")
dbDisconnect(con)
a1<-read.csv("w_ssgs_financindex.csv",header=T)
library("ggplot2")
names(a1)
b1<-a1[1:2]
names(a1)[2]<-"ID"
names(b1)[2]<-"ID"
b<-merge(b1,a,by=c("ID"))


dim(b)
b2<-grep("ST",b$F_companyname)

d<-rep(0,42636)
d[b2]<-1
length(d)

a2<-cbind(a1,d)
length(b2)
x1<-a2[which(a2$d==0),]
index<-sample(c(1,0),nrow(x1),replace=T,prob=c(0.21,0.79))
xx<-x1[which(index==1),]
x2<-rbind(xx,a2[which(a2$d==1),])
x3<-x2[-c(1,2,22,14,17)]
x4<-na.omit(x3)
x4[1:17]<-scale(x4[1:17])
mean(x4$d)
st<-sample(nrow(x4),round(nrow(x4)*0.7))
train<-x4[st,]
test<-x4[-st,]
nam<-paste0("a",1:length(names(x4)),sep="")
names(train)<-nam
names(test)<-nam
names(test)
fnam<-paste0("a",1:length(names(x4)),sep="",collapse="+")
formula<-a18~a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17
glm1<-glm(formula,data=train,family=binomial(link="logit"))
summary(glm1)
predict1<-predict(glm1,type="response")
qplot(seq(-2,2,length=length(predict1)),sort(predict1))

round(apply(x3,2,function(x){mean(is.na(x))}),3)
x2<-na.omit(x1)
cor(x2)
x2[1:16]<-scale(x2[1:16])
x2<-x2[-c(4,5,6,7,14,17)]
x2<-x2[-7]


##x3<-x2[x2$d==1,]
##write.csv(x3,"d:/data/x3.csv")
###
st<-sample(nrow(x2),round(nrow(x2)*0.7))
train<-x2[st,]
test<-x2[-st,]
nam<-paste0("a",1:length(names(x2)),sep="")
names(train)<-nam
names(test)<-nam
formula<-a12~a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11
glm1<-glm(formula,data=train,family=binomial(link="logit"))
summary(glm1)


names(x1)[20]<-"失信判别"
x1<-x1[-4]
x2<-na.omit(x1)
mean(x2$d)
x2<-x2[-1]
x2[1:18]<-scale(x2[1:18])
dim(x2)
names(x2)

##

st<-sample(nrow(x2),round(nrow(x2)*0.7))
train<-x2[st,]
test<-x2[-st,]
nam<-paste0("a",1:18,sep="")
names(train)<-nam
names(test)<-nam
trainc<-train[c(4,6,10,18)]
formula<-a18~a4+a6+a10
glm1<-glm(formula,data=train,family=binomial(link="logit"))
summary(glm1)
predict1<-predict(glm1,type="response")
qplot(seq(-2,2,length=length(predict1)),sort(predict1))
##

formula<-a18~a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17
glm1<-glm(formula,data=train,family=binomial(link="logit"))
summary(glm1)
predict1<-predict(glm1,type="response")
qplot(seq(-2,2,length=length(predict1)),sort(predict1))

trainc<-train[-c(3,7:8,11:17)]

formulac<-a18~a1+a2+a4+a5+a6+a9+a10
glm2<-glm(formulac,data=trainc,family=binomial(link="logit"))
predict2<-predict(glm2,type="response")
qplot(seq(-2,2,length=length(predict2)),sort(predict2))
