library("RMySQL") ##读取Mysql
library("ggplot2")
setwd("d:/data")
con <- dbConnect(MySQL(),host="127.0.0.1",dbname="zx",user="root",password="root")
dbSendQuery(con,'SET NAMES gbk') 
a<-dbGetQuery(con, "SELECT * FROM w_ssgs_balance_wind")
#head(a)
#class(a)
b1<-grep("ST",a$Name)
b2<-rep(0,nrow(a))
b2[b1]<-1
a$p<-b2

#mean(a$p)
#sum(a$p)
b3<-a[which(a$p==0),]
b4<-a[which(a$p==1),]
#nrow(b3)
ind<-sample(85827,1000)
b3<-b3[ind,]
b4<-rbind(b3,b4)
#class(b4$ynndqdfldfz)
b5<-sapply(b4,function(x){length(which(x==0))/nrow(b4)})
b6<-apply(b4,2,function(x){length(which(x==0))})
#length(which(b4$ynndqdfldfz==0))
b5<-sort(b5)
b6<-b4[c("hbzj","jyxjrzc","yspj","yszk","qtysk","yfzk","ysgl","yslx","ch","ynndqdfldzc","qtldzc","ldzchj","kgcsjrzc","cyzdqtz","p")]
b6$p<-b4$p
# paste0("a",1:length(names(b6)),sep="",collapse="+")
names(b6)<-paste0("a",1:length(names(b6)),sep="")

head(b6$a4)
indh<-


fomula<-a15~a4+a5+a6+a8+a9+a10+a13
glm1<-glm(fomula,data=b6,family=binomial(link="logit"))
summary(glm1)
prd<-predict(glm1,type="response")

qplot(seq(-10,10,length=length(prd)),sort(prd),col=factor(b6$a15))
h0<-prd[b6$a15==0]
h1<-prd[b6$a15==1]

hist(h0)
hist(h1)
prd1<-prd
prd1[prd1>0.482]<-1
prd1[prd1<=0.482]<-0
table(prd1,b6$a15)