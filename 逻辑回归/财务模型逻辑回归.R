setwd("d:/data")
a1<-read.csv("w_ssgs_financindex.csv",header=T)
summary(a1)
library("ggplot2")
a1$F_YEAR<-NULL
a1$F_jstrbl<-NULL
a1$企业ID<-NULL
a2<-na.omit(a1)
#summary(a2)
#length(which(a2$速动比率...<72))/nrow(a1)
#length(which(a2$资产负债率...>90))/nrow(a1)
#length(which(a2$净资产收益率...<1))/nrow(a1)
#length(which(a2$营业利润率...<1))/nrow(a1)
#length(which(a2$营业收入增长率...<0))/nrow(a1)
#length(which(a2$总资产增长率...<0))/nrow(a1)
b1<-rep(0,length=nrow(a2))
b1[which(a2$速动比率...<247.42&a2$资产负债率...>45.305&a2$利息保障倍数.倍.<52.9&a2$带息负债比率...>33.3&a2$盈余现金保障倍数.倍.<0&a2$营业利润增长率... <211.3&a2$总资产周转率.次.<0.7)]<-1

#length(which(a2$资产负债率...>70&a2$盈余现金保障倍数.倍.<1&a2$营业利润增长率... <10&a2$总资产周转率.次.<0.7))


a3<-cbind(a2,b1)
a4<-a3[which(a3$b1==0),]

ind<-sample(c(1,0),37113,replace=T,prob=c(0.1,0.9))
a4<-a4[ind==1,]
a4<-rbind(a4,a3[which(a3$b1==1),])
a5<-a4[,c(1,3,4,5,9,13,19,20)]
names(a5)<-c("a1","a2","a3","a4","a5","a6","a7","a8")
idx<-sample(nrow(a5),nrow(a5*0.7))
train<-a5[idx,]
test<-a5[idx,]
formula<-a8~a1+a2+a3+a5+a6+a4+a7-1
glm1<-glm(formula,data=train,family=binomial(link=("logit")))
summary(glm1)

prd<-predict(glm1,test[,1:7])

options(scipen=200)
aaa<-glm1$coefficients

##roc曲线
droc<-data.frame(prd,test$a8)
names(droc)<-c("x","y")
droc<-droc[order(droc$x),]
pfr<-c()
ptr<-c()
th<-droc$x
n<-nrow(droc)
for(i in 1:n){
    x1<-droc
    pt<-sum(x1$x>th[i]&x1$y==1)
    pf<-sum(x1$x>th[i]&x1$y==0)
    nt<-sum(x1$x<th[i]&x1$y==1)
    nf<-sum(x1$x<th[i]&x1$y==0)
    pfr[i]<-pf/(pf+nf)
    ptr[i]<-pt/(pt+nt) 
}
plot(pfr,ptr)
qplot(pfr,ptr)+geom_point(aes(pl))
pl<-seq(0,1,length=length(ptr))

ggplot(plot1)+geom_point(aes(x=pfr,y=ptr,col="red"))+geom_line(aes(pl,pl),col="blue")+ggtitle("ROC")+xlab("PFR")+ylab("PTR")
plot1<-data.frame(pfr,ptr,pl)





roc(prd,test$a8)


prd[prd>0.3]<-1
prd[prd<=0.3]<-0
table(prd,test$a8)


qplot(seq(-2,2,length=length(prd)),sort(prd))
qplot(seq(-2,2,length=length(prd)),sort(a5$a1),ylim=c(0,100))

plot.roc(formula,train,col="1")


library(pROC)
modelroc <- roc(test$a8,prd)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)