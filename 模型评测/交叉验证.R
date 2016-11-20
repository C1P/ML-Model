library("ggplot2")
a<-iris[51:150,]
ind<-rep(0,nrow(a))
ind[which(a$Species=="versicolor")]<-1
a$Species<-ind
names(a)<-c("a1","a2","a3","a4","a5")
kfc1<-function(x,n){

}


kfc2<-function(x,n,p){
    b<-rep(1:n,each=floor(nrow(x)/n))
   b1<-data.frame(b=b,c=runif(nrow(x)))
   b1<-b1[order(b1$c),]
   b0<<-b1$b
  b3<<-b1$b
  b2<-data.frame(x=x,y=b0[1:nrow(x)])
  xy<<-split(b2,b2$y)
    tpr<-c()
    nfr<-c()
    for(i in 1:n){
        test<-xy[i]
        test<-as.data.frame(test)
        names(test)<-c("a1","a2","a3","a4","a5","a6")
        train<-unsplit(xy[-i],b0[-(which(b0==i))])
        train<-train[-6]
        test1<-as.data.frame(test)
        test1<-test1[1:5]
        names(train)<-c("a1","a2","a3","a4","a5")
        names(test1)<-c("a1","a2","a3","a4","a5")
        formula<-a5~.+a1+a2+a3+a4
        glm1<-glm(formula,data=train,family=binomial(link="logit"),control = list(maxit=500))
        pre<-1/(1+exp(as.numeric(predict(glm1,test1))))
        for(i1 in 1:length(pre)){
            if(pre[i1]>p)
                pre[i1]<-0
            else
                if(pre[i1]<=p)
                    pre[i1]<-1  
        }
        
        tp1<-pre[which(test$a5==1)]
        tpr[i]<-length(which(tp1==1))/length(which(test$a5==1))
        nf1<-pre[which(test$a5==0)]
        nfr[i]<-length(which(nf1==0))/length(which(test$a5==0))
    }
       print(paste0("tpr",mean(tpr),sep = ""))
       print(paste0("nfr",mean(nfr),sep = ""))
       print(summary(glm1))
}


kfc2(a,10,0.5)

########
test<-xy[2]
test<-as.data.frame(test)
names(test)<-c("a1","a2","a3","a4","a5","a6")
train<-unsplit(xy[-2],b0[-(which(b0==2))])
train<-train[-6]
test1<-as.data.frame(test)
test1<-test1[1:5]
names(train)<-c("a1","a2","a3","a4","a5")
names(test1)<-c("a1","a2","a3","a4","a5")
formula<-a5~.+a1+a2+a3+a4
glm1<-glm(formula,data=train,family=binomial(link="logit"))
pre<-predict(glm1,test1)
pre<-as.numeric(pre)
pre<-1/(1+exp(pre))
for(i in 1:10){
    if(pre[i]>0.5)
        pre[i]<-0
    else
        if(pre[i]<=0.5)
          pre[i]<-1  
}

tp1<-pre[which(test$a5==1)]
tpr<-length(which(tp1==1))/length(which(test$a5==1))
nf1<-pre[which(test$a5==0)]
nfr<-length(which(nf1==0))/length(which(test$a5==0))


qplot(seq(-2,2,length=length(pre)),pre,col=factor(test$X1.x.a5))
