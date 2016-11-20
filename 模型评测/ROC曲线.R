
##
a<-iris[51:150,]
names(a)<-c("sp","sw","pl","pw","s")

ind<-rep(c(1,0),each=50)
ind[which(a$s=="versicolor")]<-1
a$s<-ind
###
lgst<-glm(s~.,data=a,family = binomial(link="logit"))
pre<-predict(lgst,type="response")
###

a1<-pre;
b<-a$s

roc<-function(a,b){
droc<-data.frame(a,b)
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
}
###
roc(pre,a$s)

