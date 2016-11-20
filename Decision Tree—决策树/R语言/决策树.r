##决策树练习
library("rpart")
View(iris) 
head(iris)

a<-rpart(Species~.,data=iris,method="class",parms=list(split="gini"))
##画出决策树
##method类型有anova(连续型),poisson(计数型),class(分类),exp(生存)
plot(a,uniform=T,branch=0,margin = 0.1)
text(a,use.n=T,fancy=T,col="black")
str(iris$Species)

##模型验证
c<-sample(2,nrow(iris),replace=T,prob=c(0.7,0.3))
train<-iris[c==1,]
test<-iris[c==2,]
##用gini
irist<-rpart(Species~.,data=train,method="class")
iristr<-predict(irist,test[,-5],type="class")
table(test[,5],iristr)
1-(1+3)/36
## 熵
irist1<-rpart(Species~.,data=train,method="class",parms=list(split="information"))
iristr1<-predict(irist1,test[,-5],type="class")
table(test[,5],iristr1)
1-(1+3)/36

##c4.5
library("RWeka")
##RWeka包中J48函数
irisj48tr<-J48(Species~.,data=train)
irisj48<-predict(irisj48tr,test[,-5],type="class")
table(test[,5],irisj48)
1-6/36

printcp(irist)
