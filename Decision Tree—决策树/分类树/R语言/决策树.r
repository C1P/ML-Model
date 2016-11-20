## 决策树练习
library ("rpart")
library ("rpart.plot")
## rpart(formula, data, weights, subset, na.action = na.rpart, 
##   method,model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)
## formula 输入公式常见 Species~.方法，省略后面所有自变量名称
## na.action 默认删除因变量缺失的观测，而保留自变量缺失的观测
## method 连续性method=“anova”,离散型method=“class”,计数型method=“poisson”,生存分析型method=“exp”
## parms 熵计算方法parms = list (split = "information)
##       Gini系数计算方法parms = list (split = "gini")
## control 控制：先验概率、损失矩阵、CP剪枝的CP值，交叉验证次数：xval = 10
## minsplit 是最小分支节点数，分支节点数大于这个数，决策树继续分类下去，小于就会停止
## minbucket 叶子节点最小样本数
## maxdepth 决策树的最大深度
## 代价复杂度剪枝CCP
##     printcp 函数输出各个节点的CP值
##     plotcp 函数直接画出各个节点CP值，虚线应该是最小xerror+stde
## prune函数，prune(fit, cp=0.01) 剪去CP小于这个数的枝
## predict时，type函数需要选择“class”才会返回常见的分类值，不选的话应该返回的是各个值的可能概率

a <- rpart(Species~.,data = iris,method = "class",
           parms = list(split="gini"))
##画出决策树
##method类型有anova(连续型),poisson(计数型),class(分类),exp(生存)
plot (a,uniform = T,branch = 0,margin = 0.1)
text (a,use.n = T,fancy = T,col = "black")
str (iris$Species)

c <- sample(2,nrow(iris),replace = T,prob = c(0.7,0.3))
train <- iris [c==1,]
test <- iris [c==2,]
##用gini
irist <- rpart(Species~.,data = train,method = "class")
iristr <- predict(irist,test[,-5],type = "class")
 table(test[,5],iristr)
1-(1+3)/36
## 熵 信息增益
irist1 <- rpart(Species~.,data = train,method = "class",
                parms = list(split = "information"))
iristr1 <- predict(irist1,test[,-5],type = "class")
table(test[,5],iristr1)
1-(1+3)/36

##c4.5
library("RWeka")
##RWeka包中J48函数
irisj48tr <- J48(Species~.,data = train)
irisj48 <- predict(irisj48tr,test[,-5],type = "class")
table(test[,5],irisj48)
1-6/36

printcp(irist)
