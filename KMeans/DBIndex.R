##DBIndex<-function(x,n,m){
    x1<-as.data.frame(x)
    DBI<-c()
    DIm<-c()
    TTW<-c()
    BET<-c()
    TTW2<-c()
    BET2<-c()
    for(ij in n){
        for(i1 in 1:m){
    cluster1<-kmeans(x1,centers=ij)
    a1<-split.data.frame(x1,cluster1$cluster)
    k<-c()
    c<-c()
    ax<-c()
    bx<-c()
    Di<-c()
    mk<-c()
    R11<-c()
    for(i in 1:ij){
        ax<-as.data.frame(a1[i])
        bx<-cluster1$centers[i,]
        for(j in 1:ncol(x1)){
            c<-sum(abs((ax[[j]]-bx[j])))/nrow(ax)##在计算
        }
        k[i]<-sum(c)
    }

    Rij<-as.matrix(dist(cluster1$centers))##计算公式中的Rij
    for(i in 1:ij){
        mk<-cbind(mk,(k+k[i]))
    }
    
    mk1<-mk/Rij
    for(i in 1:ij){
        mkr<-mk1[i,]
        R11<-c(max(mkr[which(!(is.infinite(mkr)))]),R11)
    }
    DIm[i1]<-sum(R11)
    TTW[i1]<-cluster1$tot.withinss
    BET[i1]<-cluster1$betweenss
    }
    DBI<-c(mean(DIm),DBI)
    TTW2<-c(mean(TTW),TTW2)
    BET2<-c(mean(BET),BET2)
    }
    print(DBI)
    par(mfrow=c(1,1))
    plot(n,DBI,type="l",ylim=c(0,0.5))
##}


DBIndex(a,2:10,50)
DBIndex1(a,8)
DBIndex3(a,2:8,10)

a<-iris[1:4]

##使用DBIndex函数 ##n是分类的数量，可以是一个数也可以是个连续数值 如2：10
##x是输入的数据集
##kmeans因为中心点初始是随机的，所以设置m是重复实验多少次
####
DBIndex<-function(x,n,m){
    x1<-as.data.frame(x)
    DBI<-c()
    DIm<-c()
    TTW<-c()
    BET<-c()
    TTW2<-c()
    BET2<-c()
    for(ij in n){
        for(i1 in 1:m){
        cluster1<-kmeans(x1,centers=ij)
        a1<-split.data.frame(x1,cluster1$cluster)
        k<-c()
        c<-c()
        ax<-c()
        bx<-c()
        Di<-c()
        mk<-c()
        R11<-c()
        for(i in 1:ij){
            ax<-as.data.frame(a1[i])
            bx<-cluster1$centers[i,]
            for(j in 1:nrow(ax)){
                c<-sqrt(sum((ax[j,]-bx)^2)/nrow(ax))##在计算
            }
            k[i]<-sum(c)
        }
        
        Rij<-as.matrix(dist(cluster1$centers))##计算公式中的Rij
        for(i in 1:ij){
            mk<-cbind(mk,(k+k[i]))
        }
        
        mk1<-mk/Rij
        for(i in 1:ij){
            mkr<-mk1[i,]
            R11<-c(max(mkr[which(!(is.infinite(mkr)))]),R11)
        }
        DIm<-c(DIm,sum(R11))
        }
        DBI<-c(DBI,sum(DIm)/m)
    }
    print(DBI)
    par(mfrow=c(1,1))
    plot(n,DBI,type="l")
}
###
bb<-(dist(cbind(as.numeric(ax[1,]),bx)))

##
DBIndex1<-function(x,n){
    x1<-as.data.frame(x)
        cluster1<-kmeans(x1,centers=n)
        a1<-split.data.frame(x1,cluster1$cluster)
        k<-c()
        c<-c()
        ax<-c()
        bx<-c()
        Di<-c()
        for(i in 1:n){
            ax<-as.data.frame(a1[i])
            bx<-cluster1$centers[i,]
            for(j in 1:ncol(x1)){
                c<-sum(abs(ax[[j]]-bx[j]))/nrow(ax)##在计算
            }
            k[i]<-sum(c)
        }
        
        Rij<-as.matrix(dist(cluster1$centers))##计算公式中的Rij
        mk<-c()
        for(i in 1:n){
            mk<-cbind(mk,(k+k[i]))
        }
        
        mk1<-mk/Rij
        R11<-c()
        for(i in 1:n){
            mkr<-mk1[i,]
            R11<-c(max(mkr[which(!(is.infinite(mkr)))]),R11)
        }
        sum(R11)
        return(sum(R11))
}

##
x1<-as.data.frame(a)
cluster1<-kmeans(x1,centers=2)
a1<-split.data.frame(x1,cluster1$cluster)
k<-c()
c<-c()
ax<-c()
bx<-c()
Di<-c()
mk<-c()
R11<-c()
for(i in 1:2){
    ax<-as.data.frame(a1[i])
    bx<-cluster1$centers[i,]
    for(j in 1:nrow(ax)){
        c<-sqrt(sum((ax[j,]-bx)^2)/nrow(ax))##在计算
    }
    k[i]<-sum(c)
}

Rij<-as.matrix(dist(cluster1$centers))##计算公式中的Rij
for(i in 1:2){
    mk<-cbind(mk,(k+k[i]))
}

mk1<-mk/Rij
for(i in 1:2){
    mkr<-mk1[i,]
    R11<-c(max(mkr[which(!(is.infinite(mkr)))]),R11)
}
DIm<-c(DIm,sum(R11))
