library(rpart)
library(rpart.plot)
library(tree)
library(mltools)
library(MASS)
library(party)
library(readxl)
Q1 <- read_excel("Desktop/University of Alberta/Stat 441/assignment 4/Q1.xlsx")
data = Q1
data$Sex = factor(data$Sex, levels = c("Female","male"), labels = c(1,2))
data$Sex = factor(data$Sex)
Sex = data$Sex
x1 = data$x1
x2 = data$x2
x3 = data$x3
gini = function(x) 1-sum(x*x)

#build a tree via calculating impurities

snake_tr=rpart(Sex~x1 ,data=data)
plot(snake_tr,margin=0.1)
text(snake_tr,use.n=T,cex=1.3)
gini(c(1/2,1/2))-43/66*gini(c(28/43,15/43))-23/66*gini(c(9/23,14/23))

snake_tr=rpart(Sex~x2 ,data=data)
plot(snake_tr,margin=0.1)
text(snake_tr,use.n=T,cex=1.3)
gini(c(1/2,1/2))-35/66*gini(c(26/35,9/35))-31/66*gini(c(11/31,20/31))

snake_tr=rpart(Sex~x3 ,data=data)
plot(snake_tr,margin=0.1)
text(snake_tr,use.n=T,cex=1.3)
gini(c(1/2,1/2))-51/66*gini(c(37/51,14/51))-15/66*gini(c(0,1))



#build a tree( method 1)

snake_tr=rpart(Sex~x1+x2+x3 ,data=data,minsplit=1,cp=0.00001)
rpart.plot(snake_tr, branch = 0.5 )
text(snake_tr,use.n=T,cex=1)
printcp(snake_tr)
plotcp(snake_tr)

#pruning(method 1)

snake_tr=rpart(Sex~x1+x2+x3 ,data=data,minsplit=1,cp=0.069)
printcp(snake_tr)
rpart.plot(snake_tr,  main = "Snake")
text(final_tree,use.n=T,cex=1)

#build a tree(method 2)
snake_tr=tree(Sex~x1+x2+x3, data=data, control=tree.control(nobs=nrow(data),minsize=1,mindev=0), split="gini") 
plot(snake_tr,  main = "Snake")
text(snake_tr,cex=1)
tree = cv.tree(snake_tr)

#pruning(method 2)

plot(tree,type="b", main = "Snake")
plot(prune.misclass(snake_tr,best = 4))
text(prune.misclass(snake_tr,best = 4),main = "Snake")


xer=rep(0,5)
xvr=rep(0,5)

#pruning(method 3)

for (nn in (1:20)) {
  snake_tr=rpart(Sex~.,data=data,minsplit=1)
  xer=xer+snake_tr$cptable[,4]
  xvr=xvr+snake_tr$cptable[,5]^2
  }
snake_tr$cptable[,5]=sqrt(xvr/20)
snake_tr$cptable[,4]=xer/20
plotcp(snake_tr,minline=FALSE)
snake_tr=rpart(Sex~.,data=data,minsplit=1,cp=0.069)
plot(snake_tr,margin=0.1)
text(snake_tr,use.n=TRUE)

#logistic

Q1 <- read_excel("Desktop/University of Alberta/Stat 441/assignment 4/Q1.xlsx")
data = Q1
data = data.frame(data)
Sex = factor(data$Sex, levels = c("Female","male"), labels = c(0,1))
snake_glm = lda(Sex~x1+x2+x3, data = data, family=binomial)
summary(snake_glm)
data$Sex = as.integer(Sex)
Sex
#dropping factor x1 
s_sam = sammon(dist(data[,3:4]) )
plot(s_sam[[1]],pch=1*data[,1], main = "Sex of Snake")
#lda
s_sam
snake_lda = lda(Sex~., data = data)
snake_lda
wrong= data[,1] != predict(snake_lda)$class
points(s_sam[[1]][wrong,],pch=4,cex=3)
plot(snake_lda, main = "Sex of Snake")




