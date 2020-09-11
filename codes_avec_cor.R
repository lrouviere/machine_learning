## ----message=FALSE, warning=FALSE, echo=FALSE-------------
library(tidyverse)
theme_set(theme_classic())

## ---------------------------------------------------------
n <- 2000
set.seed(12345)
X1 <- runif(n)
X2 <- runif(n)
set.seed(9012)
R1 <- X1<=0.25
R2 <- (X1>0.25 & X2>=0.75)
R3 <- (X1>0.25 & X2<0.75)
Y <- rep(0,n)
Y[R1] <- rbinom(sum(R1),1,0.25)
Y[R2] <- rbinom(sum(R2),1,0.25)
Y[R3] <- rbinom(sum(R3),1,0.75)
donnees <- data.frame(X1,X2,Y)
donnees$Y <- as.factor(donnees$Y)
ggplot(donnees)+aes(x=X1,y=X2,color=Y)+geom_point()

## ----teacher=correct--------------------------------------
set.seed(234)
indapp <- sample(nrow(donnees),1500)
dapp <- donnees[indapp,]
dtest <- donnees[-indapp,]

## ----teacher=correct--------------------------------------
library(class)
knn3 <- knn(dapp[,1:2],dtest[,1:2],cl=dapp$Y,k=3)

## ----teacher=correct--------------------------------------
mean(knn3!=dtest$Y)

## ----teacher=correct--------------------------------------
prev_cv <- knn.cv(donnees[,-3],cl=donnees$Y,k=3)

## ----teacher=correct--------------------------------------
mean(prev_cv!=donnees$Y)

## ---------------------------------------------------------
K_cand <- seq(1,500,by=20)

## ----hold-out-selk,indent='        ',teacher=correct,cache=2----
err.ho <- rep(0,length(K_cand))
for (i in 1:length(K_cand)){
  knni <- knn(dapp[,1:2],dtest[,1:2],cl=dapp$Y,k=K_cand[i])
  err.ho[i] <- mean(knni!=dtest$Y)
}

## ----indent='        ',teacher=correct--------------------
K_cand[which.min(err.ho)]

## ----loo-selk,indent='        ',teacher=correct,cache=2----
err.loo <- rep(0,length(K_cand))
for (i in 1:length(K_cand)){
  knni <- knn.cv(donnees[,-3],cl=donnees$Y,k=K_cand[i])
  err.loo[i] <- mean(knni!=donnees$Y)
} 
K_cand[which.min(err.loo)]

## ---------------------------------------------------------
set.seed(2345)
blocs <- caret::createFolds(1:nrow(donnees),10,returnTrain = TRUE)

## ----cv-selk,teacher=correct,cache=2----------------------
err.cv <- rep(0,length(K_cand))
prev <- donnees$Y
for (i in 1:length(K_cand)){
  for (j in 1:length(blocs)){
train <- donnees[blocs[[j]],]
test <- donnees[-blocs[[j]],]
prev[-blocs[[j]]] <- knn(train[,1:2],test[,1:2],cl=train$Y,k=K_cand[i])
  }
  err.cv[i] <- mean(prev!=donnees$Y)
}
K_cand[which.min(err.cv)]

## ----caret-ho1,cache=2------------------------------------
library(caret)
set.seed(321)
ctrl1 <- trainControl(method="LGOCV",number=1)
KK <- data.frame(k=K_cand)
caret.ho <- train(Y~.,data=donnees,method="knn",trControl=ctrl1,tuneGrid=KK)
caret.ho
plot(caret.ho)

## ----caret-ho2,teacher=correct,cache=2--------------------
ctrl2 <- trainControl(method="LGOCV",number=1,index=list(indapp))
caret.ho2 <- train(Y~.,data=donnees,method="knn",trControl=ctrl2,tuneGrid=KK)
caret.ho2$bestTune

## ----caret-loo,teacher=correct,cache=2--------------------
ctrl3 <- trainControl(method="LOOCV",number=1)
caret.loo <- train(Y~.,data=donnees,method="knn",trControl=ctrl3,tuneGrid=KK)
caret.loo$bestTune

## ----caret-cv,teacher=correct,cache=2---------------------
ctrl4 <- trainControl(method="cv",index=blocs)
caret.cv <- train(Y~.,data=donnees,method="knn",trControl=ctrl4,tuneGrid=KK)
caret.cv$bestTune

## ----caret-parallele,cache=2------------------------------
library(doParallel)
cl <- makePSOCKcluster(1)
registerDoParallel(cl)
system.time(ee3 <- train(Y~.,data=donnees,method="knn",trControl=ctrl4,tuneGrid=KK))
stopCluster(cl)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
system.time(ee3 <- train(Y~.,data=donnees,method="knn",trControl=ctrl4,tuneGrid=KK))
stopCluster(cl)

## ----caret-ho-rep,eval=FALSE------------------------------
#  ctrl <- trainControl(method="LGOCV",number=5)
#  caret.ho.rep <- train(Y~.,data=donnees,method="knn",trControl=ctrl,tuneGrid=KK)

## ----caret-cv-rep,eval=FALSE------------------------------
#  ctrl <- trainControl(method="repeatedcv",repeats=5)
#  caret.ho.rep <- train(Y~.,data=donnees,method="knn",trControl=ctrl,tuneGrid=KK)

## ----caret-auc,cache=2------------------------------------
donnees1 <- donnees
names(donnees1)[3] <- "Class"
levels(donnees1$Class) <- c("G0","G1")
ctrl <- trainControl(method="LGOCV",number=1,classProbs=TRUE,summary=twoClassSummary)
caret.auc <- train(Class~.,data=donnees1,method="knn",trControl=ctrl,tuneGrid=KK,metric="ROC")
caret.auc

## ---------------------------------------------------------
n <- 20
set.seed(123)
X1 <- scale(runif(n))
set.seed(567)
X2 <- scale(runif(n))
Y <- rep(-1,n)
Y[X1>X2] <- 1
Y <- as.factor(Y)
donnees <- data.frame(X1=X1,X2=X2,Y=Y)
p <- ggplot(donnees)+aes(x=X2,y=X1,color=Y)+geom_point()
p

## ---------------------------------------------------------
library(e1071)
mod.svm <- svm(Y~.,data=donnees,kernel="linear",cost=10000000000)

## ----teacher=correct--------------------------------------
ind.svm <- mod.svm$index
sv <- donnees %>% slice(ind.svm)
sv
p1 <- p+geom_point(data=sv,aes(x=X2,y=X1),color="blue",size=2)

## ----teacher=correct--------------------------------------
sv1 <- sv[,2:1]
b <- (sv1[1,2]-sv1[2,2])/(sv1[1,1]-sv1[2,1])
a <- sv1[1,2]-b*sv1[1,1]
a1 <- sv1[3,2]-b*sv1[3,1]
p1+geom_abline(intercept = c(a,a1),slope=b,col="blue",size=1)

## ----teacher=correct--------------------------------------
plot(mod.svm,data=donnees,grid=250)

## ----teacher=correct--------------------------------------
w <- apply(mod.svm$coefs*donnees[mod.svm$index,1:2],2,sum)
w
b <- -mod.svm$rho
b

## ----teacher=correct--------------------------------------
newX <- data.frame(X1=-0.5,X2=0.5)
sum(w*newX)+b

## ----teacher=correct--------------------------------------
predict(mod.svm,newX,decision.values = TRUE)

## ----teacher=correct--------------------------------------
mod.svm1 <- svm(Y~.,data=donnees,kernel="linear",cost=10000000000,probability=TRUE)
predict(mod.svm1,newX,decision.values=TRUE,probability=TRUE)

## ----teacher=correct--------------------------------------
score.newX <- sum(w*newX)+b
1/(1+exp(-(mod.svm1$probB+mod.svm1$probA*score.newX)))

## ---------------------------------------------------------
n <- 1000
set.seed(1234)
df <- as.data.frame(matrix(runif(2*n),ncol=2))
df1 <- df %>% filter(V1<=V2)%>% mutate(Y=rbinom(nrow(.),1,0.95))
df2 <- df %>% filter(V1>V2)%>% mutate(Y=rbinom(nrow(.),1,0.05))
df3 <- bind_rows(df1,df2) %>% mutate(Y=as.factor(Y))
ggplot(df3)+aes(x=V2,y=V1,color=Y)+geom_point()+
  scale_color_manual(values=c("#FFFFC8", "#7D0025"))+
  theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## ----teacher=correct--------------------------------------
mod.svm1 <- svm(Y~.,data=df3,kernel="linear",cost=0.000001)
mod.svm2 <- svm(Y~.,data=df3,kernel="linear",cost=0.1)
mod.svm3 <- svm(Y~.,data=df3,kernel="linear",cost=5)

## ----teacher=correct--------------------------------------
mod.svm1$nSV
mod.svm2$nSV
mod.svm3$nSV

## ---------------------------------------------------------
n <- 500
set.seed(13)
X <- matrix(runif(n*2,-2,2),ncol=2) %>% as.data.frame()
Y <- rep(0,n)
cond <- (X$V1^2+X$V2^2)<=2.8
Y[cond] <- rbinom(sum(cond),1,0.9)
Y[!cond] <- rbinom(sum(!cond),1,0.1)
df <- X %>% mutate(Y=as.factor(Y))
ggplot(df)+aes(x=V2,y=V1,color=Y)+geom_point()+theme_classic()

## ----teacher=correct--------------------------------------
mod.svm0 <- svm(Y~.,data=df,kernel="linear",cost=1)
plot(mod.svm0,df,grid=250)

## ---------------------------------------------------------
mod.svm1 <- svm(Y~.,data=df,kernel="radial",gamma=1,cost=1)
plot(mod.svm1,df,grid=250)

## ----svm-cv-tune,teacher=correct,cache=2------------------
set.seed(1234)
tune.out <- tune(svm,Y~.,data=df,kernel="radial",
             ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

## ----svm-cv-caret,teacher=correct,cache=2-----------------
C <- c(0.001,0.01,1,10,100,1000)
sigma <- c(0.5,1,2,3,4)
gr <- expand.grid(C=C,sigma=sigma)
ctrl <- trainControl(method="cv")
res.caret1 <- train(Y~.,data=df,method="svmRadial",trControl=ctrl,tuneGrid=gr,prob.model=TRUE)
res.caret1

## ----svm-repcv-caret,teacher=correct,cache=2--------------
library(doParallel) ## pour paralléliser
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
set.seed(12345)
ctrl <- trainControl(method="repeatedcv",number=10,repeats=5)
res.caret2 <- train(Y~.,data=df,method="svmRadial",trControl=ctrl,tuneGrid=gr,prob.model=TRUE)
on.exit(stopCluster(cl))
res.caret2

## ----teacher=correct--------------------------------------
library(kernlab)
C.opt <- res.caret2$bestTune$C
sigma.opt <- res.caret2$bestTune$sigma
svm.sel <- ksvm(Y~.,data=df,kernel="rbfdot",kpar=list(sigma=sigma.opt),C=C.opt)
plot(svm.sel,data=df)

## ----echo=FALSE,purl=FALSE,teacher=correct----------------
newX <- data.frame(X1=1,X2=-0.5,X3=-1)
sum(w*newX)+b

## ---------------------------------------------------------
n <- 50
set.seed(1234)
X <- runif(n)
set.seed(5678)
Y <- 1*X*(X<=0.6)+(-1*X+3.2)*(X>0.6)+rnorm(n,sd=0.1)
data1 <- data.frame(X,Y)
ggplot(data1)+aes(x=X,y=Y)+geom_point()

## ----teacher=correct--------------------------------------
library(rpart)
tree <- rpart(Y~X,data=data1)

## ----teacher=correct--------------------------------------
library(rpart.plot)
prp(tree)
rpart.plot(tree)

## ----teacher=correct--------------------------------------
df1 <- data.frame(x=c(0,0.58),xend=c(0.58,1),y=c(0.31,2.41),yend=c(0.31,2.41))
ggplot(data1)+aes(x=X,y=Y)+geom_point()+geom_vline(xintercept = 0.58,size=1,color="blue")+
  geom_segment(data=df1,aes(x=x,y=y,xend=xend,yend=yend),size=1,color="red")

## ---------------------------------------------------------
n <- 50
set.seed(12345)
X1 <- runif(n)
set.seed(5678)
X2 <- runif(n)
Y <- rep(0,n)
set.seed(54321)
Y[X1<=0.45] <- rbinom(sum(X1<=0.45),1,0.85)
set.seed(52432)
Y[X1>0.45] <- rbinom(sum(X1>0.45),1,0.15)
data2 <- data.frame(X1,X2,Y)
ggplot(data2)+aes(x=X1,y=X2,color=Y)+geom_point(size=2)+scale_x_continuous(name="")+
  scale_y_continuous(name="")+theme_classic()

## ----teacher=correct--------------------------------------
tree <- rpart(Y~.,data=data2)
rpart.plot(tree)

## ----teacher=correct--------------------------------------
data2$Y <- as.factor(data2$Y)
tree <- rpart(Y~.,data=data2)
rpart.plot(tree)

## ----teacher=correct--------------------------------------
ggplot(data2)+aes(x=X1,y=X2,color=Y,shape=Y)+geom_point(size=2)+
  theme_classic()+geom_vline(xintercept = 0.44,size=1,color="blue")

## ---------------------------------------------------------
n <- 100
X <- factor(rep(c("A","B","C","D"),n))
set.seed(1234)
Y[X=="A"] <- rbinom(sum(X=="A"),1,0.9)
Y[X=="B"] <- rbinom(sum(X=="B"),1,0.25)
Y[X=="C"] <- rbinom(sum(X=="C"),1,0.8)
Y[X=="D"] <- rbinom(sum(X=="D"),1,0.2)
Y <- as.factor(Y)
data3 <- data.frame(X,Y)

## ----teacher=correct--------------------------------------
tree3 <- rpart(Y~.,data=data3)
rpart.plot(tree3)

## ---------------------------------------------------------
library(ISLR)
data(Carseats)
summary(Carseats)

## ----teacher=correct--------------------------------------
tree <- rpart(Sales~.,data=Carseats)
rpart.plot(tree)

## ----teacher=correct--------------------------------------
printcp(tree)

## ----teacher=correct--------------------------------------
Carseats %>% mutate(fitted=predict(tree)) %>% 
  summarise(MSE=mean((fitted-Sales)^2)/mean((Sales-mean(Sales))^2))
#mean((predict(tree)-Carseats$Sales)^2)/mean((Carseats$Sales-mean(Carseats$Sales))^2)

## ----teacher=correct,max.height='500px'-------------------
set.seed(123)
tree1 <- rpart(Sales~.,data=Carseats,cp=0.00001,minsplit=2)
printcp(tree1)

## ----teacher=correct--------------------------------------
plotcp(tree1)

## ----teacher=correct--------------------------------------
cp_opt <- tree1$cptable %>% as.data.frame() %>% filter(xerror==min(xerror)) %>% dplyr::select(CP) %>% as.numeric()
cp_opt

## ----teacher=correct--------------------------------------
tree_opt <- prune(tree,cp=cp_opt)
rpart.plot(tree_opt)

## ----teacher=correct--------------------------------------
library(visNetwork)
visTree(tree_opt)

## ----eval=FALSE,echo=correct------------------------------
#  visTreeEditor(Carseats)

## ---------------------------------------------------------
new_ind <- Carseats %>% slice(3,58,185,218) %>% select(-Sales)
new_ind

## ----teacher=correct--------------------------------------
predict(tree_opt,newdata=new_ind)

## ----teacher=correct--------------------------------------
n.train <- 250
set.seed(1234)
perm <- sample(nrow(Carseats))
train <- Carseats[perm[1:n.train],]
test <- Carseats[-perm[1:n.train],]

## ----teacher=correct--------------------------------------
set.seed(4321)
tree <- rpart(Sales~.,data=train,cp=0.000001,minsplit=2)

## ----echo=!correct,eval=FALSE-----------------------------
#  set.seed(4321)
#  tree <- rpart(Sales~.,data=train,cp=0.000001,minsplit=2)

## ----teacher=correct,max.height='500px'-------------------
printcp(tree)

## ----teacher=correct--------------------------------------
simple.tree <- prune(tree,cp=0.1)
large.tree <- prune(tree,cp=1e-6)
#cp_opt <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
cp_opt <- tree$cptable %>% as.data.frame() %>% filter(xerror==min(xerror)) %>% dplyr::select(CP) %>% as.numeric()
opt.tree <- prune(tree,cp=cp_opt)

## ----teacher=correct--------------------------------------
data.prev <- data.frame(simple=predict(simple.tree,newdata = test),
                    large=predict(large.tree,newdata = test),
                    opt=predict(opt.tree,newdata = test),
                    obs=test$Sale)

## ----teacher=correct--------------------------------------
data.prev %>% summarise_at(1:3,~mean((obs-.)^2))

## ----teacher=correct--------------------------------------
library(caret)
K <- 10
set.seed(1234)
kfolds <- createFolds(1:nrow(Carseats),k=K)

## ----teacher=correct--------------------------------------
prev <- matrix(0,nrow=nrow(Carseats),ncol=3) %>% as.data.frame()
names(prev) <- c("simple","large","opt")
for (j in 1:K){
  train <- Carseats[-kfolds[[j]],]
  test <- Carseats[kfolds[[j]],]
  tree <- rpart(Sales~.,data=train,minsplit=2,cp=1e-9)
  simple <- prune(tree,cp=tree$cptable[2,1])
  large <- prune(tree,cp=1e-9)
  cp_opt <- tree$cptable %>% as.data.frame() %>% filter(xerror==min(xerror)) %>% 
dplyr::select(CP) %>% as.numeric()
  opt <- prune(tree,cp=cp_opt) 
  prev[kfolds[[j]],1] <- predict(simple,newdata=test)
  prev[kfolds[[j]],2] <- predict(large,newdata=test)
  prev[kfolds[[j]],3] <- predict(opt,newdata=test)
}
prev %>% mutate(obs=Carseats$Sales) %>% summarize_at(1:3,~mean((obs-.)^2))

## ---------------------------------------------------------
High <- ifelse(Carseats$Sales<=8,"No","Yes")
data1 <- Carseats %>% dplyr::select(-Sales) %>% mutate(High)

## ----teacher=correct--------------------------------------
set.seed(321)
tree <- rpart(High~.,data=data1)
rpart.plot(tree)

## ---------------------------------------------------------
tree1 <- rpart(High~.,data=data1,parms=list(split="information"))
tree1$parms

## ----teacher=correct--------------------------------------
printcp(tree)

## ----teacher=correct--------------------------------------
data1 %>% mutate(fitted=predict(tree,type="class")) %>% 
  summarise(MC=mean(fitted!=High)/mean(High=="Yes"))
#mean(predict(arbre,type="class")!=donnees$High)/mean(donnees$High=="Yes")

## ----teacher=correct--------------------------------------
tree1 <- rpart(High~.,data=data1,cp=0.000001,minsplit=2)
plotcp(tree1)
cp_opt <- tree1$cptable %>% as.data.frame() %>% slice(which.min(xerror)) %>% 
  dplyr::select(CP) %>% as.numeric()
tree_sel <- prune(tree1,cp=cp_opt)
rpart.plot(tree_sel) 

## ---------------------------------------------------------
tree2 <- rpart(High~.,data=data1,parms=list(loss=matrix(c(0,5,1,0),ncol=2)),
           cp=0.01,minsplit=2)

## ---------------------------------------------------------
tree2$parms
printcp(tree2)

## ----teacher=correct--------------------------------------
prev <- predict(tree2,type="class")
conf <- table(data1$High,prev)
conf
loss <- tree2$parms$loss
(conf[1,2]*loss[1,2]+conf[2,1]*loss[2,1])/nrow(data1)/mean(data1$High=="No")

## ----teacher=correct--------------------------------------
summary(predict(tree_sel,type="class"))
summary(predict(tree2,type="class"))

## ----message=FALSE, warning=FALSE-------------------------
library(kernlab)
data(spam)
set.seed(1234)
spam <- spam[sample(nrow(spam)),]

## ----teacher=correct--------------------------------------
plot(rf1)

## ----teacher=correct--------------------------------------
sel.mtry$bestTune

## ----teacher=correct--------------------------------------
ggplot(Imp) + aes(x=reorder(variable,MeanDecreaseAccuracy),y=MeanDecreaseAccuracy)+geom_bar(stat="identity")+coord_flip()+xlab("")+theme_classic()

## ----teacher=correct--------------------------------------
library(vip)
vip(rf3)

## ---------------------------------------------------------
x <- seq(-2*pi,2*pi,by=0.01)
y <- sin(x)
set.seed(1234)
X <- runif(200,-2*pi,2*pi)
Y <- sin(X)+rnorm(200,sd=0.2)
df1 <- data.frame(X,Y)
df2 <- data.frame(X=x,Y=y)
p1 <- ggplot(df1)+aes(x=X,y=Y)+geom_point()+geom_line(data=df2,size=1)+xlab("")+ylab("")
p1

## ----teacher=correct--------------------------------------
prev1 <- predict(L2boost,newdata=df2,n.trees=1)
df3 <- df2 %>% rename(vraie=Y) %>% mutate(`M=1`=prev1)
df4 <- df3 %>% pivot_longer(-X,names_to="courbes",values_to="prev")
ggplot(df4)+aes(x=X,y=prev,color=courbes)+geom_line(size=1)

## ----teacher=correct--------------------------------------
prev1000 <- predict(L2boost,newdata=df2,n.trees=1000)
prev500000 <- predict(L2boost,newdata=df2,n.trees=500000)
df31 <- df3 %>% mutate(`M=1000`=prev1000,`M=500000`=prev500000)
df41 <- df31 %>% pivot_longer(-X,names_to="courbes",values_to="prev")
ggplot(df41)+aes(x=X,y=prev,color=courbes)+geom_line(size=1)

## ---------------------------------------------------------
library(kernlab)
data(spam)
set.seed(1234)
spam <- spam[sample(nrow(spam)),]

## ---- eval=FALSE, include=TRUE,echo=TRUE------------------
#  model_ada1 <- gbm(type~.,data=spam,distribution="adaboost",interaction.depth=2,shrinkage=0.05,n.trees=500)

## ----teacher=correct--------------------------------------
spam1 <- spam
spam1$type <- as.numeric(spam1$type)-1
set.seed(1234)
model_ada1 <- gbm(type~.,data=spam1,distribution="adaboost",interaction.depth=2,shrinkage=0.05,n.trees=500)

## ----teacher=correct,max.height='500px'-------------------
summary(model_ada1)

## ----teacher=correct--------------------------------------
library(vip)
vip(model_ada1,num_features = 20L)

## ---------------------------------------------------------
library(keras)
#install_keras() 1 seule fois sur la machine

## ---------------------------------------------------------
library(kernlab)
data(spam)
spamX <- as.matrix(spam[,-58])
#spamY <- to_categorical(as.numeric(spam$type)-1, 2)
spamY <- as.numeric(spam$type)-1

## ---------------------------------------------------------
set.seed(5678)
perm <- sample(4601,3000)
appX <- spamX[perm,]
appY <- spamY[perm]
validX <- spamX[-perm,]
validY <- spamY[-perm]

## ----teacher=correct--------------------------------------
percep.sig <- keras_model_sequential() 
percep.sig %>% layer_dense(units=1,input_shape = 57,activation="sigmoid")

## ----teacher=correct--------------------------------------
summary(percep.sig)

## ----teacher=correct--------------------------------------
percep.sig %>% compile(
  loss="binary_crossentropy",
  optimizer="adam",
  metrics="accuracy"
)

## ----teacher=correct--------------------------------------
plot(p.sig)

## ---------------------------------------------------------
spamY1 <- to_categorical(as.numeric(spam$type)-1, 2)
appY1 <- spamY1[perm,]
validY1 <- spamY1[-perm,]

## ----teacher=correct--------------------------------------
percep.soft <- keras_model_sequential() 
percep.soft %>% layer_dense(units=2,input_shape = 57,activation="softmax")

## ----teacher=correct--------------------------------------
summary(percep.soft)

## ----teacher=correct--------------------------------------
percep.soft %>% compile(
  loss="binary_crossentropy",
  optimizer="adam",
  metrics="accuracy"
)

## ----teacher=correct--------------------------------------
plot(p.soft)

## ----teacher=correct--------------------------------------
percep.sig %>% evaluate(validX,validY)
percep.soft %>% evaluate(validX,validY1)

## ----teacher=correct--------------------------------------
mod2c <- keras_model_sequential() 
mod2c %>% layer_dense(units=100,activation="softmax") %>%
  layer_dense(units=100,activation="softmax") %>%
  layer_dense(units = 1,activation = "sigmoid")

## ----teacher=correct--------------------------------------
mod2c %>% compile(
  loss="binary_crossentropy",
  optimizer="adam",
  metrics="accuracy"
)

## ----teacher=correct--------------------------------------
mod2c %>% evaluate(validX,validY)

## ----teacher=correct--------------------------------------
mod2cd <- keras_model_sequential() 
mod2cd %>% layer_dropout(0.7) %>%
  layer_dense(units=50,activation="softmax") %>%
  layer_dense(units=30,activation="softmax") %>%
  layer_dense(units = 1,activation = "sigmoid")

## ----teacher=correct--------------------------------------
mod2cd %>% compile(
  loss="binary_crossentropy",
  optimizer="adam",
  metrics="accuracy"
)

## ----teacher=correct--------------------------------------
mod2cd %>% evaluate(validX,validY)

