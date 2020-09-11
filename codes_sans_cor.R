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

## ----echo=TRUE,eval=FALSE,purl=!correct-------------------
#  library(class)
#  knn3 <- knn(dapp[,1:2],dtest[,1:2],cl=dapp$Y,k=3)

## ---------------------------------------------------------
K_cand <- seq(1,500,by=20)

## ----indent='        ',hint=!correct----------------------
#  err.ho <- rep(0,length(K_cand))
#  for (i in 1:length(K_cand)){
#    ...
#    ...
#  }

## ----indent='        ',hint=!correct----------------------
#  err.loo <- rep(0,length(K_cand))
#  for (i in 1:length(K_cand)){
#    ...
#    ...
#  }

## ---------------------------------------------------------
set.seed(2345)
blocs <- caret::createFolds(1:nrow(donnees),10,returnTrain = TRUE)

## ----hint=!correct,cache=2--------------------------------
#  err.cv <- rep(0,length(K_cand))
#  prev <- donnees$Y
#  for (i in 1:length(K_cand)){
#    for (j in 1:length(blocs)){
#  ...
#  ...
#  ...
#    }
#    ...
#  }
#  K_cand[which.min(err.cv)]

## ----caret-ho1,cache=2------------------------------------
library(caret)
set.seed(321)
ctrl1 <- trainControl(method="LGOCV",number=1)
KK <- data.frame(k=K_cand)
caret.ho <- train(Y~.,data=donnees,method="knn",trControl=ctrl1,tuneGrid=KK)
caret.ho
plot(caret.ho)

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

## ----hint=!correct----------------------------------------
#  ind.svm <- mod.svm$index
#  sv <- donnees %>% slice(ind.svm)
#  ...

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

## ----hint=!correct----------------------------------------
#  mod.svm1 <- svm(Y~.,data=df3,kernel="linear",...)
#  mod.svm2 <- svm(Y~.,data=df3,kernel="linear",...)
#  mod.svm3 <- svm(Y~.,data=df3,kernel="linear",...)

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

## ---------------------------------------------------------
mod.svm1 <- svm(Y~.,data=df,kernel="radial",gamma=1,cost=1)
plot(mod.svm1,df,grid=250)

## ----hint=!correct----------------------------------------
#  mod.svm2 <- svm(Y~.,data=df,kernel="radial",gamma=...,cost=...)
#  mod.svm3 <- svm(Y~.,data=df,kernel="radial",gamma=...,cost=...)
#  mod.svm4 <- svm(Y~.,data=df,kernel="radial",gamma=...,cost=...)
#  
#  plot(mod.svm2,df,grid=250)
#  plot(mod.svm3,df,grid=250)
#  plot(mod.svm4,df,grid=250)
#  
#  mod.svm2$nSV
#  mod.svm3$nSV
#  mod.svm4$nSV
#  

## ----hint=!correct----------------------------------------
#  tune.out <- tune(svm,Y~.,data=...,kernel="...",
#               ranges=list(cost=...,gamma=...))

## ----hint=!correct----------------------------------------
#  C <- c(0.001,0.01,1,10,100,1000)
#  sigma <- c(0.5,1,2,3,4)
#  gr <- expand.grid(C=C,sigma=sigma)
#  ctrl <- trainControl(...)
#  res.caret1 <- train(...,prob.model=TRUE)
#  res.caret1

## ---------------------------------------------------------
n <- 50
set.seed(1234)
X <- runif(n)
set.seed(5678)
Y <- 1*X*(X<=0.6)+(-1*X+3.2)*(X>0.6)+rnorm(n,sd=0.1)
data1 <- data.frame(X,Y)
ggplot(data1)+aes(x=X,y=Y)+geom_point()

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

## ---------------------------------------------------------
library(ISLR)
data(Carseats)
summary(Carseats)

## ----eval=FALSE,echo=correct------------------------------
#  visTreeEditor(Carseats)

## ---------------------------------------------------------
new_ind <- Carseats %>% slice(3,58,185,218) %>% select(-Sales)
new_ind

## ----echo=!correct,eval=FALSE-----------------------------
#  set.seed(4321)
#  tree <- rpart(Sales~.,data=train,cp=0.000001,minsplit=2)

## ---------------------------------------------------------
High <- ifelse(Carseats$Sales<=8,"No","Yes")
data1 <- Carseats %>% dplyr::select(-Sales) %>% mutate(High)

## ---------------------------------------------------------
tree1 <- rpart(High~.,data=data1,parms=list(split="information"))
tree1$parms

## ---------------------------------------------------------
tree2 <- rpart(High~.,data=data1,parms=list(loss=matrix(c(0,5,1,0),ncol=2)),
           cp=0.01,minsplit=2)

## ---------------------------------------------------------
tree2$parms
printcp(tree2)

## ----message=FALSE, warning=FALSE-------------------------
library(kernlab)
data(spam)
set.seed(1234)
spam <- spam[sample(nrow(spam)),]

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

## ---------------------------------------------------------
library(kernlab)
data(spam)
set.seed(1234)
spam <- spam[sample(nrow(spam)),]

## ---- eval=FALSE, include=TRUE,echo=TRUE------------------
#  model_ada1 <- gbm(type~.,data=spam,distribution="adaboost",interaction.depth=2,shrinkage=0.05,n.trees=500)

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

## ----hint=!correct,cache=2--------------------------------
#  #Définition du modèle
#  percep.sig <- keras_model_sequential()
#  percep.sig %>% layer_dense(units=...,input_shape = ...,activation="...")
#  summary(percep.sig)
#  percep.sig %>% compile(
#    loss="binary_crossentropy",
#    optimizer="adam",
#    metrics="accuracy"
#  )
#  #Entrainement
#  p.sig <- percep.sig %>% fit(
#    x=...,
#    y=...,
#    epochs=...,
#    batch_size=...,
#    validation_split=...,
#    verbose=0
#  )

## ---------------------------------------------------------
spamY1 <- to_categorical(as.numeric(spam$type)-1, 2)
appY1 <- spamY1[perm,]
validY1 <- spamY1[-perm,]

