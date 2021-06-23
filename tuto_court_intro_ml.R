## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
library(tidyverse)
theme_set(theme_bw(base_size = 9))
update_geom_defaults("point", list(size=0.65))

## -----------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(car)
library(bestglm)
library(glmnet)
library(kernlab)
library(rpart)
library(rpart.plot)
library(ranger)
library(pROC)

## -----------------------------------------------------------------------------
ozone <- read.table("ozone.txt")
summary(ozone)

## ----teacher=correct----------------------------------------------------------
mod.complet <- lm(maxO3~.,data=ozone)

## ----teacher=correct----------------------------------------------------------
summary(mod.complet)

## ----teacher=correct----------------------------------------------------------
Anova(mod.complet)

## ----teacher=correct----------------------------------------------------------
ozone1 <- ozone[,c(2:13,1)]
ozone1$vent <- as.factor(ozone1$vent)
ozone1$pluie <- as.factor(ozone1$pluie)
sel.BIC <- bestglm(ozone1)
sel.AIC <- bestglm(ozone1,IC="AIC")

## ----teacher=correct----------------------------------------------------------
sel.BIC$BestModel
sel.AIC$BestModel

## ----teacher=correct----------------------------------------------------------
sel.BIC$BestModels
sel.AIC$BestModels

## ----teacher=correct----------------------------------------------------------
final <- sel.BIC$BestModel
summary(final)

## ----teacher=correct----------------------------------------------------------
tbl <- tibble(index=1:nrow(ozone),res=rstudent(final),maxO3=ozone$maxO3)
ggplot(tbl)+aes(x=index,y=res)+geom_point()+geom_hline(yintercept = c(-2,2))
ggplot(tbl)+aes(x=maxO3,y=res)+geom_point()+geom_hline(yintercept = c(-2,2))

## -----------------------------------------------------------------------------
data(SAheart)
summary(SAheart)

## ----teacher=correct----------------------------------------------------------
mod.complet <- glm(chd~.,data=SAheart,family="binomial")
summary(mod.complet)

## -----------------------------------------------------------------------------
(xnew <- SAheart[50,1:9])

## ----teacher=correct----------------------------------------------------------
predict(mod.complet,newdata=xnew,type="response")

## ----teacher=correct----------------------------------------------------------
sel.BIC <- bestglm(SAheart,family=binomial)
sel.BIC$BestModel %>% summary()

## -----------------------------------------------------------------------------
set.seed(123)
don_split <- initial_split(SAheart,prop=2/3)
dapp <- training(don_split)
dtest <- testing(don_split)

## ----teacher=correct----------------------------------------------------------
complet <- glm(chd~.,data=dapp,family=binomial)
sel <- glm(chd~tobacco + ldl + famhist + typea + age,data=dapp,family=binomial)

## ----teacher=correct----------------------------------------------------------
tbl.prev <- tibble(complet=predict(complet,newdata=dtest,type="response"),
               sel=predict(sel,newdata=dtest,type="response"),
               obs=as.factor(dtest$chd))

## ----eval=correct-------------------------------------------------------------
roc.obj <- roc(obs~.,data=tbl.prev)
plot(roc.obj[[1]]) 
plot(roc.obj[[2]],add=TRUE,col="red")
legend("bottomright",legend=c("S1","S2"),col=c("black","red"),lwd=3,cex=0.5)

## ----teacher=correct----------------------------------------------------------
pROC::ggroc(roc.obj)+labs(color="Score")

## ----teacher=correct----------------------------------------------------------
lapply(roc.obj,auc)

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% 
  summarize_at(1:2,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## -----------------------------------------------------------------------------
data(spam)
set.seed(123)
spam_split <- initial_split(spam,prop=2/3)
dapp <- training(spam_split)
dtest <- testing(spam_split)

## -----------------------------------------------------------------------------
tbl.prev <- matrix(0,ncol=2,nrow=nrow(dtest)) %>% as_tibble()
names(tbl.prev) <- c("Ridge","Lasso")

## -----------------------------------------------------------------------------
X.app <- model.matrix(type~.,data=dapp)[,-1]
Y.app <- dapp$type
X.test <- model.matrix(type~.,data=dtest)[,-1]
Y.test <- dtest$type

## ----teacher=correct----------------------------------------------------------
set.seed(123)
ridge.cv <- cv.glmnet(X.app,Y.app,alpha=0,family=binomial)
plot(ridge.cv)

## ----teacher=correct----------------------------------------------------------
set.seed(123)
ridge.cv <- cv.glmnet(X.app,Y.app,alpha=0,family=binomial,
                  lambda=exp(seq(-10,2,length=50)))
plot(ridge.cv)

## ----teacher=correct----------------------------------------------------------
prev.ridge <- predict(ridge.cv,newx=X.test,type="response") %>% as.numeric()
tbl.prev$Ridge <- prev.ridge

## ----teacher=correct----------------------------------------------------------
set.seed(123)
lasso.cv <- cv.glmnet(X.app,Y.app,alpha=1,family=binomial)
plot(lasso.cv)

## ----teacher=correct----------------------------------------------------------
prev.lasso <- predict(lasso.cv,newx=X.test,type="response") %>% as.numeric()
tbl.prev$Lasso <- prev.lasso

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  pivot_longer(-obs,names_to="Algo",values_to = "score") %>%
  group_by(Algo) %>%
  roc_curve(truth=obs,estimate=score,event_level="second") %>%
  autoplot()

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:2,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## ----teacher=correct----------------------------------------------------------
logit <- glm(type~.,data=dapp,family=binomial)
tbl.prev1 <- tbl.prev %>% mutate(logit=predict(logit,newdata=dtest,type="response"))
tbl.prev1 %>% mutate(obs=dtest$type) %>% 
  pivot_longer(-obs,names_to="Algo",values_to = "score") %>%
  group_by(Algo) %>%
  roc_curve(truth=obs,estimate=score,event_level="second") %>%
  autoplot()

## ----teacher=correct----------------------------------------------------------
tbl.prev1 %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:3,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## ----teacher=correct----------------------------------------------------------
set.seed(123)
arbre <- rpart(type~.,data=dapp,cp=0.0001,minsplit=15)
plotcp(arbre)

## ----teacher=correct----------------------------------------------------------
arbre_final <- prune(arbre,cp=0.0035)
rpart.plot(arbre_final)

## ----teacher=correct----------------------------------------------------------
prev_arbre <- predict(arbre_final,newdata=dtest)[,2]
tbl.prev$Arbre <- prev_arbre

## ----teacher=correct----------------------------------------------------------
foret.prob <- ranger(type~.,data=dapp,probability=TRUE)

## ----teacher=correct----------------------------------------------------------
prev_foret <- predict(foret.prob,data=dtest)$predictions[,2]
tbl.prev$Foret <- prev_foret

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  pivot_longer(-obs,names_to="Algo",values_to = "score") %>%
  group_by(Algo) %>%
  roc_curve(truth=obs,estimate=score,event_level="second") %>%
  autoplot()

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:4,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## ----teacher=correct----------------------------------------------------------
prev.class <- round(tbl.prev) %>% 
  mutate_all(~dplyr::recode(.,"0"="nonspam","1"="spam")) %>%
  bind_cols(obs=dtest$type)
head(prev.class)

## ----teacher=correct----------------------------------------------------------
multi_metric <- metric_set(accuracy,bal_accuracy,f_meas,kap)
prev.class %>% 
  pivot_longer(-obs,names_to = "Algo",values_to = "classe") %>%
  mutate(classe=as.factor(classe)) %>%
  group_by(Algo) %>%
  multi_metric(truth=obs,estimate=classe,event_level = "second") %>%
  mutate(.estimate=round(.estimate,3)) %>%
  pivot_wider(-.estimator,names_from=.metric,values_from = .estimate)

