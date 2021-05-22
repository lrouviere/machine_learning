## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
library(tidyverse)
theme_set(theme_bw(base_size = 9))
update_geom_defaults("point", list(size=0.65))

## -----------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(glmnet)
library(kernlab)
library(kknn)
library(doParallel)
library(rpart)
library(rpart.plot)
library(ranger)
library(gbm)

## -----------------------------------------------------------------------------
data(spam)
dim(spam)

## -----------------------------------------------------------------------------
summary(spam$type)

## -----------------------------------------------------------------------------
set.seed(123)
spam_split <- initial_split(spam,prop=2/3)
dapp <- training(spam_split)
dtest <- testing(spam_split)

## -----------------------------------------------------------------------------
knn20 <- kknn(type~.,train=dapp,test=dtest,k=25,
              kernel="rectangular")$fitted.values
head(knn20)

## -----------------------------------------------------------------------------
mean(knn20!=dtest$type)

## -----------------------------------------------------------------------------
tibble(prev=knn20,obs=dtest$type) %>% accuracy(truth=obs,estimate=prev)

## -----------------------------------------------------------------------------
tune_spec <- 
  nearest_neighbor(neighbors=tune(),weight_func="rectangular") %>% 
  set_mode("classification") %>%
  set_engine("kknn") 

## -----------------------------------------------------------------------------
ppv_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(type ~ .)

## -----------------------------------------------------------------------------
set.seed(123)
re_ech_cv <- vfold_cv(spam,v=10)

## -----------------------------------------------------------------------------
grille_k <- tibble(neighbors=c(1,5,11,101,1001))

## -----------------------------------------------------------------------------
ppv.cv %>% collect_metrics()

## -----------------------------------------------------------------------------
(best_k <- ppv.cv %>% select_best())

## -----------------------------------------------------------------------------
final_ppv <- 
  ppv_wf %>% 
  finalize_workflow(best_k) %>%
  fit(data = spam)

## ----tunegrid-ppv-auc, teacher=correct,cache=TRUE-----------------------------
auc.cv <- ppv_wf %>% 
  tune_grid(
resamples = re_ech_cv,
grid = grille_k,
control=control_resamples(save_pred = TRUE),
metrics=metric_set(roc_auc))
auc.cv %>% collect_metrics()

## ----teacher=correct----------------------------------------------------------
score <- collect_predictions(auc.cv)
score %>% group_by(neighbors) %>% 
  roc_curve(type, .pred_spam,event_level="second") %>% 
  autoplot()

## -----------------------------------------------------------------------------
tbl.prev <- matrix(0,ncol=6,nrow=nrow(dtest)) %>% as_tibble()
names(tbl.prev) <- c("Ridge","Lasso","SVM","Arbre","Foret","Boosting")

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
                      lambda=exp(seq(-8,2,length=50)))
plot(ridge.cv)

## ----teacher=correct----------------------------------------------------------
prev.ridge <- predict(ridge.cv,newx=X.test,type="response") %>% as.numeric()
tbl.prev$Ridge <- prev.ridge

## -----------------------------------------------------------------------------
set.seed(123)
lasso.cv <- cv.glmnet(X.app,Y.app,alpha=1,family=binomial)
plot(lasso.cv)

## ----teacher=correct----------------------------------------------------------
prev.lasso <- predict(lasso.cv,newx=X.test,type="response") %>% as.numeric()
tbl.prev$Lasso <- prev.lasso

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:2,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## -----------------------------------------------------------------------------
tune_spec_svm <- 
  svm_rbf(cost=tune(),rbf_sigma = 0.1) %>%
  set_mode("classification") %>%
  set_engine("kernlab")
svm_wf <- workflow() %>% 
  add_model(tune_spec_svm) %>%
  add_formula(type ~ .)

## -----------------------------------------------------------------------------
set.seed(12345)
re_ech_cv <- vfold_cv(spam,v=5)
grille_C <- tibble(cost=c(0.1,1,5,10))

## ----tune-svm-para,teacher=correct,cache=TRUE---------------------------------
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
svm.cv <- svm_wf %>% 
  tune_grid(resamples = re_ech_cv,
            grid = grille_C,
            control=control_resamples(save_pred = TRUE),
            metrics=metric_set(roc_auc))
stopCluster(cl)

## ----teacher=correct----------------------------------------------------------
svm.cv %>% collect_metrics()

## ----teacher=correct----------------------------------------------------------
best_C <- svm.cv %>% select_best()
final_svm <- svm_wf %>% 
  finalize_workflow(best_C) %>%
  fit(data = dapp)

## ----teacher=correct----------------------------------------------------------
prev_svm <- predict(final_svm,new_data=dtest,type="prob") %>%
  select(.pred_spam) %>% unlist() %>% as.numeric()
tbl.prev$SVM <- prev_svm

## ----teacher=correct----------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
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
tbl.prev %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:4,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## -----------------------------------------------------------------------------
foret.prob <- ranger(type~.,data=dapp,probability=TRUE)

## -----------------------------------------------------------------------------
prev_foret <- predict(foret.prob,data=dtest)$predictions[,2]
tbl.prev$Foret <- prev_foret

## ----eval=correct-------------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:5,~roc_auc_vec(truth=obs,estimate=.,event_level="second"))

## ---- teacher=correct,error=TRUE----------------------------------------------
logit1 <- gbm(type~.,data=dapp,distribution="bernoulli",
              interaction.depth=4,
              shrinkage=0.05,n.trees=500)

## -----------------------------------------------------------------------------
dapp1 <- dapp
dapp1$type <- as.numeric(dapp1$type)-1
set.seed(1234)
boost1 <- gbm(type~.,data=dapp1,distribution="bernoulli",
              interaction.depth=4,
              shrinkage=0.05,n.trees=500)

## ----teacher=correct----------------------------------------------------------
set.seed(321)
boost <- gbm(type~.,data=dapp1,distribution="bernoulli",
          interaction.depth=4,
          cv.folds=5,
          shrinkage=0.1,n.trees=500)

## ----teacher=correct----------------------------------------------------------
(ntrees_opt <- gbm.perf(boost))

## ----teacher=correct----------------------------------------------------------
prev_boost <- predict(boost,newdata=dtest,
                  type="response",n.trees=ntrees_opt)
tbl.prev$Boosting <- prev_boost

## ----echo=FALSE,eval=correct--------------------------------------------------
save(tbl.prev,file="~/Dropbox/LAURENT/COURS/CEPE/MACHINE_LEARNING/TUTO_COURT/tbl.prev.RData")

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
load("~/Dropbox/LAURENT/COURS/CEPE/MACHINE_LEARNING/TUTO_COURT/tbl.prev.RData")

## -----------------------------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  summarize_at(1:6,~roc_auc_vec(truth=obs,estimate=.,event_level="second")) %>% 
  round(3)

## -----------------------------------------------------------------------------
tbl.prev %>% mutate(obs=dtest$type) %>% 
  pivot_longer(-obs,names_to="Algo",values_to = "score") %>%
  group_by(Algo) %>%
  roc_curve(truth=obs,estimate=score,event_level="second") %>%
  autoplot()

## -----------------------------------------------------------------------------
prev.class <- round(tbl.prev) %>% 
  mutate_all(~recode(.,"0"="nonspam","1"="spam")) %>%
  bind_cols(obs=dtest$type)
head(prev.class)

## -----------------------------------------------------------------------------
multi_metric <- metric_set(accuracy,bal_accuracy,f_meas,kap)
prev.class %>% 
  pivot_longer(-obs,names_to = "Algo",values_to = "classe") %>%
  mutate(classe=as.factor(classe)) %>%
  group_by(Algo) %>%
  multi_metric(truth=obs,estimate=classe,event_level = "second") %>%
  mutate(.estimate=round(.estimate,3)) %>%
  pivot_wider(-.estimator,names_from=.metric,values_from = .estimate)

