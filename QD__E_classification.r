#clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# CARICO DATASET
load(file="merge.RData")

#tutte le features da eliminare secondo AIC
merge$`Total assetsth EUR_mean`=NULL
merge$`Total assets turnover (times)_trend`=NULL
merge$`Solvency ratio (%)%_trend`=NULL
merge$`Return on asset (ROA)%_trend`=NULL
merge$`Return on equity (ROE)%_trend`=NULL
merge$`Current ratio_trend`   =NULL
merge$`Debt/EBITDA ratio%_mean`=NULL
merge$`EBITDAth EUR_trend`=NULL
merge$Leverage_trend  = NULL
merge$`EBITDA/Vendite%_trend` =NULL
merge$`Net working capitalth EUR_trend`= NULL
merge$`EBITDAth EUR_mean` =NULL
merge$`Debt/equity ratio%_mean` =NULL
merge$`Profit (loss)th EUR_trend`= NULL
merge$`Cash Flowth EUR_trend` =NULL
merge$`Net working capitalth EUR_mean`=NULL


Abstract(merge)

#tolgo tutti i missing values
merge=na.omit(merge)
table(merge$Failed,merge$`Last accounting closing date`)

#elimino i valori di merge che sono venuti inf a causa della media = 0
nums <- unlist(lapply(merge, is.numeric), use.names = FALSE)  
merge=merge[is.finite(rowSums(merge[ , nums])),]


table(merge$Failed,merge$`Last accounting closing date`)
library(DescTools)

#creo il training test
train=merge[merge$`Last accounting closing date`<= 2017,]
table(train$Failed,train$`Last accounting closing date`)
table(train$Failed)

#creo il test set
test=merge[merge$`Last accounting closing date`== 2018,]
table(test$Failed,test$`Last accounting closing date`)
table(test$Failed)



#trovo outliers tra i numerici e li imposto come NA
nums <- unlist(lapply(train, is.numeric), use.names = FALSE)  
train_numeric=train[ , nums]

for (i in 1:ncol(train_numeric)){
  print(names(train_numeric)[i])
  
  list_quantiles <- tapply(train_numeric[[i]], train$Failed,quantile)
  
  Q1s <- sapply(1:2, function(i) list_quantiles[[i]][2])
  Q3s <- sapply(1:2, function(i) list_quantiles[[i]][4])
  
  IQRs <- tapply(train[[i]], train$Failed, IQR)
  
  Lowers <- Q1s - 3 *IQRs
  Uppers <- Q3s + 3 *IQRs
  
  
  for (y in 1:2){
    is.na(train_numeric[,i])  <- train_numeric[train$Failed==y-1,][i] < Lowers[y] | train_numeric[train$Failed==y-1,][i] > Uppers[y]
  }
}

library(DescTools)

#aggiungo i categorici
train_numeric["Failed"]=train$Failed
train_numeric["Area"]=train$Area
train_numeric["ATECO_CAT"]=train$ATECO_CAT
train_numeric["Legal form"]=train$`Legal form`
train=train_numeric

Abstract(train)
#elimino le colonne che hanno troppi outliers
train$Age=NULL
test$Age=NULL
train$`Current liabilities/Tot ass.%_trend`=NULL
test$`Current liabilities/Tot ass.%_trend`=NULL

train=na.omit(train)

table(train$Failed)

#seleziono random un numero di aziende fallite nel 2018 simile al valore delle attive
negative=train[train$Failed==1,]
negative=negative[sample(nrow(negative),nrow(train[train$Failed==0,])),]
train=rbind(negative,train[train$Failed==0,])
table(train$Failed)


table(test$Failed)


#seleziono random un numero di aziende fallite nel 2018 simile al valore delle attive
positive=test[test$Failed==0,]
positive=positive[sample(nrow(positive),nrow(test[test$Failed==1,])),]
test=rbind(positive,test[test$Failed==1,])
table(test$Failed)


library(ROCR)
library(car)

#trasformo la data in factor
train$`Last accounting closing date`=factor(train$`Last accounting closing date`)
test$`Last accounting closing date`=factor(test$`Last accounting closing date`)


#seleziono per il training e per il test solo le colonne numeriche cosi da fare lo scaling
nums <- unlist(lapply(train, is.numeric), use.names = FALSE)  
train_num=train[ , nums]

nums_test <- unlist(lapply(test, is.numeric), use.names = FALSE)  
test_num=test[ , nums_test]

#mi salvo in un altro dataset i factor
train_cat=train[,sapply(train, is.factor)]
test_cat=test[,sapply(test, is.factor)]


library(caret)
#normalize data- min-max method
process_train <- preProcess(train_num, method = c("range"))

scaled_train <- predict(process_train, train_num)
scaled_test <- predict(process_train,test_num)


# riunisco numerici e categorici ed elimino la colonna con la data, inutile per le predizioni
scaled_train = cbind(scaled_train, train_cat)
scaled_train$`Last accounting closing date`=NULL
scaled_test = cbind(scaled_test, test_cat)
scaled_test$`Last accounting closing date`=NULL
# using ROC as metric
# need to use class names that are valid variable identifiers
lev = c("class0", "class1")
levels(scaled_train$Failed) = lev
levels(scaled_test$Failed) = lev











# custom summary for AUC
custSummary = function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}


#ESEGUO LOGISTIC REGRESSION CON 10 FOLD CROSS VALIDATION ESEGUITA 5 VOLTE
rcv = trainControl(method="repeatedcv", repeats=5, number=10, 
                   classProbs = TRUE, summaryFunction=custSummary)
set.seed(42) 
lr2.fit = train(Failed~., data = scaled_train, 
                method = "glm", trControl=rcv, metric="AUC", # custom metric
                family=binomial(logit))


lr2.fit
lr2.fit$resample # details over 5x10 folds
lr2.folds = lr2.fit$resample[['AUC']]
summary(lr2.fit) #COEFFICIENTI
t.test(lr2.folds) # AUC confidence interval



#ANALISI TRAINING SET  LOGISTIC REGRESSION
lev = c("class0", "class1")
levels(scaled_train$Failed) = lev

#PREDIZIONI SUL TRAINING SET  LOGISTIC REGRESSION
lr.pred = predict(lr2.fit, newdata = scaled_train) # class predict
lr.prob = predict(lr2.fit, newdata = scaled_train, type="prob") # as predict_proba in scikit-learn
lr.pconf = lr.prob[,2]

#CONFUSION MATRIX TRAINING SET
confusionMatrix(lr.pred, scaled_train$Failed, positive="class1")

#PER FARE LA CALIBRATION SERVONO 0 1
lev = c(0, 1)
levels(scaled_train$Failed) = lev

#CALIBRAZIONE MODELLO  LOGISTIC REGRESSION
cal_data=calibration(scaled_train$Failed ~ lr.pconf, class="1")
plot(cal_data)


#ANALISI ROC CURVE TRAINING SET LOGISTIC REGRESSION
lr.prediction = prediction(lr.pconf, scaled_train$Failed)

lr.acc = performance(lr.prediction, "acc"); plot(lr.acc)

lr.f1 = performance(lr.prediction, "f"); plot(lr.f1)
# roc curve
lr.roc = performance(lr.prediction, "tpr", "fpr")
plot(lr.roc, colorize=T); abline(a=0, b= 1)

#test wilcox per vedere quanto distingue in media positivi da negativi
negpos_train_lr = split(lr.pconf, scaled_train$Failed)
wilcox.test(negpos_train_lr$`1`, negpos_train_lr$`0`, conf.int=TRUE)
featurePlot(x=data.frame(lr.pconf), y=scaled_train$Failed, plot='density', auto.key = list(columns = 2))

##ANALISI TEST SET LOGISTIC REGRESSION

lev = c("class0", "class1")
levels(scaled_test$Failed) = lev
# PREDIZIONI TEST SET
lr.pred = predict(lr2.fit, newdata = scaled_test) # class predict
lr.prob = predict(lr2.fit, newdata = scaled_test, type="prob") # as predict_proba in scikit-learn
lr.pconf = lr.prob[,2]
#CONFUSION MATRIX TEST SET
confusionMatrix(lr.pred, scaled_test$Failed, positive="class1")

#SERVONO 0 1 PER LA CALIBRATION
lev = c(0, 1)
levels(scaled_test$Failed) = lev
cal_data=calibration(scaled_test$Failed ~ lr.pconf, class="1")
plot(cal_data)


#ANALISI ROC CURVE TEST SET LOGISTIC REGRESSION

lr.prediction = prediction(lr.pconf, scaled_test$Failed)
lr.acc = performance(lr.prediction, "acc"); plot(lr.acc)

lr.f1 = performance(lr.prediction, "f"); plot(lr.f1)
# roc curve
lr.roc = performance(lr.prediction, "tpr", "fpr")
plot(lr.roc, colorize=T); abline(a=0, b= 1)

#test wilcox per vedere quanto distingue in media positivi da negativi
negpos_test_lr = split(lr.pconf, scaled_test$Failed)
wilcox.test(negpos_test_lr$`1`, negpos_test_lr$`0`, conf.int=TRUE)
featurePlot(x=data.frame(lr.pconf), y=scaled_test$Failed, plot='density', auto.key = list(columns = 2))








## RANDOM FOREST

lev = c("class0", "class1")
levels(scaled_train$Failed) = lev

#PROVO DIVERSE COMBINAZIONI DI FEATURES CONSIDERATE
tunegrid = expand.grid(.mtry=(5:20)) 
tunegrid = expand.grid(.mtry=11) 

set.seed(42) # same folds as lr2.fit
rf.fit = train(Failed~., data = scaled_train, 
               method = "rf", trControl=rcv, metric="AUC",tuneGrid=tunegrid, ntree=20) 

rf.fit
rf.fit$resample # details over 5x10 folds
rf.folds = rf.fit$resample[['AUC']]
summary(rf.fit) #COEFFICIENTI
t.test(rf.folds) # AUC confidence interval



#ANALISI TRAINING SET RANDOM FOREST
lev = c("class0", "class1")
levels(scaled_train$Failed) = lev

#PREDIZIONI SUL TRAINING SET
rf.pred = predict(rf.fit, newdata = scaled_train) # class predict
rf.prob = predict(rf.fit, newdata = scaled_train, type="prob") # as predict_proba in scikit-learn
rf.pconf = rf.prob[,2]

#CONFUSION MATRIX TRAINING SET
confusionMatrix(rf.pred, scaled_train$Failed, positive="class1")

#PER FARE LA CALIBRATION SERVONO 0 1
lev = c(0, 1)
levels(scaled_train$Failed) = lev

#CALIBRAZIONE MODELLO RANDOM FOREST TRAINING SET
cal_data=calibration(scaled_train$Failed ~ rf.pconf, class="1")
plot(cal_data)


#ANALISI ROC CURVE TRAINING SET RANDOM FOREST
rf.prediction = prediction(rf.pconf, scaled_train$Failed)

rf.acc = performance(rf.prediction, "acc"); plot(rf.acc)

rf.f1 = performance(rf.prediction, "f"); plot(rf.f1)
# roc curve
rf.roc = performance(rf.prediction, "tpr", "fpr")
plot(rf.roc, colorize=T); abline(a=0, b= 1)

#test wilcox per vedere quanto distingue in media positivi da negativi
negpos_train_rf = split(lr.pconf, scaled_train$Failed)
wilcox.test(negpos_train_rf$`1`, negpos_train_rf$`0`, conf.int=TRUE)
featurePlot(x=data.frame(lr.pconf), y=scaled_train$Failed, plot='density', auto.key = list(columns = 2))


##ANALISI TEST SET RANDOM FOREST

lev = c("class0", "class1")
levels(scaled_test$Failed) = lev
# PREDIZIONI TEST SET
rf.pred = predict(rf.fit, newdata = scaled_test) # class predict
rf.prob = predict(rf.fit, newdata = scaled_test, type="prob") # as predict_proba in scikit-learn
rf.pconf = rf.prob[,2]
#CONFUSION MATRIX TEST SET RANDOM FOREST
confusionMatrix(rf.pred, scaled_test$Failed, positive="class1")

#SERVONO 0 1 PER LA CALIBRATION
lev = c(0, 1)
levels(scaled_test$Failed) = lev
cal_data=calibration(scaled_test$Failed ~ rf.pconf, class="1")
plot(cal_data)


#ANALISI ROC CURVE TEST SET RANDOM FOREST

rf.prediction = prediction(rf.pconf, scaled_test$Failed)
rf.acc = performance(rf.prediction, "acc"); plot(rf.acc)

rf.f1 = performance(rf.prediction, "f"); plot(rf.f1)
# roc curve
rf.roc = performance(rf.prediction, "tpr", "fpr")
plot(rf.roc, colorize=T); abline(a=0, b= 1)

#test wilcox per vedere quanto distingue in media positivi da negativi
negpos_test_rf = split(rf.pconf, scaled_test$Failed)
wilcox.test(negpos_test_rf$`1`, negpos_test_rf$`0`, conf.int=TRUE)
featurePlot(x=data.frame(rf.pconf), y=scaled_test$Failed, plot='density', auto.key = list(columns = 2))






##LASSO





lev = c(0, 1)
levels(scaled_train$Failed) = lev
# Elastic net logistic regression with cross-validation
library(glmnet)

x = model.matrix(Failed~., scaled_train)[,-1]
y = scaled_train$Failed

cv.lasso = cv.glmnet(x, y, alpha = 1, family = "binomial")


lev = c("class0", "class1")
levels(scaled_train$Failed) = lev

rcv = trainControl(method="repeatedcv", repeats=5, number=10, 
                   classProbs = TRUE, summaryFunction=custSummary)

lasso<-train(y = y,
             x = x,
             method = 'glmnet', 
             trControl=rcv, metric="AUC",
             family="binomial",
             tuneGrid = expand.grid(alpha = 1, lambda = cv.lasso$lambda.1se) 
) 



lasso


lasso$resample # details over 5x10 folds
lr_lasso.folds = lasso$resample[['AUC']]
mean(lr_lasso.folds)
t.test(lr_lasso.folds) # auc confidence interval




#ANALISI TRAINING SET  LOGISTIC REGRESSION WITH LASSO
lev = c("class0", "class1")
levels(scaled_train$Failed) = lev

#PREDIZIONI SUL TRAINING SET  LOGISTIC REGRESSION WITH LASSO
lasso.pred = predict(lasso, newdata = x) # class predict
lasso.prob = predict(lasso, newdata = x, type="prob") # as predict_proba in scikit-learn
lasso.pconf = lasso.prob[,2]

#CONFUSION MATRIX TRAINING SET WITH LASSO
confusionMatrix(lasso.pred, scaled_train$Failed, positive="class1")

#PER FARE LA CALIBRATION SERVONO 0 1
lev = c(0, 1)
levels(scaled_train$Failed) = lev

#CALIBRAZIONE MODELLO  LOGISTIC REGRESSION WITH LASSO
cal_data=calibration(scaled_train$Failed ~ lasso.pconf, class="1")
plot(cal_data)


#ANALISI ROC CURVE TRAINING SET LOGISTIC REGRESSION WITH LASSO
lasso.prediction = prediction(lasso.pconf, scaled_train$Failed)

lasso.acc = performance(lasso.prediction, "acc"); plot(lasso.acc)

lasso.f1 = performance(lasso.prediction, "f"); plot(lasso.f1)
# roc curve
lasso.roc = performance(lasso.prediction, "tpr", "fpr")
plot(lasso.roc, colorize=T); abline(a=0, b= 1)

#test wilcox per vedere quanto distingue in media positivi da negativi
negpos_train_lasso = split(lasso.pconf, scaled_train$Failed)
wilcox.test(negpos_train_lasso$`1`, negpos_train_lasso$`0`, conf.int=TRUE)
featurePlot(x=data.frame(lasso.pconf), y=scaled_train$Failed, plot='density', auto.key = list(columns = 2))

##ANALISI TEST SET LOGISTIC REGRESSION WITH LASSO

x_test=model.matrix(Failed~., scaled_test)[,-1]
lev = c("class0", "class1")
levels(scaled_test$Failed) = lev
# PREDIZIONI TEST SET
lasso.pred = predict(lasso, newdata = x_test) # class predict
lasso.prob = predict(lasso, newdata = x_test, type="prob") # as predict_proba in scikit-learn
lasso.pconf = lasso.prob[,2]
#CONFUSION MATRIX TEST SET
confusionMatrix(lasso.pred, scaled_test$Failed, positive="class1")

#SERVONO 0 1 PER LA CALIBRATION
lev = c(0, 1)
levels(scaled_test$Failed) = lev
cal_data=calibration(scaled_test$Failed ~ lasso.pconf, class="1")
plot(cal_data)


#ANALISI ROC CURVE TEST SET LOGISTIC REGRESSION WITH LASSO

lasso.prediction = prediction(lasso.pconf, scaled_test$Failed)
lasso.acc = performance(lasso.prediction, "acc"); plot(lasso.acc)

lasso.f1 = performance(lasso.prediction, "f"); plot(lasso.f1)
# roc curve
lasso.roc = performance(lasso.prediction, "tpr", "fpr")
plot(lasso.roc, colorize=T); abline(a=0, b= 1)

#test wilcox per vedere quanto distingue in media positivi da negativi
negpos_test_lasso = split(lasso.pconf, scaled_test$Failed)
wilcox.test(negpos_test_lasso$`1`, negpos_test_lasso$`0`, conf.int=TRUE)
featurePlot(x=data.frame(lasso.pconf), y=scaled_test$Failed, plot='density', auto.key = list(columns = 2))


#COMPARISON MODELS 2 models

#costruisco un dataset con i valori della AUC nelle varie folds
models=cbind.data.frame(lr2.fit$resample[,1],rf.fit$resample[,1])
colnames(models) <- c("Logistic", "Random Forest")

boxplot(models,main="Box-Plot AUC")

#controllo la normalità
sapply(models, function(r) shapiro.test(r)$p.value)


sapply(models, function(r) t.test(r))



#COMPARISON MODELS 3 models

library(multcomp) # multiple comparison library

#costruisco un dataset con i valori della AUC nelle varie folds
models=cbind.data.frame(lr2.fit$resample[,1],rf.fit$resample[,1],lasso$resample[,1])
colnames(models) <- c("Logistic", "Random Forest","Lasso")

boxplot(models,main="Box-Plot AUC")

#controllo la normalità
sapply(models, function(r) shapiro.test(r)$p.value)

#controllo l'omogeneità della varianza
merged = data.frame(auc=c(t(models)), clf=rep(colnames(models), nrow(models)))
View(merged)
bartlett.test(auc~clf, data=merged)


#eseguo test di anova 
merged$clf=factor(merged$clf)
fit = lm(auc~clf, data=merged)
summary(fit)
anova(fit)

#eseguo test post-hoc (2 tipi)
ph = glht(fit, linfct = mcp(clf = "Tukey"))
summary(ph) # Tukey all-pairs post-hoc
confint(ph, level=0.99) # confidence intervals

ph = glht(fit, linfct = mcp(clf = "Dunnett"))
summary(ph) # Dunnett one-vs-other post-hoc
confint(ph, level=0.99) # confidence intervals


library(dplyr)
#RATING
lr.pred = predict(lr2.fit, newdata = scaled_test) # class predict
lr.prob = predict(lr2.fit, newdata = scaled_test, type="prob")
lr.pconf = lr.prob[,2]
typeof(lr.pconf) #check type
#rm(dt,col,bin,total)
dt <- data.table::as.data.table(lr.pconf) #convert to df
#View(dt)
col = dt %>% mutate(new_bin = cut(lr.pconf, breaks= (seq(0,1, 0.1))))
bin = dt %>% mutate(new_bin = cut(lr.pconf, breaks= seq(0,1, 0.1), labels = toupper(letters[seq(1:10)])))
#View(bin)
test_rating <- cbind(scaled_test, bin)
test_rating = cbind(test_rating, col[,2])
names(test_rating)[length(test_rating)] <- 'new_bin_num'
View(test_rating)


labels = toupper(letters[seq(1:10)])

#ESEGUO BINOMIAL TEST PER OGNI CATEGORIAL DI RATING PER VEDERE SE LA PROB PREDETTA E' COMPARABILE CON QUELLA 
#OSSERVATA
for (lab in labels) {
  n_letter  = nrow(test_rating[test_rating$new_bin == lab,])  #number of companies for that label
  n_letter_failed = nrow(test_rating[test_rating$new_bin == lab
                                     & test_rating$Failed == "class1",]) #number of failed companies for a specific label
  
  
  if (lab == "A") { prob = 0.1}
  if (lab == "B") { prob = 0.2}
  if (lab == "C") { prob = 0.3}
  if (lab == "D") { prob = 0.4}
  if (lab == "E") { prob = 0.5}
  if (lab == "F") { prob = 0.6}
  if (lab == "G") { prob = 0.7}
  if (lab == "H") { prob = 0.8}
  if (lab == "I") { prob = 0.9}
  if (lab == "J") { prob = 1}  
  
  print(paste0("************ binom test for companies with class ",lab))
  bin=binom.test(n_letter_failed, n_letter ,p=prob, conf = 1-((1-0.95)/length(labels)),
                 alternative = "greater")  #Bonferroni correction
  print(bin)
  
}






#Selective classification

# selective classification

#predizioni logistic regression su test set
lev = c("class0", "class1")
levels(scaled_test$Failed) = lev
lr.pred = predict(lr2.fit, newdata = scaled_test) # class predict
lr.prob = predict(lr2.fit, newdata = scaled_test, type="prob") # as predict_proba in scikit-learn
lr.pconf = lr.prob[,2]
#CONFUSION MATRIX TEST SET
confusionMatrix(lr.pred, scaled_test$Failed, positive="class1")

View(lr.prob)

g = function(prob, tau) tau > pmin(prob[1], prob[2])
taus = seq(0.05, 0.5, 0.05)
coverage = c()
misc = c()

for (tau in taus) {
  
  sel = g(lr.prob, tau)
  cov0 = mean(sel)
  coverage = append(coverage, cov0)
  cm=confusionMatrix(lr.pred[sel], scaled_test$Failed[sel], positive="class1")
  
  risk0 = 1-cm$overall["Accuracy"]
  misc = append(misc, risk0)
  
}
# coverage-risk curve for misclassification error
plot(coverage, misc, type='b',ylim=c(0.1,0.4),main="Risk Coverage Curve",
     xlab="Coverage (%)",ylab="Misclassification (%)")

text(coverage, misc, labels=taus, cex=0.9, font=2, pos=3) 

save.image()
