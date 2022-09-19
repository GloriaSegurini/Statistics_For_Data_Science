# clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# CARICO DATASET
load(file="merge.RData")
#load(file="test.RData")



#tolgo tutti i missing values
merge=na.omit(merge)
table(merge$Failed,merge$`Last accounting closing date`)

#elimino i valori di merge che sono venuti inf a causa della media = 0
nums <- unlist(lapply(merge, is.numeric), use.names = FALSE)  
merge=merge[is.finite(rowSums(merge[ , nums])),]


#creo il training test
train=merge[merge$`Last accounting closing date`< 2017,]
table(train$Failed,train$`Last accounting closing date`)
table(train$Failed)

#creo il test set
test=merge[merge$`Last accounting closing date`>= 2017,]
table(test$Failed,test$`Last accounting closing date`)
table(test$Failed)

#seleziono random un numero di aziende fallite nel 2018 simile al valore delle attive
test["select"]=1
rec_act=nrow(test[test$Failed==0,])
rec_fail=nrow(test[test$Failed==1,])
set.seed(30)
rb=rbinom(rec_act,1,rec_fail/(rec_fail+rec_act))
test[test$Failed==0,]["select"]=rb
test=test[test$select==1,]
table(test$Failed)
test["select"]=NULL






#trovo outliers tra i numerici e li imposto come NA
nums <- unlist(lapply(train, is.numeric), use.names = FALSE)  
train_numeric=train[ , nums]

for (i in 1:ncol(train_numeric)){
  print(names(train_numeric)[i])
  
  list_quantiles <- tapply(train_numeric[[i]], train$Failed,quantile)
  
  Q1s <- sapply(1:2, function(i) list_quantiles[[i]][2])
  Q3s <- sapply(1:2, function(i) list_quantiles[[i]][4])
  
  IQRs <- tapply(train[[i]], train$Failed, IQR)
  
  Lowers <- Q1s - 6*IQRs
  Uppers <- Q3s + 6*IQRs
  

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

Abstract(train_numeric)
#elimino le colonne che hanno troppi outliers
train_numeric$Age=NULL
test$Age=NULL
train_numeric$`Current liabilities/Tot ass.%_trend`=NULL
test$`Current liabilities/Tot ass.%_trend`=NULL


#elimino tutti gli outliers che prima avevo segnato come NA
train=na.omit(train_numeric)
Abstract(train)


table(train$Failed,train$`Last accounting closing date`)
table(train$Failed)


#FACCIO IL TEST DEL CHI QUADRO TRA I NUMERICI (CATEGORIZZATI IN BINS) E
#IL TARGET FAILED PER VEDERE SE SONO INDIPENDENTI
nums <- unlist(lapply(train, is.numeric), use.names = FALSE)  
train_numeric=train[ , nums]
train_numeric$`Last accounting closing date`=NULL
ntest = ncol(train)-1

for (i in 1:ncol(train_numeric)){
  

  print("***************************")
  print(names(train_numeric)[i])
  
  #ESEGUO I BINS UTILIZZANDO I QUANTILI
  div=quantile(train_numeric[[i]], 0:4 / 4)
  div[1]=-Inf
  train_numeric[i] <- cut(train_numeric[[i]],div)
  
  U = table(train_numeric[[i]], train$Failed)
  print(chisq.test(U, correct=FALSE)$p.value*ntest) # Chi-squared test
  cat("\n")
  
}


#ELIMINO Total assets turnover (times)_trend PERCHE' PRESENTA P-VALUE ELEVATO
train$`Total assets turnover (times)_trend`=NULL
test$`Total assets turnover (times)_trend`=NULL


#testing independence of Failed for factors
tab = table(train$ATECO_CAT,train$Failed )
chisq.test(tab, correct=FALSE)$p.value*ntest # low p-value -> keep
tab_2 = table(train$`Legal form`,train$Failed )
chisq.test(tab_2, correct=F)$p.value*ntest  # low p-value -> keep
tab_3 = table( train$Area,train$Failed)
chisq.test(tab_3, correct=F)$p.value*ntest  # low p-value -> keep
table(train$Failed)
Abstract(train)




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
Abstract(scaled_train)



#calcolo la correlazione di pearson e guardo dove è maggiore di 0.70
corr=cor(scaled_train[,unlist(lapply(test, is.numeric), use.names = FALSE)])
corr[abs(corr) < 0.7] = NA
View(corr)

rm(corr)


library(car)
#eseguo logistic regression per vedere il variance inflaction rate delle features
fit = glm(Failed~., data=scaled_train, family=binomial("logit"))
summary(fit)
vif(fit)

#elimino Cash Flowth EUR_mean per l'alto valore di VIR
scaled_train$`Cash Flowth EUR_mean`=NULL
scaled_test$`Cash Flowth EUR_mean`=NULL

#controllo ancora il valore di VIR dopo aver tolto la feature
fit = glm(Failed~., data=scaled_train, family=binomial("logit"))
summary(fit)
vif(fit)

library(MASS)
#eseguo AIC per fare una features selection
step = stepAIC(fit, direction="backward")
summary(step)

save(step, file="step.RData") 

#tutte le features da eliminare secondo AIC
scaled_train$`Total assetsth EUR_mean`=NULL
scaled_train$`Total assets turnover (times)_trend`=NULL
scaled_train$`Solvency ratio (%)%_trend`=NULL
scaled_train$`Return on asset (ROA)%_trend`=NULL
scaled_train$`Return on equity (ROE)%_trend`=NULL
scaled_train$`Current ratio_trend`   =NULL
scaled_train$`Debt/EBITDA ratio%_mean`=NULL
scaled_train$`EBITDAth EUR_trend`=NULL
scaled_train$Leverage_trend  = NULL
scaled_train$`EBITDA/Vendite%_trend` =NULL
scaled_train$`Net working capitalth EUR_trend`= NULL
scaled_train$`EBITDAth EUR_mean` =NULL
scaled_train$`Debt/equity ratio%_mean` =NULL
scaled_train$`Profit (loss)th EUR_trend`= NULL
scaled_train$`Cash Flowth EUR_trend` =NULL
scaled_train$`Net working capitalth EUR_mean`=NULL

#controllo ancora il valore del VIR
fit = glm(Failed~., data=scaled_train, family=binomial("logit"))
summary(fit)
vif(fit)

#save(train_final, file="train_final.RData") 
#save(test, file="test_final.RData") 

