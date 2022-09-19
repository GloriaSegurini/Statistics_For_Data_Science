#clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# CARICO DATASET
load(file="QA.RData")

colnames(QA)

#take year = 2016
QC_2016=QA[QA$`Last accounting closing date`==2016,]


table(QC_2016$Failed,QC_2016$Size)

###probabilità condizionata rispetto a size

#creo i dataset con le diverse size e calcolo la probabilità condizionata su ognuno
size=c()
prob=c()
for (siz in unique(QC_2016$Size) ) {
  nam = paste0("QC_",2016,"_",siz)
  assign(nam, QC_2016[QC_2016$Size == siz,] )
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = num/den
  
  size=c(size,siz)
  prob=c(prob,round(tot,2))
  print(paste0("N. of failed wrt total for company ",nam, " is:", round(tot,2)))
}

library(ggplot2)

#creo i barplot per la probabilità condizionata di fallimento rispetto a size 
#utilizzo le prob calcolate sopra
pFailed_Size=NA
pFailed_Size=data.frame(cbind(prob,size))

p_plot<-ggplot(data=pFailed_Size, aes(x=size, y=prob)) +
  labs(title="Probability of Failure in 2016", x="SIZE", y = "PROB. OF FAILURE")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=prob), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  scale_x_discrete(limits=levels(QC_2016$Size))+
  theme(plot.title = element_text(hjust = 0.5))

p_plot

#applico il binomial test ad ogni probabilità di fallimento condizionata
#rispetto a size vs la probabilità di fallimento generale del 2016

num = nrow( QC_2016[QC_2016[["Failed"]] == 1,])
den = nrow( QC_2016)
tot = round(num/den,2)

n_test_size=length(unique(QC_2016$Size))
for (size in unique(QC_2016$Size)) {
 
      cat("\n")
      print(size)
      nam_1 = paste0("QC_",2016,"_",size)
      bin=binom.test(sum(get(nam_1)["Failed"]==1),nrow(get(nam_1)),p=tot,
                     conf.level =1-((1-0.95)/n_test_size))
      print(bin)
      
  
}


#probabilità condizionata rispetto a age

#creo dei bins per age
hist(QC_2016$Age, freq=FALSE, breaks="Sturges")
lines(density(QC_2016$Age, bw=2), col=4, lwd=2)
lines(density(QC_2016$Age, bw=1), col=6, lwd=2)
lines(density(QC_2016$Age, bw=0.5), col=8, lwd=2)


hist(QC_2016$Age, freq=T, breaks=quantile(QC_2016$Age, 0:5 / 5))

div=quantile(QC_2016$Age, 0:5 / 5)
div[1]=-1
QC_2016$AgeBins <- cut(QC_2016$Age,div,labels=c("[0,1]","[2,4]","[5,9]","[10,15]","[16,114]"))

table(QC_2016$AgeBins)
table(QC_2016$Failed,QC_2016$AgeBins)
table(QC_2016$Failed)

#creo i dataset con i diversi age_bins e calcolo la probabilità condizionata su ognuno
age_bin=c()
prob_age=c()

for (age in unique(QC_2016$AgeBins) ) {
  nam = paste0("QC_",2016,"_",age)
  assign(nam, QC_2016[QC_2016$AgeBins == age,] )
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = num/den
  
  age_bin=c(age_bin,age)
  prob_age=c(prob_age,round(tot,2))
  print(paste0("N. of failed wrt total for company ",nam, " is:", tot))
}


#creo i barplot per la probabilità condizionata di fallimento rispetto a age_bins 
#utilizzo le prob calcolate sopra
pFailed_Age=NA
pFailed_Age=data.frame(cbind(prob_age,age_bin))

p_plot<-ggplot(data=pFailed_Age, aes(x=age_bin, y=prob_age)) +
  labs(title="Probability of Failure in 2016", x="AGE", y = "PROB. OF FAILURE")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=prob_age), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  scale_x_discrete(limits=levels(QC_2016$AgeBins))+
  theme(plot.title = element_text(hjust = 0.5))

p_plot

#applico il binomial test ad ogni probabilità di fallimento condizionata
#rispetto a age_bins vs la probabilità di fallimento generale del 2016


n_test_age=length(unique(QC_2016$AgeBins))
for (age in unique(QC_2016$AgeBins)) {

      cat("\n")
      print(age)
      nam_1 = paste0("QC_",2016,"_",age)
      
      bin=binom.test(sum(get(nam_1)["Failed"]==1),nrow(get(nam_1)),p=tot,
                     conf.level =1-((1-0.95)/n_test_age))
      print(bin)
      
  
}






#probabilità condizionata rispetto a size fissando i vari legal form

#creo matrice per salvare le varie prob condizionate su size
P_form <- data.frame(matrix(ncol = length(unique(QC_2016$Size)), nrow = length(unique(QC_2016$`Legal form`))))                      
names_col <- c(unique(QC_2016$Size))
colnames(P_form) <- names_col
names_row=c(unique(QC_2016$`Legal form` ))
rownames(P_form) <- names_row


#creo sotto dataset e calcolo la prob condizionata su size wrt legal form
for (siz in unique(QC_2016$Size) ) {
  #prob=c()
  nam = paste0("QC_",2016,"_",siz)
  for (form in unique(get(nam)[["Legal form"]])){
    nam_2 = paste0(nam,"_",form)
    assign(nam_2, get(nam)[get(nam)[["Legal form"]] == form,])
    
    num = nrow(get(nam_2)[get(nam_2)[["Failed"]] == 1,])
    den = nrow( get(nam_2))
    tot = num/den
    
    P_form[form,siz]=round(tot,2)
    #prob=c(prob,round(tot,2))
    print(paste0("N. of failed wrt total for company ",nam, " with legal form ",nam_2," is: ", round(tot,2)))
  }
}

#per ogni size guardo quanti record (failed 0 -1) ho fissando legal form
for (siz in unique(QC_2016$Size) ) {
  nam = paste0("QC_",2016,"_",siz)
  print("*****************")
  print(siz)
  print(table(get(nam)$Failed,get(nam)[["Legal form"]]))
  cat("\n\n\n")
}

#creo i bar plot con la probabilità condizionata su size wrt legal form
#utilizzo la matrice con le prob creata prima
for (i in unique(QC_2016$`Legal form`)){
  ppp=NA
  ppp=data.frame(cbind(as.numeric(P_form[i,]),colnames(P_form)))
  p_plot<-ggplot(data=ppp, aes(x=X2, y=X1)) +
    labs(title=paste0("Probability of Failure in 2016 WITH LEGAL FORM = ",i), x="SIZE", y = "PROB. OF FAILURE")+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=X1), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    scale_x_discrete(limits=levels(QC_2016$Size))+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_plot)
  
}


#eseguo per ogni value su legal form il binomial test wrt la prob condizionata
#su size considerando qualunque legal form per il 2016
n_test_form=length(unique(QC_2016$`Legal form`))
for (size in unique(QC_2016$Size)) {
  nam = paste0("QC_",2016,"_",size)
  
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = round(num/den,2)
  
  for (form in unique(get(nam)$`Legal form`)){
    cat("\n")
    print(size)
    print(form)
    nam_2 = paste0(nam,"_",form)
      
    bin=binom.test(sum(get(nam_2)["Failed"]==1),nrow(get(nam_2)),p=tot,
                     conf.level =1-((1-0.95)/n_test_form))
    print(bin)
      
  }
}



#probabilità condizionata rispetto ad age fissando i vari legal form

#creo matrice per salvare le varie prob condizionate su age
P_form <- data.frame(matrix(ncol = length(unique(QC_2016$AgeBins)), nrow = length(unique(QC_2016$`Legal form`))))                      
P_form[is.na(P_form)] <- 0
names_col <- c(unique(QC_2016$AgeBins))
colnames(P_form) <- names_col
names_row=c(unique(QC_2016$`Legal form` ))
rownames(P_form) <- names_row


#creo sotto dataset e calcolo la prob condizionata su age wrt legal form

for (age in unique(QC_2016$AgeBins) ) {
  nam = paste0("QC_",2016,"_",age)
  for (form in unique(get(nam)[["Legal form"]])){
    nam_2 = paste0(nam,"_",form)
    assign(nam_2, get(nam)[get(nam)[["Legal form"]] == form,])
    
    num = nrow(get(nam_2)[get(nam_2)[["Failed"]] == 1,])
    den = nrow( get(nam_2))
    tot = num/den
    
    P_form[form,age]=round(tot,2)
    print(paste0("N. of failed wrt Age ",nam, " with legal form ",nam_2," is: ", round(tot,2)))
  }
}

#per ogni age guardo quanti record (failed 0 -1) ho fissando legal form
for (age in unique(QC_2016$AgeBins) ) {
  nam = paste0("QC_",2016,"_",age)
  print("*****************")
  print(age)
  print(table(get(nam)$Failed,get(nam)[["Legal form"]]))
  cat("\n\n\n")
}

#creo i bar plot con la probabilità condizionata su age wrt legal form
#utilizzo la matrice con le prob creata prima
for (i in unique(QC_2016$`Legal form`)){
  ppp=NA
  ppp=data.frame(cbind(as.numeric(P_form[i,]),colnames(P_form)))
  p_plot<-ggplot(data=ppp, aes(x=X2, y=X1)) +
    labs(title=paste0("Probability of Failure in 2016 WITH LEGAL FORM = ",i), x="AGE", y = "PROB. OF FAILURE")+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=X1), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    scale_x_discrete(limits=levels(QC_2016$AgeBins))+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_plot)
  
}


#eseguo per ogni value su legal form il binomial test wrt la prob condizionata
#su age considerando qualunque legal form per il 2016
n_test_form=length(unique(QC_2016$`Legal form`))
for (age in unique(QC_2016$AgeBins)) {
  nam = paste0("QC_",2016,"_",age)
  
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = round(num/den,2)
  
  for (form in unique(get(nam)$`Legal form`)){
    cat("\n")
    print(age)
    print(form)
    nam_2 = paste0(nam,"_",form)
    
    bin=binom.test(sum(get(nam_2)["Failed"]==1),nrow(get(nam_2)),p=tot,
                   conf.level =1-((1-0.95)/n_test_form))
    print(bin)
    
  }
}


#probabilità condizionata rispetto a size fissando i vari Ateco cat

#creo matrice per salvare le varie prob condizionate su size
P_cat <- data.frame(matrix(ncol = length(unique(QC_2016$Size)), nrow = length(unique(QC_2016$ATECO_CAT))))                      
P_cat[is.na(P_cat)] <- 0
names_col <- c(unique(QC_2016$Size))
colnames(P_cat) <- names_col
names_row=c(unique(QC_2016$ATECO_CAT))
rownames(P_cat) <- names_row


#creo sotto dataset e calcolo la prob condizionata su age wrt ATECO

for (size in unique(QC_2016$Size) ) {
  nam = paste0("QC_",2016,"_",size)
  for (cat in unique(get(nam)[["ATECO_CAT"]])){
    nam_2 = paste0(nam,"_",cat)
    assign(nam_2, get(nam)[get(nam)[["ATECO_CAT"]] == cat,])
    
    num = nrow(get(nam_2)[get(nam_2)[["Failed"]] == 1,])
    den = nrow( get(nam_2))
    tot = num/den
    
    P_cat[cat,size]=round(tot,2)
    print(paste0("N. of failed wrt Size ",nam, " with ATECO = ",nam_2," is: ", round(tot,2)))
  }
}

#per ogni size guardo quanti record (failed 0 -1) ho fissando ATECO
for (size in unique(QC_2016$Size) ) {
  nam = paste0("QC_",2016,"_",size)
  print("*****************")
  print(size)
  print(table(get(nam)$Failed,get(nam)[["ATECO_CAT"]]))
  cat("\n\n\n")
}

#creo i bar plot con la probabilità condizionata su size wrt ATECO
#utilizzo la matrice con le prob creata prima
for (i in unique(QC_2016$ATECO_CAT)){
  ppp=NA
  ppp=data.frame(cbind(as.numeric(P_cat[i,]),colnames(P_cat)))
  p_plot<-ggplot(data=ppp, aes(x=X2, y=X1)) +
    labs(title=paste0("Probability of Failure in 2016 WITH ATECO = ",i), x="SIZE", y = "PROB. OF FAILURE")+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=X1), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    scale_x_discrete(limits=levels(QC_2016$Size))+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_plot)
  
}


#eseguo per ogni value su legal form il binomial test wrt la prob condizionata
#su size considerando qualunque ATECO per il 2016
n_test_cat=length(unique(QC_2016$ATECO_CAT))
for (size in unique(QC_2016$Size)) {
  nam = paste0("QC_",2016,"_",size)
  
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = round(num/den,2)
  
  for (cat in unique(get(nam)$ATECO_CAT)){
    cat("\n")
    print(size)
    print(cat)
    nam_2 = paste0(nam,"_",cat)
    
    bin=binom.test(sum(get(nam_2)["Failed"]==1),nrow(get(nam_2)),p=tot,
                   conf.level =1-((1-0.95)/n_test_cat))
    print(bin)
    
  }
}







#probabilità condizionata rispetto a age fissando i vari Ateco cat

#creo matrice per salvare le varie prob condizionate su age
P_cat <- data.frame(matrix(ncol = length(unique(QC_2016$AgeBins)), nrow = length(unique(QC_2016$ATECO_CAT))))                      
P_cat[is.na(P_cat)] <- 0
names_col <- c(unique(QC_2016$AgeBins))
colnames(P_cat) <- names_col
names_row=c(unique(QC_2016$ATECO_CAT))
rownames(P_cat) <- names_row


#creo sotto dataset e calcolo la prob condizionata su age wrt ATECO

for (age in unique(QC_2016$AgeBins) ) {
  nam = paste0("QC_",2016,"_",age)
  for (cat in unique(get(nam)[["ATECO_CAT"]])){
    nam_2 = paste0(nam,"_",cat)
    assign(nam_2, get(nam)[get(nam)[["ATECO_CAT"]] == cat,])
    
    num = nrow(get(nam_2)[get(nam_2)[["Failed"]] == 1,])
    den = nrow( get(nam_2))
    tot = num/den
    
    P_cat[cat,age]=round(tot,2)
    print(paste0("N. of failed wrt Age ",nam, " with ATECO = ",nam_2," is: ", round(tot,2)))
  }
}

#per ogni age guardo quanti record (failed 0 -1) ho fissando ATECO
for (age in unique(QC_2016$AgeBins) ) {
  nam = paste0("QC_",2016,"_",age)
  print("*****************")
  print(age)
  print(table(get(nam)$Failed,get(nam)[["ATECO_CAT"]]))
  cat("\n\n\n")
}

#creo i bar plot con la probabilità condizionata su age wrt ATECO
#utilizzo la matrice con le prob creata prima
for (i in unique(QC_2016$ATECO_CAT)){
  ppp=NA
  ppp=data.frame(cbind(as.numeric(P_cat[i,]),colnames(P_cat)))
  p_plot<-ggplot(data=ppp, aes(x=X2, y=X1)) +
    labs(title=paste0("Probability of Failure in 2016 WITH ATECO = ",i), x="AGE", y = "PROB. OF FAILURE")+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=X1), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    scale_x_discrete(limits=levels(QC_2016$AgeBins))+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_plot)
  
}


#eseguo per ogni value su ateco cat il binomial test wrt la prob condizionata
#su age considerando qualunque ATECO per il 2016
n_test_cat=length(unique(QC_2016$ATECO_CAT))
for (age in unique(QC_2016$AgeBins)) {
  nam = paste0("QC_",2016,"_",age)
  
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = round(num/den,2)
  
  for (cat in unique(get(nam)$ATECO_CAT)){
    cat("\n")
    print(age)
    print(cat)
    nam_2 = paste0(nam,"_",cat)
    
    bin=binom.test(sum(get(nam_2)["Failed"]==1),nrow(get(nam_2)),p=tot,
                   conf.level =1-((1-0.95)/n_test_cat))
    print(bin)
    
  }
}





#probabilità condizionata rispetto a size fissando i vari Area


P_loc <- data.frame(matrix(ncol = length(unique(QC_2016$Size)),  #create empty df
                           nrow = length(unique(QC_2016$Area))))                      
names_col <- c(unique(QC_2016$Size))
colnames(P_loc) <- names_col
names_row=c(unique(QC_2016$Area ))
rownames(P_loc) <- names_row



for (siz in unique(QC_2016$Size) ) {
  #prob=c()
  nam = paste0("QC_",2016,"_",siz)
  for (loc in unique(get(nam)[["Area"]])){
    nam_2 = paste0(nam,"_",loc)
    assign(nam_2, get(nam)[get(nam)[["Area"]] == loc,])  
    
    num = nrow(get(nam_2)[get(nam_2)[["Failed"]] == 1,])
    den = nrow( get(nam_2))
    tot = num/den  #conditional probability
    
    P_loc[loc,siz]=round(tot,2)  #save probs in df for future plot
    #prob=c(prob,round(tot,2))   #save probs for future plot
    print(paste0("N. of failed wrt total for company ",nam, 
                 " with location ",nam_2," is: ", round(tot,2)))
  }
}  #create df with Probability of failure conditioned on Size wrt Area


for (siz in unique(QC_2016$Size) ) {
  nam = paste0("QC_",2016,"_",siz)
  print("*****************")
  print(siz)
  print(table(get(nam)$Failed,get(nam)[["Area"]]))
  cat("\n\n\n")
}  #check how many failed and active for every Area for every Size


for (loc in unique(QC_2016$Area)){
  ppp=NA
  ppp=data.frame(cbind(as.numeric(P_loc[loc,]),colnames(P_loc)))
  p_plot<-ggplot(data=ppp, aes(x=X2, y=X1)) +
    labs(title=paste0("Probability of Failure in 2016 WITH LOCATION = ",loc), 
         x="SIZE", y = "PROB. OF FAILURE")+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=X1), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    scale_x_discrete(limits=levels(QC_2016$Size))+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_plot) }  #barplots



n_test_loc=length(unique(QC_2016$Area))  

for (size in unique(QC_2016$Size)) {
  nam = paste0("QC_",2016,"_",size)
  
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = round(num/den,2)  #probability of failure conditioned to Size
  
  for (loc in unique(get(nam)$Area)){
    cat("\n")
    print(size)
    print(loc)
    nam_2 = paste0(nam,"_",loc)
    
    bin=binom.test(sum(get(nam_2)["Failed"]==1),nrow(get(nam_2)),p=tot,
                   conf.level =1-((1-0.95)/n_test_loc))  #Bonferroni correction
    print(bin)
    
  } #for every Size, consider the Area 
}  #bintest


###### GLO 1.09.22 ##### conditional prob of failure conditional to AGE wrt a specific location
table(QC_2016$AgeBins)


P_loc <- data.frame(matrix(ncol = length(unique(QC_2016$AgeBins)),  #create empty df
                           nrow = length(unique(QC_2016$Area))))                      
names_col <- c(unique(QC_2016$AgeBins))
colnames(P_loc) <- names_col
names_row=c(unique(QC_2016$Area ))
rownames(P_loc) <- names_row



for (bins in unique(QC_2016$AgeBins) ) {
  #prob=c()
  nam = paste0("QC_",2016,"_",bins)
  for (loc in unique(get(nam)[["Area"]])){
    nam_2 = paste0(nam,"_",loc)
    assign(nam_2, get(nam)[get(nam)[["Area"]] == loc,])  
    
    num = nrow(get(nam_2)[get(nam_2)[["Failed"]] == 1,])
    den = nrow( get(nam_2))
    tot = num/den  #conditional probability
    
    P_loc[loc,bins]=round(tot,2)  #save probs in df for future plot
    #prob=c(prob,round(tot,2))   #save probs for future plot
    print(paste0("N. of failed wrt total for Age in ", bins, 
                 " with location ", loc ," is: ", round(tot,2)))
  }
}  #create df with Probability of failure conditioned on AgeBin wrt Area


for (bins in unique(QC_2016$AgeBins) ) {
  nam = paste0("QC_",2016,"_",bins)
  print("*****************")
  print(bins)
  print(table(get(nam)$Failed,get(nam)[["Area"]]))
  cat("\n\n\n")
}  #check how many failed and active for every Area for every AgeBin


for (loc in unique(QC_2016$Area)){
  ppp=NA
  ppp=data.frame(cbind(as.numeric(P_loc[loc,]),colnames(P_loc)))
  p_plot<-ggplot(data=ppp, aes(x=X2, y=X1)) +
    labs(title=paste0("Probability of Failure in 2016 WITH LOCATION = ",loc), 
         x="AgeBin", y = "PROB. OF FAILURE")+
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=X1), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    scale_x_discrete(limits=levels(QC_2016$AgeBins))+
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_plot) }  #barplots



n_test_loc=length(unique(QC_2016$Area))  

for (bins in unique(QC_2016$AgeBins)) {
  nam = paste0("QC_",2016,"_",bins)
  
  num = nrow( get(nam)[get(nam)[["Failed"]] == 1,])
  den = nrow( get(nam))
  tot = round(num/den,2)  #probability of failure conditioned to Size
  
  for (loc in unique(get(nam)$Area)){
    cat("\n")
    print(bins)
    print(loc)
    nam_2 = paste0(nam,"_",loc)
    
    bin=binom.test(sum(get(nam_2)["Failed"]==1),nrow(get(nam_2)),p=tot,
                   conf.level =1-((1-0.95)/n_test_loc))  #Bonferroni correction
    print(bin)
    
  } #for every Size, consider the Area 
}  #bintest

       