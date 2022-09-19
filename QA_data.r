# clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# CARICO DATASET
load(file="QA.RData")
View(QA)

library(DescTools)
Abstract(QA)

# ANALISI ATTRIBUTO AGE 
boxplot(QA$Age,main="Box Plot - Age")

# CONTROLLIAMO LA DISTRIBUZIONE DEGLI ANNI PER SELEZIONARNE UNO
summary(QA$`Last accounting closing date`)
hist(QA$`Last accounting closing date`, freq=FALSE, breaks="Sturges",main="Histogram of Last accounting closing date",xlab="Last accounting closing date")


summary(QA[["Last accounting closing date"]])
table(QA$`Last accounting closing date`)
QA=QA[QA$`Last accounting closing date`>=2005,]
hist(QA$`Last accounting closing date`, freq=FALSE, breaks="Sturges",main="Histogram of Last accounting closing date",xlab="Last accounting closing date")

table(QA[QA[["Last accounting closing date"]]==2011,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2012,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2013,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2014,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2015,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2016,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2017,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2018,]$`Failed`)
table(QA[QA[["Last accounting closing date"]]==2019,]$`Failed`)

#SCEGLIAMO L'ANNO 2016 PER LE NOSTRE ANALISI
QA_2016=QA[QA$`Last accounting closing date`==2016,]
Abstract(QA_2016)

table(QA_2016$Failed)/length((QA_2016$Failed))

# ANALIZIAMO AGE PER L'ANNO 2016
summary(QA_2016$Age)
QA_2016[QA_2016[["Age"]]>100,]

#CERCHIAMO DI CAPIRE LA DISTRIBUZIONE DI AGE
library(fitdistrplus) 
# Cullen and Frey graph
descdist(QA_2016$Age, boot=1000) 

#install.packages("EnvStats")

#GUARDIAMO SE FOSSE UNA GAMMA O UNA PARETO. PRIMA STIMO I PARAMETRI CON MLE PER ENTRAMBE, POI FACCIO KS.TEST
library(EnvStats)
fgamma=egamma(QA_2016$Age)
fgamma



library(actuar) 
fpareto = fitdist(QA_2016$Age, "pareto", start=list(shape=2, scale=1)) 
fpareto
#P-VALUE BASSISSIMI, RIFIUTO LE IPOTESI
ks.test(QA_2016$Age, ppareto, fpareto$estimate[1], fpareto$estimate[2])
ks.test(QA_2016$Age, pgamma, fgamma$parameters[1], fgamma$parameters[2])


boxplot(QA_2016$Age,main="Box Plot - Age in 2016")

#DIVIDO IL DATASET IN FAILED E ACTIVE PER CONFRONTARE LE DUE CURVE PER L'ANNO 2016
QA_2016_FAILED=QA_2016[QA_2016$Failed==1,]
QA_2016_ACTIVE=QA_2016[QA_2016$Failed==0,]

#DENSITY FUNCTION CON DIVERSI VALORI DI BW TESTATI. ATTENZIONE AL TRONCAMENTO PRIMA DELLO ZERO!!!!!!!!!!!
hist(QA_2016_FAILED$Age, freq=FALSE, breaks="Sturges")
lines(density(QA_2016_FAILED$Age, bw=10), col=2, lwd=2)
lines(density(QA_2016_FAILED$Age, bw=5), col=3, lwd=2)
lines(density(QA_2016_FAILED$Age, bw=2), col=4, lwd=2)
lines(density(QA_2016_FAILED$Age, bw=1), col=6, lwd=2)
lines(density(QA_2016_FAILED$Age, bw=0.75), col=7, lwd=2)
lines(density(QA_2016_FAILED$Age, bw=0.5), col=8, lwd=2)

hist(QA_2016_ACTIVE$Age, freq=FALSE, breaks="Sturges")
lines(density(QA_2016_ACTIVE$Age, bw=10), col=2, lwd=2)
lines(density(QA_2016_ACTIVE$Age, bw=5), col=3, lwd=2)
lines(density(QA_2016_ACTIVE$Age, bw=2), col=4, lwd=2)
lines(density(QA_2016_ACTIVE$Age, bw=1), col=6, lwd=2)
lines(density(QA_2016_ACTIVE$Age, bw=0.75), col=7, lwd=2)
lines(density(QA_2016_ACTIVE$Age, bw=0.5), col=8, lwd=2)

#SCEGLIAMO BW=1 PER ENTRAMBE
plot(density(QA_2016_ACTIVE$Age, bw=1), col=2, lwd=3,main="Kernel density estimation with Bandwidth = 1",xlab="AGE")
lines(density(QA_2016_FAILED$Age, bw=1), col=9, lwd=3)
legend("topright",inset = 0.05,legend=c("Active","Failed"),col=c("red","black"),cex = 1.5,text.font=4,lty = c(1, 1),lwd = 2)


#library(ks) # kernel smoothing library
#dens_failed = kde.boundary(QA_2016_FAILED$Age, h=1, xmin=0)
#plot(dens_failed, lwd=2)

# plug-in density estimate best bandwidth
#library(plugdensity)
#lines(density(QA_2016_FAILED$Age, bw=10), col=2, lwd=2)
#lines(plugin.density(QA_2016_FAILED$Age), col=3, lwd=2)


#RIFIUTO IPOTESI STESSA DISTRIBUZIONE
ks.test(QA_2016_FAILED$Age,QA_2016_ACTIVE$Age)

library(BSDA)

#NON POSSIAMO APPLICARE SHAPIRO TEST SU PIU' DI 5000 DATI
shapiro.test(QA_2016_FAILED$Age)
shapiro.test(QA_2016_ACTIVE$Age)

#TESTIAMO LA NORMALITA' ATTRAVERSO IL KS.TEST, MA LE IPOTESI SONO RIFIUTATE
fnorm_act = fitdist(QA_2016_ACTIVE$Age, "norm") 
ks.test(QA_2016_ACTIVE$Age, pnorm, fnorm_act$estimate[1], fnorm_act$estimate[2])

fnorm_fail = fitdist(QA_2016_FAILED$Age, "norm") 
ks.test(QA_2016_FAILED$Age, pnorm, fnorm_fail$estimate[1], fnorm_fail$estimate[2])

# UTILIZZO Z-TEST PERCHE' ABBIAMO MOLTI DATI. RIFIUTO CHE ABBIANO LA STESSA MEDIA
z.test(QA_2016_FAILED$Age, QA_2016_ACTIVE$Age, sigma.x=sd(QA_2016_FAILED$Age), sigma.y=sd(QA_2016_ACTIVE$Age))
#t.test(QA_2016_FAILED$Age, QA_2016_ACTIVE$Age) 




#ANALIZZIAMO SIZE
Abstract(QA_2016)

QA_2016_FAILED=QA_2016[QA_2016$Failed==1,]
QA_2016_ACTIVE=QA_2016[QA_2016$Failed==0,]

table(QA_2016_FAILED$Size_numb)/length((QA_2016_FAILED$Size_numb))
table(QA_2016_ACTIVE$Size_numb)/length((QA_2016_ACTIVE$Size_numb))

#BARPLOT DI SIZE NEL 2016
tab = table(QA_2016$Failed,QA_2016$Size)
bp = barplot(tab,y = "Num_companies",ylim = c(0,50000), legend = c("Active",'Failed'),main="Barplot of Size in 2016",xlab="Size of a company")

#ESEGUIAMO LA FUNZIONE PER IL CALCOLO DEL CHI QUADRO TRA DUE DISTRIBUZIONI DISCRETE
chi_quadro=function(data_1, data_2)
{
  
  #data_1=QA_2016_OTH_ACT$Size_numb
  #data_2=QA_2016_OTH_FAIL$Size_numb
  #n=length(data_1)
  #m=length(data_2)
  bins = sort(unique(c(data_1, data_2)))
  N1i = sapply(bins, function(i) sum(data_1==i))
  N2i = sapply(bins, function(i) sum(data_2==i))
  
  #xi2 = sum((sqrt(m/n)*N1i - sqrt(n/m)*N2i)^2/(N1i + N2i))
  #xi2
  #df = length(bins)-1
  #1-pchisq(xi2, df) # p-value
  # built-in function
  
  mat = cbind(N1i, N2i)
  #View(mat)
  chisq.test(mat)
  
}
#RIFIUTO L'IPOTESI CHE SU SIZE LE DUE DISTRIBUZIONI SIANO CONTINUE
chi_quadro(QA_2016_ACTIVE$Size_numb,QA_2016_FAILED$Size_numb)
#RIFIUTO IL FATTO CHE ABBIAMO LA STESSA MEDIA
z.test(QA_2016_FAILED$Size_numb, QA_2016_ACTIVE$Size_numb, sigma.x=sd(QA_2016_FAILED$Size_numb), sigma.y=sd(QA_2016_ACTIVE$Size_numb))




## ANALISI DELLE DISTRIBUZIONI IN BASE AL VALORE DI LEGAL FORM
Abstract(QA_2016)
summary(QA_2016$`Legal form`)

#CREO DEI DATASET PER OGNI VALORE DI LEGAL FORM

#QA_2016_CONS=QA_2016[QA_2016$`Legal form` =="Consortium",]
#QA_2016_OTH=QA_2016[QA_2016$`Legal form` =="Other",]
#QA_2016_SCA=QA_2016[QA_2016$`Legal form` =="S.C.A.R.L.",]
#QA_2016_SPA=QA_2016[QA_2016$`Legal form` =="S.P.A.",]
#QA_2016_SRL=QA_2016[QA_2016$`Legal form` =="S.R.L.",]
#QA_2016_SRL_ONE=QA_2016[QA_2016$`Legal form` =="S.R.L. one-person",]
#QA_2016_S.R.L._simp=QA_2016[QA_2016$`Legal form` =="S.R.L. simplified",]


for (form in unique(QA_2016$`Legal form`)) {
  for (val in unique(QA_2016$`Failed`)){

    nam = paste0("QA_2016_",form,"_",val)
    assign (nam, QA_2016[QA_2016$`Legal form` == form & QA_2016$Failed == val,]  )
  }
}

for (form in unique(QA_2016$`Legal form`)) {
  print("**************")
  nam_0 = paste0("QA_2016_",form,"_0")
  nam_1 = paste0("QA_2016_",form,"_1")
  
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  plot(density((get(nam_0)[["Age"]]), bw=1), col=2, lwd=3,main="Kernel density estimation with Bandwidth = 1",xlab=paste("AGE WITH LEGAL FORM = ",form))
  lines(density((get(nam_1)[["Age"]]), bw=1), col=9, lwd=3)
  legend("topright",inset = 0.05,legend=c("Active","Failed"),col=c("red","black"),cex = 1.5,text.font=4,lty = c(1, 1),lwd = 2)
  
  
}


for (form in unique(QA_2016$`Legal form`)) {
  print("****************************************************************")
  nam_0 = paste0("QA_2016_",form,"_0")
  nam_1 = paste0("QA_2016_",form,"_1")
  
  #ks_result = ks.test(get(nam_0)[["Age"]],get(nam_1)[["Age"]])
  z_result = z.test(get(nam_1)[["Age"]], get(nam_0)[["Age"]], sigma.x = sd(get(nam_1)[["Age"]]), 
                    sigma.y = sd(get(nam_0)[["Age"]]),conf.level = 1-((1-0.95)/length(unique(QA_2016$`Legal form`)))) 
  print(nam_0)
  print(nam_1)
  #print("ks_result:")
  #print(ks_result)
  #print("P-VALUE BONFERRONI:")
  #print(p.adjust(z_result$p.value, "bonferroni",n=length(unique(QA_2016$`Legal form`))))
  #cat("\n")
  print("Z-TEST:")
  print(z_result)
 
  
}



## ANALISI DELLE DISTRIBUZIONI IN BASE AL VALORE DI ATECO
for (cat in unique(QA_2016$ATECO_CAT)) {
  for (val in unique(QA_2016$`Failed`)){
    
    nam = paste0("QA_2016_",cat,"_",val)
    assign (nam, QA_2016[QA_2016$ATECO_CAT == cat & QA_2016$Failed == val,]  )
  }
}

for (cat in unique(QA_2016$ATECO_CAT)) {
  print("**************")
  nam_0 = paste0("QA_2016_",cat,"_0")
  nam_1 = paste0("QA_2016_",cat,"_1")
  
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  plot(density((get(nam_0)[["Age"]]), bw=1), col=2, lwd=3,main="Kernel density estimation with Bandwidth = 1",xlab=paste("AGE WITH ATECO = ",form))
  lines(density((get(nam_1)[["Age"]]), bw=1), col=9, lwd=3)
  legend("topright",inset = 0.05,legend=c("Active","Failed"),col=c("red","black"),cex = 1.5,text.font=4,lty = c(1, 1),lwd = 2)
  
  
}

for (cat in unique(QA_2016$ATECO_CAT)) {
  print("****************************************************************")
  nam_0 = paste0("QA_2016_",cat,"_",0)
  nam_1 = paste0("QA_2016_",cat,"_",1)
  #View(nam_0["Age"])
  #ks_result = ks.test(get(nam_0)[["Age"]],get(nam_1)[["Age"]])
  z_result = z.test(get(nam_1)[["Age"]], get(nam_0)[["Age"]], sigma.x = sd(get(nam_1)[["Age"]]), 
                    sigma.y = sd(get(nam_0)[["Age"]]),conf.level = 1-((1-0.95)/length(unique(QA_2016$ATECO_CAT))))  #In 2016 and G category, failed companies are older on average
  print(nam_0)
  print(nam_1)
  #print("ks_result:")
  #print(ks_result)
  #print("P-VALUE BONFERRONI:")
  #print(p.adjust(ks_result$p.value, "bonferroni",n=length(unique(QA_2016$ATECO_CAT))))
  #cat("\n")
  print(z_result)
  
}


#
## ANALISI DELLE DISTRIBUZIONI DI SIZE IN BASE AL VALORE DI LEGAL FORM
Abstract(QA_2016)
summary(QA_2016$`Legal form`)

#USO I DATASET CREATI PRIMA PER OGNI VALORE DI LEGAL FORM
for (form in unique(QA_2016$`Legal form` )) {
  print("**************")
  nam_0 = paste0("QA_2016_",form,"_0")
  nam_1 = paste0("QA_2016_",form,"_1")
  name_all=paste0("QA_2016_",form)
  
  assign (name_all, QA_2016[QA_2016$ATECO_CAT == cat,]  )
  
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  tab = table((get(name_all)[["Failed"]]),(get(name_all)[["Size_numb"]]))
  bp = barplot(tab,y = "Num_companies",ylim = c(0,30000), legend = c("Active",'Failed'),main="Barplot of Size in 2016",xlab=paste("SIZE OF COMPANY WITH LEGAL FORM = ",form))
  
}
for (form in unique(QA_2016$`Legal form`)) {
  print("****************************************************************")
  nam_0 = paste0("QA_2016_",form,"_0")
  nam_1 = paste0("QA_2016_",form,"_1")
  #chi_quadro_result = chi_quadro(get(nam_0)[["Size_numb"]],get(nam_1)[["Size_numb"]])
  z_result = z.test(get(nam_1)[["Size_numb"]], get(nam_0)[["Size_numb"]], sigma.x = sd(get(nam_1)[["Size_numb"]]), 
                    sigma.y = sd(get(nam_0)[["Size_numb"]]),conf.level =1-((1-0.95)/length(unique(QA_2016$`Legal form`)))  ) 
  print(nam_0)
  print(nam_1)
  #print("chi_quadro_result:")
  #print(chi_quadro_result)
  #print("P-VALUE BONFERRONI:")
  #print(p.adjust(chi_quadro_result$p.value, "bonferroni",n=length(unique(QA_2016$`Legal form`))))
  #cat("\n")
  print(z_result)
  
}

table(QA_2016$`Legal form`,QA_2016$Size)


## ANALISI DELLE DISTRIBUZIONI DI SIZE IN BASE AL VALORE DI ATECO
Abstract(QA_2016)
summary(QA_2016$ATECO_CAT)

for (cat in unique(QA_2016$ATECO_CAT)) {
  print("**************")
  nam_0 = paste0("QA_2016_",cat,"_0")
  nam_1 = paste0("QA_2016_",cat,"_1")
  name_all=paste0("QA_2016_",cat)
  
  assign (name_all, QA_2016[QA_2016$ATECO_CAT == cat,]  )
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  tab = table((get(name_all)[["Failed"]]),(get(name_all)[["Size_numb"]]))
  bp = barplot(tab,y = "Num_companies",ylim = c(0,10000), legend = c("Active",'Failed'),main="Barplot of Size in 2016",xlab=paste("SIZE OF COMPANY WITH ATECO = ",cat))
  
}
#USO I DATASET CREATI PRIMA PER OGNI VALORE DI ATECO
for (cat in unique(QA_2016$ATECO_CAT)) {
  print("****************************************************************")
  nam_0 = paste0("QA_2016_",cat,"_0")
  nam_1 = paste0("QA_2016_",cat,"_1")
  #chi_quadro_result = chi_quadro(get(nam_1)[["Size_numb"]], get(nam_0)[["Size_numb"]])
  z_result = z.test(get(nam_1)[["Size_numb"]], get(nam_0)[["Size_numb"]], sigma.x = sd(get(nam_1)[["Size_numb"]]), 
                    sigma.y = sd(get(nam_0)[["Size_numb"]]),conf.level =1-((1-0.95)/length(unique(QA_2016$ATECO_CAT)))  )  #In 2016 and G category, failed companies are older on average
  
  print(nam_0)
  print(nam_1)
  #print("chi_quadro_result:")
  #print(chi_quadro_result)
  #print("P-VALUE BONFERRONI:")
  #print(p.adjust(chi_quadro_result$p.value, "bonferroni",n=length(unique(QA_2016$ATECO_CAT))))
  #cat("\n")
  print(z_result)
  
}





