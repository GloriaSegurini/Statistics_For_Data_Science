#clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# CARICO DATASET
load(file="QA.RData")
View(QA)

library(DescTools)
Abstract(QA)

table(QA$Failed,QA$`Last accounting closing date`)


#TENGO SOLO LE AZIENDE FALLITE
QB_FAILED=QA[QA[["Failed"]]==1,]
table(QB_FAILED$Failed)



# CONTROLLIAMO LA DISTRIBUZIONE DEGLI ANNI PER SELEZIONARNE DUE DA CONFRONTARE
summary(QB_FAILED$`Last accounting closing date`)
hist(QB_FAILED$`Last accounting closing date`, freq=FALSE, breaks="Sturges",main="Histogram of Last accounting closing date",xlab="Last accounting closing date")


summary(QB_FAILED)
table(QB_FAILED$`Last accounting closing date`)
QB_FAILED=QB_FAILED[QB_FAILED$`Last accounting closing date`>=2005,]
hist(QB_FAILED$`Last accounting closing date`, freq=FALSE, breaks="Sturges",main="Histogram of Last accounting closing date",xlab="Last accounting closing date")


#SCEGLIAMO GLI ANNI 2016 e 2009 PER LE NOSTRE ANALISI
QB_2016_2009=QB_FAILED[QB_FAILED$`Last accounting closing date`==2016 | QB_FAILED$`Last accounting closing date`==2009,]
Abstract(QB_2016_2009)

QB_2016=QB_2016_2009[QB_2016_2009$`Last accounting closing date`==2016,]
QB_2009=QB_2016_2009[QB_2016_2009$`Last accounting closing date`==2009,]

#ANALISI AGE PER I DUE DIVERSI ANNI
boxplot(QB_2009$Age,QB_2016$Age,main="Box Plot - Age",xlab="YEAR",names=c("2009","2016"))



#DENSITY FUNCTION CON DIVERSI VALORI DI BW TESTATI. ATTENZIONE AL TRONCAMENTO PRIMA DELLO ZERO!!!!!!!!!!!
hist(QB_2009$Age, freq=FALSE, breaks="Sturges")
lines(density(QB_2009$Age, bw=2), col=4, lwd=2)
lines(density(QB_2009$Age, bw=1), col=6, lwd=2)
lines(density(QB_2009$Age, bw=0.5), col=8, lwd=2)

hist(QB_2016$Age, freq=FALSE, breaks="Sturges")
lines(density(QB_2016$Age, bw=2), col=4, lwd=2)
lines(density(QB_2016$Age, bw=1), col=6, lwd=2)
lines(density(QB_2016$Age, bw=0.5), col=8, lwd=2)

#SCEGLIAMO BW=1 PER ENTRAMBE
plot(density(QB_2009$Age, bw=1), col=2, lwd=3,main="Kernel density estimation with Bandwidth = 1",xlab="AGE")
lines(density(QB_2016$Age, bw=1), col=9, lwd=3)
legend("topright",inset = 0.05,legend=c("2009","2016"),col=c("red","black"),cex = 1.5,text.font=4,lty = c(1, 1),lwd = 2)


#library(ks) # kernel smoothing library
#dens_failed = kde.boundary(QA_2016_FAILED$Age, h=1, xmin=0)
#plot(dens_failed, lwd=2)

# plug-in density estimate best bandwidth
#library(plugdensity)
#lines(density(QA_2016_FAILED$Age, bw=10), col=2, lwd=2)
#lines(plugin.density(QA_2016_FAILED$Age), col=3, lwd=2)


#RIFIUTO IPOTESI STESSA DISTRIBUZIONE
ks.test(QB_2009$Age,QB_2016$Age)

library(BSDA)


#TESTIAMO LA NORMALITA' ATTRAVERSO IL KS.TEST, MA LE IPOTESI SONO RIFIUTATE
fnorm_act = fitdist(QB_2009$Age, "norm") 
ks.test(QB_2009$Age, pnorm, fnorm_act$estimate[1], fnorm_act$estimate[2])

fnorm_fail = fitdist(QB_2016$Age, "norm") 
ks.test(QB_2016$Age, pnorm, fnorm_fail$estimate[1], fnorm_fail$estimate[2])

# UTILIZZO Z-TEST PERCHE' ABBIAMO MOLTI DATI. RIFIUTO CHE ABBIANO LA STESSA MEDIA
z.test(QB_2009$Age, QB_2016$Age, sigma.x=sd(QB_2009$Age), sigma.y=sd(QB_2016$Age))
#t.test(QA_2016_FAILED$Age, QA_2016_ACTIVE$Age) 


#CERCHIAMO DI CAPIRE LA DISTRIBUZIONE DI AGE PER ENTRAMBI
library(fitdistrplus) 
# Cullen and Frey graph
descdist(QB_2009$Age, boot=1000) 

descdist(QB_2016$Age, boot=1000) 


library(EnvStats)
fgamma=egamma(QB_2016$Age)
fgamma

library(actuar)
fexp=fitdist(QB_2016$Age, "exp")
fexp
fpareto = fitdist(QB_2016$Age, "pareto", start=list(shape=2, scale=1)) 
fpareto

#P-VALUE BASSISSIMI, RIFIUTO LE IPOTESI
ks.test(QB_2016$Age, ppareto, fpareto$estimate[1], fpareto$estimate[2])
ks.test(QB_2016$Age, pexp, fexp$estimate[1])
ks.test(QB_2016$Age, pgamma, fgamma$parameters[1], fgamma$parameters[2])




#ANALIZZIAMO SIZE
Abstract(QB_2016_2009)


QB_2016=QB_2016_2009[QB_2016_2009$`Last accounting closing date`==2016,]
QB_2009=QB_2016_2009[QB_2016_2009$`Last accounting closing date`==2009,]

table(QB_2016$Size_numb)/length((QB_2016$Size_numb))
table(QB_2009$Size_numb)/length((QB_2009$Size_numb))

#BARPLOT DI SIZE 
tab=table(QB_2016_2009$`Last accounting closing date`,QB_2016_2009$Size_numb)
bp = barplot(tab,y = "Num_companies",ylim = c(0,32000),beside=T, legend = c("2009",'2016'),main="Barplot of Size",xlab="Size of a company")

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
chi_quadro(QB_2009$Size_numb,QB_2016$Size_numb)
#RIFIUTO IL FATTO CHE ABBIAMO LA STESSA MEDIA
z.test(QB_2009$Size_numb, QB_2016$Size_numb, sigma.x=sd(QB_2009$Size_numb), sigma.y=sd(QB_2016$Size_numb))





## ANALISI DELLE DISTRIBUZIONI DI AGE IN BASE AL VALORE DI LEGAL FORM
table(QB_2016_2009$`Last accounting closing date`,QB_2016_2009$`Legal form`)

QB_2016_2009=QB_2016_2009[QB_2016_2009$`Legal form`!="Other" & QB_2016_2009$`Legal form`!="S.R.L. simplified",]
QB_2016_2009$`Legal form`=factor(QB_2016_2009$`Legal form`)

for (form in unique(QB_2016_2009$`Legal form`)) {
  for (val in unique(QB_2016_2009$`Last accounting closing date`)){
    
    nam = paste0("QB_2016_2009_",form,"_",val)
    assign (nam, QB_2016_2009[QB_2016_2009$`Legal form` == form & QB_2016_2009$`Last accounting closing date` == val,]  )
  }
}

for (form in unique(QB_2016_2009$`Legal form`)) {
  print("*******************************************")
  nam_0 = paste0("QB_2016_2009_",form,"_2009")
  nam_1 = paste0("QB_2016_2009_",form,"_2016")
  
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  plot(density((get(nam_0)[["Age"]]), bw=1), col=2, lwd=3,main="Kernel density estimation with Bandwidth = 1",xlab=paste("AGE WITH LEGAL FORM = ",form))
  lines(density((get(nam_1)[["Age"]]), bw=1), col=9, lwd=3)
  legend("topright",inset = 0.05,legend=c("2009","2016"),col=c("red","black"),cex = 1.5,text.font=4,lty = c(1, 1),lwd = 2)
  
  
}


for (form in unique(QB_2016_2009$`Legal form`)) {
  print("****************************************************************")
  nam_0 = paste0("QB_2016_2009_",form,"_2009")
  nam_1 = paste0("QB_2016_2009_",form,"_2016")
  
  #ks_result = ks.test(get(nam_0)[["Age"]],get(nam_1)[["Age"]])
  z_result = z.test(get(nam_1)[["Age"]], get(nam_0)[["Age"]], sigma.x = sd(get(nam_1)[["Age"]]), 
                    sigma.y = sd(get(nam_0)[["Age"]]),conf.level = 1-((1-0.95)/length(unique(QB_2016_2009$`Legal form`)))) 
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




#
## ANALISI DELLE DISTRIBUZIONI DI SIZE IN BASE AL VALORE DI LEGAL FORM
summary(QB_2016_2009$`Legal form`)

#USO I DATASET CREATI PRIMA PER OGNI VALORE DI LEGAL FORM
for (form in unique(QB_2016_2009$`Legal form` )) {
  print("*********************************************")
  nam_0 = paste0("QB_2016_2009_",form,"_2009")
  nam_1 = paste0("QB_2016_2009_",form,"_2016")
  name_all=paste0("QB_2016_2009_",form)
  assign (name_all, QB_2016_2009[QB_2016_2009$`Legal form` == form,]  )
  
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  #BARPLOT DI SIZE 
  tab = table((get(name_all)[["Last accounting closing date"]]),(get(name_all)[["Size_numb"]]))
  bp = barplot(tab,y = "Num_companies",ylim = c(0,22000), beside=T,legend = c("2009",'2016'),main="Barplot of Size",xlab=paste("SIZE OF COMPANY WITH LEGAL FORM = ",form))
  
}
for (form in unique(QB_2016_2009$`Legal form`)) {
  print("****************************************************************")
  nam_0 = paste0("QB_2016_2009_",form,"_2009")
  nam_1 = paste0("QB_2016_2009_",form,"_2016")
  #chi_quadro_result = chi_quadro(get(nam_0)[["Size_numb"]],get(nam_1)[["Size_numb"]])
  z_result = z.test(get(nam_1)[["Size_numb"]], get(nam_0)[["Size_numb"]], sigma.x = sd(get(nam_1)[["Size_numb"]]), 
                    sigma.y = sd(get(nam_0)[["Size_numb"]]),conf.level =1-((1-0.95)/length(unique(QB_2016_2009$`Legal form`)))  ) 
  print(nam_0)
  print(nam_1)
  #print("chi_quadro_result:")
  #print(chi_quadro_result)
  #print("P-VALUE BONFERRONI:")
  #print(p.adjust(chi_quadro_result$p.value, "bonferroni",n=length(unique(QA_2016$`Legal form`))))
  #cat("\n")
  print(z_result)
  
}









######### AREA - AGE ##############################
#creo dataframe per area e LACD
for (a in unique(QB_2016_2009$Area)) {
  for (y in unique(QB_2016_2009$`Last accounting closing date` )){  #2016,2009
    
    nam = paste0("QB_",a,"_",y)
    assign (nam, QB_2016_2009[QB_2016_2009$Area == a & 
                                QB_2016_2009$`Last accounting closing date` == y,]  )
  }
}


#control 
table(QB_2016_2009$`Last accounting closing date`,QB_2016_2009$Area)


for (loc in unique(QB_2016_2009$Area)) {
  print("**************")
  year2 = paste0("QB_", loc, "_2016")
  year1 = paste0("QB_", loc, "_2009")
  
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(year1)
  print(year2)
  #print(shap_0)
  #print(shap_1)
  
  plot(density((get(year1)[["Age"]]), bw=1), col=2, lwd=3,main="Kernel density estimation with Bandwidth = 1",xlab=paste("Age with location = ", loc))
  lines(density((get(year2)[["Age"]]), bw=1), col=9, lwd=3)
  legend("topright",inset = 0.05,legend=c("2009", "2016"),col=c("red","black"),cex = 1.5,text.font=4,lty = c(1, 1),lwd = 2)
  
  
}



###########################
for (loc in unique(QB_2016_2009$Area)) {
  print("**************")
  year2 = paste0("QB_", loc, "_2016")
  year1 = paste0("QB_", loc, "_2009")
  #View(nam_0["Age"])
  #ks_result = ks.test(get(year1)[["Age"]],get(year2)[["Age"]])
  z_result = z.test(get(year2)[["Age"]], get(year1)[["Age"]], sigma.x = sd(get(year2)[["Age"]]), 
                    sigma.y = sd(get(year1)[["Age"]]),
                    conf.level = 1-((1-0.95)/length(unique(QB_2016_2009$Area))))  #In 2016 and G category, failed companies are older on average
  print(year1)
  print(year2)
  #print("ks_result:")
  #print(ks_result)
  #print("P-VALUE BONFERRONI:")
  #print(p.adjust(ks_result$p.value, "bonferroni",n=length(unique(QA_2016$ATECO_CAT))))
  #cat("\n")
  print(z_result)
  
}


#############  SIZE ####################

for (loc in unique(QB_2016_2009$Area )) {
  print("**************")
  nam_0 = paste0("QB_",loc,2009)
  nam_1 = paste0("QB_",loc,2016)
  name_all=paste0("QB_", loc)  #make code more readable
  
  assign(name_all, QB_2016_2009[QB_2016_2009$Area == loc,]  )
  #shap_0=shapiro.test(get(nam_0)[["Age"]])
  #shap_1=shapiro.test(get(nam_1)[["Age"]])
  print(nam_0)
  print(nam_1)
  #print(shap_0)
  #print(shap_1)
  
  tab = table((get(name_all)[["Last accounting closing date"]]),
              (get(name_all)[["Size_numb"]]))
  bp = barplot(tab,y = "Num_companies",ylim = c(0,30000), 
               legend = c("2009", "2016"),
               main="Barplot of Size",
               xlab=paste("SIZE OF COMPANY WITH LOCATION = ",loc), beside=T)
  
}


for (loc in unique(QB_2016_2009$Area)) {
  print("**************")
  year2 = paste0("QB_", loc, "_2016")
  year1 = paste0("QB_", loc, "_2009")

  z_result = z.test(get(year2)[["Size_numb"]], get(year1)[["Size_numb"]], sigma.x = sd(get(year2)[["Size_numb"]]), 
                    sigma.y = sd(get(year1)[["Size_numb"]]),
                    conf.level = 1-((1-0.95)/length(unique(QB_2016_2009$Area))))  #In 2016 and G category, failed companies are older on average
  print(year1)
  print(year2)

  print(z_result)
  
}

