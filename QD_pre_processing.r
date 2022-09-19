# clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# CARICO DATASET
load(file="QD.RData")
View(aida)

library(DescTools)

Abstract(aida)

#Tolgo features non rilevanti
aida$Size_numb=NULL
aida$`ATECO 2007code`=NULL
aida$Employees_mean=NULL
aida$`Registered office address - Region`=NULL
aida$`Legal status`=NULL

#guardo la distribuzione temporale dei dati
table(aida$Failed,aida$`Last accounting closing date`)

#elimino i dati pre 2007 e post 2018
aida=aida[aida$`Last accounting closing date`>=2007,]
aida=aida[aida$`Last accounting closing date`!=2019,]

table(aida$Failed,aida$`Last accounting closing date`)

#elimino le aziende che hanno meno di due anni di vita perchè hanno troppi missing value
sum(aida$Age<2)
aida=aida[aida$Age>=2,]


#sposto le colonne che non sono indici in fondo
Abstract(aida)
library(dplyr)
aida=aida %>% relocate(`Last accounting closing date`, .after = last_col())
aida=aida %>% relocate(`Legal form`, .after = last_col())


#calcolo la media sui 3 anni e il trend
x=1
while (x<70){
  x1=x+1
  x2=x+2
  media=rowMeans(cbind(aida[,x],aida[,x1],aida[,x2]),na.rm = T)

  #se l'anno -2 ha valori null prende il valore dall'anno -1
  aida[,x2]=coalesce(unlist(aida[,x2]),unlist(aida[,x1]))
  diff=aida[,x]-aida[,x2]
  div=diff/media
  n=gsub("Last avail. yr", '', colnames(aida)[x])
  if(x==1){
    media_data=data.frame(media)
    names(media_data)[1]=paste0(n,"_mean")
    
    trend=data.frame(div[,1])
    names(trend)[1]=paste0(n,"_trend")
  }
  media_data[paste0(n,"_mean")]=media
  trend[paste0(n,"_trend")]=div[,1]
  x=x+3
  
}

#libero un poò di memoria
rm(div)
rm(diff)
rm(media)

#unisco insieme i due dataframe media e trend calcolati sopra
merge=cbind(media_data,trend)

#libero memoria
rm(trend)
rm(media_data)


Abstract(merge)




#elimino le features con troppi missing values
merge$`Cost of debit (%)%_mean`=NULL
merge$`Banks/turnover%_trend`=NULL
merge$`Debt/EBITDA ratio%_trend`=NULL
merge$`Debt/equity ratio%_trend`=NULL
merge$`Return on investment (ROI) (%)%_trend`=NULL
merge$`Cost of debit (%)%_trend`=NULL
merge$`Banks/turnover%_mean`=NULL
merge$`Number of employees_trend`=NULL
merge$`Return on sales (ROS)%_trend`=NULL
merge$`Net financial positionth EUR_trend`=NULL
merge$`Return on investment (ROI) (%)%_mean`=NULL #controllare

Abstract(merge)
#aggiungo a merge le features mancanti
merge["Last accounting closing date"]=aida$`Last accounting closing date`
merge["Failed"]=aida$Failed
merge["Age"]=aida$Age
merge["Area"]=aida$Area
merge["ATECO_CAT"]=aida$ATECO_CAT
merge["Legal form"]=aida$`Legal form`
rm(aida)




save(merge, file="merge.RData") 
