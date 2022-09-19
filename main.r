###########
# AIDA data loading
###########

# clean all
rm(list=ls())

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# to reload the whole dataset at once, run
load(file="aida.RData")
View(aida)

# summaries
table(aida$`Registered office address - Region`)
table(aida$`Legal status`)
table(aida$`Legal form`)

attach(aida)
summary(`Legal status`)

library(DescTools)
Abstract(aida)

#elimino i legal status non necessari per l'analisi
summary(aida$`Legal status`)
aida = aida[!aida$`Legal status`== 'Active (default of payments)', ]#delete rows
aida = aida[!aida$`Legal status`== 'Active (receivership)', ]
aida = aida[!aida$`Legal status`== 'Dissolved (demerger)', ]
aida = aida[!aida$`Legal status`== 'Dissolved (merger)', ]

#create new columns: 0 = active, 1 = failed
aida[['Failed']]= 0
aida$"Failed"[!aida$`Legal status`=='Active'] = 1
aida$Failed=factor(aida$Failed)
summary(aida$'Failed')

#elimino features non necessarie
aida[["Company name"]]=NULL
aida[["File"]]=NULL
aida[["Tax code number"]]=NULL

#non ci sono null value per quanto riguarda le regioni
sum(is.na(aida[["Registered office address - Region"]]))

aida[["Registered office address - Commune ISTAT code"]]=NULL
aida[["Province"]]=NULL

#elimino null value da Incorporation year per creare Age
sum(is.na(aida[["Incorporation year"]]))
aida=aida[!is.na(aida[["Incorporation year"]]),]

#non ci sono null value in Last accounting closing date
sum(is.na(aida[["Last accounting closing date"]]))

#creo age
aida["Age"]=aida$`Last accounting closing date` - aida$`Incorporation year`

#elimino record con age negativo
sum(aida$Age < 0)
aida=aida[aida$Age >= 0,]

#elimino incorporation year
aida[["Incorporation year"]]=NULL

#modifico ateco con il cod corrispondente trovato sul sito dell'istat
a=1:3
for (x in a)
  aida$`ATECO 2007code`= gsub(paste("^0",x,".*",sep=""),"A",aida$`ATECO 2007code`)
  

b=5:9
for (x in b)
  aida$`ATECO 2007code` = gsub(paste("^0",x,".*",sep=""),"B",aida$`ATECO 2007code`)

c=10:33
for (x in c)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"C",aida$`ATECO 2007code`)

d=35
for (x in d)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"D",aida$`ATECO 2007code`)

e=36:39
for (x in e)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"E",aida$`ATECO 2007code`)

f=41:43
for (x in f)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"F",aida$`ATECO 2007code`)

g=45:47
for (x in g)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"G",aida$`ATECO 2007code`)

h=49:53
for (x in h)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"H",aida$`ATECO 2007code`)

i=55:56
for (x in i)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"I",aida$`ATECO 2007code`)

j=58:63
for (x in j)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"J",aida$`ATECO 2007code`)

k=64:66
for (x in k)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"K",aida$`ATECO 2007code`)

l=68
for (x in l)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"L",aida$`ATECO 2007code`)

m=69:75
for (x in m)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"M",aida$`ATECO 2007code`)

n=77:82
for (x in n)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"N",aida$`ATECO 2007code`)

o=84
for (x in o)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"O",aida$`ATECO 2007code`)

p=85
for (x in p)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"P",aida$`ATECO 2007code`)

q=86:88
for (x in q)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"Q",aida$`ATECO 2007code`)

r=90:93
for (x in r)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"R",aida$`ATECO 2007code`)

s=94:96
for (x in s)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"S",aida$`ATECO 2007code`)

t=97:98
for (x in t)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"T",aida$`ATECO 2007code`)

u=99
for (x in u)
  aida$`ATECO 2007code` = gsub(paste("^",x,".*",sep=""),"U",aida$`ATECO 2007code`)

#elimino record con value diverso da ogni macro categoria
aida=aida[aida$`ATECO 2007code` != "000000",]

sum(is.na(aida[["ATECO 2007code"]]))
aida=aida[!is.na(aida[["ATECO 2007code"]]),]

aida$`ATECO 2007code`=factor(aida$`ATECO 2007code`)
summary(aida$`ATECO 2007code`)

Abstract(aida)



#raggruppo le regioni in zone
summary(aida$`Registered office address - Region`)
aida["Area"]=NA
Nord_Ovest=c("Piemonte","Valle d'Aosta/Vallée d'Aoste", "Liguria", "Lombardia")
Nord_Est=c("Emilia-Romagna","Friuli-Venezia Giulia", "Marche", "Trentino-Alto Adige","Veneto")
Centro=c("Abruzzo","Toscana", "Lazio", "Sardegna","Umbria")
Sud=c("Basilicata","Calabria", "Campania", "Molise","Puglia","Sicilia")

for (x in Nord_Ovest)
  aida$Area[aida$`Registered office address - Region`== x]="Nord_Ovest"
for (x in Nord_Est)
  aida$Area[aida$`Registered office address - Region`== x]="Nord_Est"
for (x in Centro)
  aida$Area[aida$`Registered office address - Region`== x]="Centro"
for (x in Sud)
  aida$Area[aida$`Registered office address - Region`== x]="Sud"

aida$Area=factor(aida$Area,c("Sud","Centro","Nord_Est","Nord_Ovest"))
summary(aida$`Area`)
plot(aida$Area)


summary(aida$`ATECO 2007code`)
plot(aida$`ATECO 2007code`)

#raggruppo tutte le macro aree di ateco con pochi record in others
cat_merge=c("A","B","D","E","K","O","P","Q","R","S","T","U")
aida["ATECO_CAT"]=aida["ATECO 2007code"]
levels(aida$`ATECO_CAT`)=c(levels(aida$`ATECO_CAT`),"Others")
for (x in cat_merge)
  aida$ATECO_CAT[aida$`ATECO_CAT`== x]="Others"


aida$`ATECO_CAT`=factor(aida$`ATECO_CAT`)
summary(aida$`ATECO_CAT`)
plot(aida$`ATECO_CAT`)



#raggruppo tutti i legal form con pochi values in others
summary(aida$`Legal form`)

sum(is.na(aida[["Legal form"]]))
aida=aida[!is.na(aida[["Legal form"]]),]
plot(aida$`Legal form`)

aida$`Legal form`[aida$`Legal form`=="Association"] = "Other"
aida$`Legal form`[aida$`Legal form`=="Foreign company"] = "Other"
aida$`Legal form`[aida$`Legal form`=="Foundation"] = "Other"
aida$`Legal form`[aida$`Legal form`=="Mutual aid society"] = "Other"
aida$`Legal form`[aida$`Legal form`=="Public agency"] = "Other"
aida$`Legal form`[aida$`Legal form`=="S.A.P.A."] = "Other"
aida$`Legal form`[aida$`Legal form`=="S.N.C."] = "Other"
aida$`Legal form`[aida$`Legal form`=="S.A.S."] = "Other"

#unisco tra loro legal form dello stesso macro tipo con pochi record
aida$`Legal form`[aida$`Legal form`=="S.C.A.R.I."] = "S.C.A.R.L."
aida$`Legal form`[aida$`Legal form`=="S.C.A.R.L.P.A."] = "S.C.A.R.L."
aida$`Legal form`[aida$`Legal form`=="Social cooperative company"] = "S.C.A.R.L."


aida$`Legal form`=factor(aida$`Legal form`)
summary(aida$`Legal form`)
plot(aida$`Legal form`)

Abstract(aida)

#calcolo numero medio di impiegati negli ultimi 3 anni
sum(is.na(aida[["Number of employeesLast avail. yr"]]) & is.na(aida[["Number of employeesYear - 1"]]) & is.na(aida[["Number of employeesYear - 2"]]))
aida=aida[!(is.na(aida[["Number of employeesLast avail. yr"]]) & is.na(aida[["Number of employeesYear - 1"]]) & is.na(aida[["Number of employeesYear - 2"]])),]
aida["Employees_mean"]=rowMeans(subset(aida,select=c(`Number of employeesLast avail. yr`,`Number of employeesYear - 1`,`Number of employeesYear - 2`)),na.rm=T)



summary(aida["Employees_mean"])
sum(aida["Employees_mean"]==0)
sum(is.na(aida$Employees_mean))

#calcolo la size dell'azienda in base al numero medio di impiegati
aida["Size"]=NA
aida$Size[aida$Employees_mean<1]="Single"
aida$Size[(aida$Employees_mean>= 1 & aida$Employees_mean<3)]="Micro"
aida$Size[(aida$Employees_mean>= 3 & aida$Employees_mean<6)]="Small"
aida$Size[(aida$Employees_mean>= 6 & aida$Employees_mean<10)]="Medium"
aida$Size[(aida$Employees_mean>= 10 & aida$Employees_mean<25)]="Large"
aida$Size[(aida$Employees_mean>= 25)]="Extra_Large"

aida$Size=factor(aida$Size,c("Single","Micro","Small","Medium","Large","Extra_Large"))

tab = table(aida$Failed,aida$Size)
bp = barplot(tab,y = "Num_companies",ylim = c(0,1000000), legend = c("Active",'Failed'),main="Barplot of Size",xlab="Size of a company")

#ASSEGNO UN VALORE NUMERICO INVECE CHE CATEGORICO
aida["Size_numb"]=NA
aida$`Size_numb`[aida$`Size`=="Single"] = 0
aida$`Size_numb`[aida$`Size`=="Micro"] = 1
aida$`Size_numb`[aida$`Size`=="Small"] = 2
aida$`Size_numb`[aida$`Size`=="Medium"] = 3
aida$`Size_numb`[aida$`Size`=="Large"] = 4
aida$`Size_numb`[aida$`Size`=="Extra_Large"] = 5
Abstract(aida)


QA=subset(aida,select=c("ATECO 2007code","ATECO_CAT","Legal form","Legal status","Failed","Age","Size","Last accounting closing date","Area","Registered office address - Region","Size_numb"))
Abstract(QA)
save(QA, file="QA.RData") 

save(aida, file="QD.RData") 

