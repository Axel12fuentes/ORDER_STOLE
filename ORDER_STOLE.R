
library(tidyverse)
library(lubridate)
library(openxlsx)
library(plyr)
library(plotly)
# Establecer directorio


### BD QLICK

db=read.xlsx("D:\\BITEL\\KPI\\ho_do.xlsx",sheet =7)

### CONVERTIR EN FECHAS
fecha=as.Date(db$create_date, origin = "1899-12-30")
### CONVERTIR EN FECHAS EN DATA FRAME
fecha=as.data.frame(fecha)
### UNIR LA BASE DE FECHAS A LA BASE DE DATOS
db2=data.frame(db,fecha)
### COLUMNAS QUE UTILIZAREMOS CAMBIANDO EL NOMBRE A LAS COLUMNAS

names(db2)[9]='N_ORDEN'
names(db2)[12]='ESTADO'
names(db2)[22]='NUM_PORTAR'
names(db2)[28]='BRANCH'
names(db2)[30]='VENDEDOR'
names(db2)[50]='NOMBRE'
names(db2)[8]='DV_BRANCH'

##################################################
##### ORDENES ROBADAS PUNBR MADBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH "PUNBR","MADBR"
bd4=filter(db2, BRANCH%in%c("PUNBR","MADBR"),status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CPUN=bd4[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE PUNBR PARA CRUZAR DATOS
CPUN2=CPUN["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CPUN2=CPUN2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS PUNBR Y MADBR

db5=db5[!grepl("PUNBR|MADBR",db5$BRANCH),]

### UBICAMOS SOLO DNI DE LA BASE SIN PUN Y MAD

db6<-db5["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE PUN Y MAD CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLE=db6 %>% 
  mutate(ROBO= deframe(CPUN2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJ=STOLE%>% 
  full_join(db5, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBPUN=filter(STOLEJ,ROBO=="1")
ROBPUN<- ROBPUN %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBPUN=ROBPUN[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### EXPORTAMOS A CSV
setwd('D:/BITEL/KPI/ROBOS/PUNO')
write.xlsx(ROBPUN, file = "ROBPUN.xlsx", row.names = FALSE)
write.xlsx(CPUN, file = "CPUN.xlsx", row.names = FALSE)








##################################################
##### ORDENES ROBADAS ANCBR HUABR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH "ANCBR","HUABR"
bd4A=filter(db2, BRANCH%in%c("ANCBR","HUABR"),status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CANC=bd4A[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE PUNBR PARA CRUZAR DATOS
CANC2=CANC["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CANC2=CANC2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5A<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

###  SACAMOS "ANCBR","HUABR"
db5A=db5A[!grepl("ANCBR|HUABR",db5A$BRANCH),]

###  UBICAMOS SOLO DNI DE LA BASE SIN ANCBR HUABR

db6A<-db5A["ID_NO"]

###  CRUZAMOS LOS DATOS |BASE DE ANCBR HUABR CRUZADA CON LA OTRA BASE PARA VER
###  PARA VER ROBO

STOLEA=db6A %>% 
  mutate(ROBO= deframe(CANC2)[ID_NO])

###  UNIENDO STOLE y DB5

STOLEJA=STOLEA%>% 
  full_join(db5A, by="ID_NO")

###  FILTRANDO LAS ORDENES ROBADAS

ROBANC=filter(STOLEJA,ROBO=="1")
ROBANC<- ROBANC %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBANC=ROBANC[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### EXPORTAMOS A CSV
setwd('D:/BITEL/KPI/ROBOS/ANCASH')
write.xlsx(ROBANC, file = "ROBANC.xlsx", row.names = FALSE)
write.xlsx(CANC, file = "CANC.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS AREBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH AREBR
bd4ARE=filter(db2, BRANCH=="AREBR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CARE=bd4ARE[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE PUNBR PARA CRUZAR DATOS
CARE2=CARE["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CARE2=CARE2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5ARE<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS AREBR

db5ARE=db5ARE[!(db5ARE$BRANCH=="AREBR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN AREBR

db6ARE<-db5ARE["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE AREBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEARE=db6ARE %>% 
  mutate(ROBO= deframe(CARE2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJARE=STOLEARE%>% 
  full_join(db5ARE, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBARE=filter(STOLEJARE,ROBO=="1")
ROBARE<- ROBARE %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBARE=ROBARE[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/AREQUIPA')
write.xlsx(ROBARE, file = "ROBARE.xlsx", row.names = FALSE)
write.xlsx(CARE, file = "CARE.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS AYABR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH AYABR
bd4AYA=filter(db2, BRANCH=="AYABR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CAYA=bd4AYA[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE AYABR PARA CRUZAR DATOS
CAYA2=CAYA["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CAYA2=CAYA2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5AYA<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS AYABR

db5AYA=db5AYA[!(db5AYA$BRANCH=="AYABR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN AYABR

db6AYA<-db5AYA["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE AYABR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEAYA=db6AYA %>% 
  mutate(ROBO= deframe(CAYA2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJAYA=STOLEAYA%>% 
  full_join(db5AYA, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBAYA=filter(STOLEJAYA,ROBO=="1")
ROBAYA<- ROBAYA %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBAYA=ROBAYA[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/AYACUCHO')
write.xlsx(ROBAYA, file = "ROBAYA.xlsx", row.names = FALSE)
write.xlsx(CAYA, file = "CAYA.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS CAJBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH CAJBR
bd4CAJ=filter(db2, BRANCH=="CAJBR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CCAJ=bd4CAJ[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE CAJBR PARA CRUZAR DATOS
CCAJ2=CCAJ["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CCAJ2=CCAJ2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5CAJ<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS CAJBR

db5CAJ=db5CAJ[!(db5CAJ$BRANCH=="CAJBR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN CAJBR

db6CAJ<-db5CAJ["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE CAJBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLECAJ=db6CAJ %>% 
  mutate(ROBO= deframe(CCAJ2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJCAJ=STOLECAJ%>% 
  full_join(db5CAJ, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBCAJ=filter(STOLEJCAJ,ROBO=="1")
ROBCAJ<- ROBCAJ %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBCAJ=ROBCAJ[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/CAJAMARCA')
write.xlsx(ROBCAJ, file = "ROBCAJ.xlsx", row.names = FALSE)
write.xlsx(CCAJ, file = "CCAJ.xlsx", row.names = FALSE)










##################################################
##### ORDENES ROBADAS CUSBR APUBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH "CUSBR APUBR"
bd4CUS=filter(db2, BRANCH%in%c("CUSBR","APUBR"),status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CCUS=bd4CUS[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE CUSBR APUBR PARA CRUZAR DATOS
CCUS2=CCUS["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CCUS2=CCUS2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5CUS<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS CUSBR|APUBR

db5CUS=db5CUS[!grepl("CUSBR|APUBR",db5CUS$BRANCH),]

### UBICAMOS SOLO DNI DE LA BASE SIN CUSBR|APUBR

db6CUS<-db5CUS["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE CUSBR APUBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLECUS=db6CUS %>% 
  mutate(ROBO= deframe(CCUS2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJCUS=STOLECUS%>% 
  full_join(db5CUS, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBCUS=filter(STOLEJCUS,ROBO=="1")
ROBCUS<- ROBCUS %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBCUS=ROBCUS[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### EXPORTAMOS A CSV

setwd('D:/BITEL/KPI/ROBOS/CUSCO')
write.xlsx(ROBCUS, file = "ROBCUS.xlsx", row.names = FALSE)
write.xlsx(CCUS, file = "CCUS.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS HUNBR UCABR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH "HUNBR UCABR"
bd4HUN=filter(db2, BRANCH%in%c("HUNBR","UCABR"),status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CHUN=bd4HUN[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE HUNBR UCABR PARA CRUZAR DATOS
CHUN2=CHUN["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CHUN2=CHUN2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5HUN<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS HUNBR UCABR

db5HUN=db5HUN[!grepl("HUNBR|UCABR",db5HUN$BRANCH),]

### UBICAMOS SOLO DNI DE LA BASE SIN HUNBR UCABR

db6HUN<-db5HUN["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE HUNBR UCABR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEHUN=db6HUN %>% 
  mutate(ROBO= deframe(CHUN2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJHUN=STOLEHUN%>% 
  full_join(db5HUN, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBHUN=filter(STOLEJHUN,ROBO=="1")
ROBHUN<- ROBHUN %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBHUN=ROBHUN[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### EXPORTAMOS A CSV
setwd('D:/BITEL/KPI/ROBOS/HUANUCO')
write.xlsx(ROBHUN, file = "ROBHUN.xlsx", row.names = FALSE)
write.xlsx(CHUN, file = "CHUN.xlsx", row.names = FALSE)








##################################################
##### ORDENES ROBADAS ICABR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH ICABR
bd4ICA=filter(db2, BRANCH=="ICABR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CICA=bd4ICA[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE ICABR PARA CRUZAR DATOS
CICA2=CICA["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CICA2=CICA2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5ICA<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS ICABR

db5ICA=db5ICA[!(db5ICA$BRANCH=="ICABR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN ICABR

db6ICA<-db5ICA["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE ICABR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEICA=db6ICA %>% 
  mutate(ROBO= deframe(CICA2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJICA=STOLEICA%>% 
  full_join(db5ICA, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBICA=filter(STOLEJICA,ROBO=="1")
ROBICA<- ROBICA %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBICA=ROBICA[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/ICA')
write.xlsx(ROBICA, file = "ROBICA.xlsx", row.names = FALSE)
write.xlsx(CICA, file = "CICA.xlsx", row.names = FALSE)










##################################################
##### ORDENES ROBADAS JUNBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH JUNBR
bd4JUN=filter(db2, BRANCH=="JUNBR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CJUN=bd4JUN[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE JUNBR PARA CRUZAR DATOS
CJUN2=CJUN["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CJUN2=CJUN2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5JUN<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS JUNBR

db5JUN=db5JUN[!(db5JUN$BRANCH=="JUNBR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN JUNBR

db6JUN<-db5JUN["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE JUNBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEJUN=db6JUN %>% 
  mutate(ROBO= deframe(CJUN2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJJUN=STOLEJUN%>% 
  full_join(db5JUN, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBJUN=filter(STOLEJJUN,ROBO=="1")
ROBJUN<- ROBJUN %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBJUN=ROBJUN[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/JUNIN')
write.xlsx(ROBJUN, file = "ROBJUN.xlsx", row.names = FALSE)
write.xlsx(CJUN, file = "CJUN.xlsx", row.names = FALSE)










##################################################
##### ORDENES ROBADAS LALBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH LALBR
bd4LAL=filter(db2, BRANCH=="LALBR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CLAL=bd4LAL[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE LALBR PARA CRUZAR DATOS
CLAL2=CLAL["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CLAL2=CLAL2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5LAL<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS LALBR

db5LAL=db5LAL[!(db5LAL$BRANCH=="LALBR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN LALBR

db6LAL<-db5LAL["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE LALBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLELAL=db6LAL %>% 
  mutate(ROBO= deframe(CLAL2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJLAL=STOLELAL%>% 
  full_join(db5LAL, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBLAL=filter(STOLEJLAL,ROBO=="1")
ROBLAL<- ROBLAL %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBLAL=ROBLAL[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LA_LIBERTAD')
write.xlsx(ROBLAL, file = "ROBLAL.xlsx", row.names = FALSE)
write.xlsx(CLAL, file = "CLAL.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS LAMBR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH LAMBR
bd4LAM=filter(db2, BRANCH=="LAMBR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CLAM=bd4LAM[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE LAMBR PARA CRUZAR DATOS
CLAM2=CLAM["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CLAM2=CLAM2%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5LAM<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS LAMBR

db5LAM=db5LAM[!(db5LAM$BRANCH=="LAMBR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN LAMBR

db6LAM<-db5LAM["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE LALBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLELAM=db6LAM %>% 
  mutate(ROBO= deframe(CLAM2)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJLAM=STOLELAM%>% 
  full_join(db5LAM, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBLAM=filter(STOLEJLAM,ROBO=="1")
ROBLAM<- ROBLAM %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBLAM=ROBLAM[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LAMBAYEQUE')
write.xlsx(ROBLAM, file = "ROBLAM.xlsx", row.names = FALSE)
write.xlsx(CLAM, file = "CLAM.xlsx", row.names = FALSE)








##################################################
##### ORDENES ROBADAS L1BR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH L1BR
bd4L1=filter(db2, BRANCH=="LI1BR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CL1=bd4L1[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE L1BR PARA CRUZAR DATOS
CL12=CL1["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CL12=CL12%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5L1<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS L1BR

db5L1=db5L1[!(db5L1$BRANCH=="LI1BR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN L1BR

db6L1<-db5L1["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE LALBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEL1=db6L1 %>% 
  mutate(ROBO= deframe(CL12)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJL1=STOLEL1%>% 
  full_join(db5L1, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBL1=filter(STOLEJL1,ROBO=="1")
ROBL1<- ROBL1 %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBL1=ROBL1[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LIMA1')
write.xlsx(ROBL1, file = "ROBL1.xlsx", row.names = FALSE)
write.xlsx(CL1, file = "CL1.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS LI2BR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH LI2BR
bd4L2=filter(db2, BRANCH=="LI2BR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CL2=bd4L2[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE LI2BR PARA CRUZAR DATOS
CL22=CL2["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CL22=CL22%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5L2<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS LI2BR

db5L2=db5L2[!(db5L2$BRANCH=="LI2BR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN LI2BR

db6L2<-db5L2["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE LI2BR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEL2=db6L2 %>% 
  mutate(ROBO= deframe(CL22)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJL2=STOLEL2%>% 
  full_join(db5L2, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBL2=filter(STOLEJL2,ROBO=="1")
ROBL2<- ROBL2 %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBL2=ROBL2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LIMA2')
write.xlsx(ROBL2, file = "ROBL2.xlsx", row.names = FALSE)
write.xlsx(CL2, file = "CL2.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS LI3BR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH LI3BR
bd4L3=filter(db2, BRANCH=="LI3BR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CL3=bd4L3[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE LI3BR PARA CRUZAR DATOS
CL32=CL3["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CL32=CL32%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5L3<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS LI3BR

db5L3=db5L3[!(db5L3$BRANCH=="LI3BR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN LI3BR

db6L3<-db5L3["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE LI3BR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEL3=db6L3 %>% 
  mutate(ROBO= deframe(CL32)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJL3=STOLEL3%>% 
  full_join(db5L3, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBL3=filter(STOLEJL3,ROBO=="1")
ROBL3<- ROBL3 %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBL3=ROBL3[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LIMA3')
write.xlsx(ROBL3, file = "ROBL3.xlsx", row.names = FALSE)
write.xlsx(CL3, file = "CL3.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS LI4BR
##################################################

### PONER LOS NOMBRE DE LAS BRANCH LI4BR
bd4L4=filter(db2, BRANCH=="LI4BR",status_id%in%c("6","4","-1"))%>%
  filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())

### SELECCIONAMOS LAS COLUMNAS

CL4=bd4L4[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]

### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE LI4BR PARA CRUZAR DATOS
CL42=CL4["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CL42=CL42%>% 
  mutate(ROBO=1)

### CREAMOS UNA BASE CON LA Q SE CRUZARA

db5L4<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]

### SACAMOS LI4BR

db5L4=db5L4[!(db5L4$BRANCH=="LI4BR"),]

### UBICAMOS SOLO DNI DE LA BASE SIN LI4BR

db6L4<-db5L4["ID_NO"]

### CRUZAMOS LOS DATOS |BASE DE LI4BR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO

STOLEL4=db6L4 %>% 
  mutate(ROBO= deframe(CL42)[ID_NO])

### UNIENDO STOLE y DB5

STOLEJL4=STOLEL4%>% 
  full_join(db5L4, by="ID_NO")

### FILTRANDO LAS ORDENES ROBADAS

ROBL4=filter(STOLEJL4,ROBO=="1")
ROBL4<- ROBL4 %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBL4=ROBL4[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LIMA4')
write.xlsx(ROBL4, file = "ROBL4.xlsx", row.names = FALSE)
write.xlsx(CL4, file = "CL4.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS LORBR
##################################################
### PONER LOS NOMBRE DE LAS BRANCH LORBR
bd4LO=filter(db2, BRANCH=="LORBR",status_id%in%c("6","4","-1"))%>%
filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())
### SELECCIONAMOS LAS COLUMNAS
CLO=bd4LO[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]
### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE LORBR PARA CRUZAR DATOS
CLO2=CLO["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CLO2=CLO2%>%
mutate(ROBO=1)
### CREAMOS UNA BASE CON LA Q SE CRUZARA
db5LO<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### SACAMOS LORBR
db5LO=db5LO[!(db5LO$BRANCH=="LORBR"),]
### UBICAMOS SOLO DNI DE LA BASE SIN LORBR
db6LO<-db5LO["ID_NO"]
### CRUZAMOS LOS DATOS |BASE DE LORBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO
STOLELO=db6LO %>%
mutate(ROBO= deframe(CLO2)[ID_NO])
### UNIENDO STOLE y DB5
STOLEJLO=STOLELO%>%
full_join(db5LO, by="ID_NO")
### FILTRANDO LAS ORDENES ROBADAS
ROBLO=filter(STOLEJLO,ROBO=="1")
ROBLO<- ROBLO %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBLO=ROBLO[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/LORETO')
write.xlsx(ROBLO, file = "ROBLO.xlsx", row.names = FALSE)
write.xlsx(CLO, file = "CLO.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS PIUBR
##################################################
### PONER LOS NOMBRE DE LAS BRANCH PIUBR
bd4PI=filter(db2, BRANCH=="PIUBR",status_id%in%c("6","4","-1"))%>%
filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())
### SELECCIONAMOS LAS COLUMNAS
CPI=bd4PI[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]
### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE PIUBR PARA CRUZAR DATOS
CPI2=CPI["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CPI2=CPI2%>%
mutate(ROBO=1)
### CREAMOS UNA BASE CON LA Q SE CRUZARA
db5PI<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### SACAMOS PIUBR
db5PI=db5PI[!(db5PI$BRANCH=="PIUBR"),]
### UBICAMOS SOLO DNI DE LA BASE SIN PIUBR
db6PI<-db5PI["ID_NO"]
### CRUZAMOS LOS DATOS |BASE DE PIUBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO
STOLEPI=db6PI %>%
mutate(ROBO= deframe(CPI2)[ID_NO])
### UNIENDO STOLE y DB5
STOLEJPI=STOLEPI%>%
full_join(db5PI, by="ID_NO")
### FILTRANDO LAS ORDENES ROBADAS
ROBPI=filter(STOLEJPI,ROBO=="1")
ROBPI<- ROBPI %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBPI=ROBPI[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/PIURA')
write.xlsx(ROBPI, file = "ROBPI.xlsx", row.names = FALSE)
write.xlsx(CPI, file = "CPI.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS SANBR AMABR
##################################################
### PONER LOS NOMBRE DE LAS BRANCH "SANBR AMABR"
bd4SA=filter(db2, BRANCH%in%c("SANBR","AMABR"),status_id%in%c("6","4","-1"))%>%
filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())
### SELECCIONAMOS LAS COLUMNAS
CSAN=bd4SA[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]
### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE SANBR AMABR PARA CRUZAR DATOS
CSAN2=CSAN["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CSAN2=CSAN2%>%
mutate(ROBO=1)
### CREAMOS UNA BASE CON LA Q SE CRUZARA
db5SA<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### SACAMOS SANBR AMABR
db5SA=db5SA[!grepl("SANBR|AMABR",db5SA$BRANCH),]
### UBICAMOS SOLO DNI DE LA BASE SIN SANBR AMABR
db6SA<-db5SA["ID_NO"]
### CRUZAMOS LOS DATOS |BASE DE SANBR AMABR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO
STOLSA=db6SA %>%
mutate(ROBO= deframe(CSAN2)[ID_NO])
### UNIENDO STOLE y DB5
STOLEJSA=STOLSA%>%
full_join(db5SA, by="ID_NO")
### FILTRANDO LAS ORDENES ROBADAS
ROBSA=filter(STOLEJSA,ROBO=="1")
ROBSA<- ROBSA %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBSA=ROBSA[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### EXPORTAMOS A CSV
setwd('D:/BITEL/KPI/ROBOS/SAN_MARTIN')
write.xlsx(ROBSA, file = "ROBSA.xlsx", row.names = FALSE)
write.xlsx(CSAN, file = "CSAN.xlsx", row.names = FALSE)









##################################################
##### ORDENES ROBADAS TACBR
##################################################
### PONER LOS NOMBRE DE LAS BRANCH TACBR
bd4TA=filter(db2, BRANCH=="TACBR",status_id%in%c("6","4","-1"))%>%
filter(fecha>=Sys.Date()-3&fecha<=Sys.Date())
### SELECCIONAMOS LAS COLUMNAS
CTA=bd4TA[,c("N_ORDEN","VENDEDOR","fecha","ID_NO","CONSULT_DESCRIPTION")]
### CREA NUEVA VARIABLE Q SON LOS CANCELADOS DE TACBR PARA CRUZAR DATOS
CTA2=CTA["ID_NO"]
### CREAMOS UNA VARIABLE ROBO
CTA2=CTA2%>%
mutate(ROBO=1)
### CREAMOS UNA BASE CON LA Q SE CRUZARA
db5TA<-db2[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
### SACAMOS TACBR
db5TA=db5TA[!(db5TA$BRANCH=="TACBR"),]
### UBICAMOS SOLO DNI DE LA BASE SIN TACBR
db6TA<-db5TA["ID_NO"]
### CRUZAMOS LOS DATOS |BASE DE TACBR CRUZADA CON LA OTRA BASE PARA VER
### PARA VER ROBO
STOLETA=db6TA %>%
mutate(ROBO= deframe(CTA2)[ID_NO])
### UNIENDO STOLE y DB5
STOLEJTA=STOLETA%>%
full_join(db5TA, by="ID_NO")
### FILTRANDO LAS ORDENES ROBADAS
ROBTA=filter(STOLEJTA,ROBO=="1")
ROBTA<- ROBTA %>% group_by(N_ORDEN) %>% filter (! duplicated(N_ORDEN))
ROBTA=ROBTA[,c("ID_NO","BRANCH","fecha","VENDEDOR","ESTADO","N_ORDEN")]
setwd('D:/BITEL/KPI/ROBOS/TACNA')
write.xlsx(ROBTA, file = "ROBTA.xlsx", row.names = FALSE)
write.xlsx(CTA, file = "CTA.xlsx", row.names = FALSE)









############################################
### ANALISIS DE ROBO
############################################
ROBO_TOTAL=rbind(ROBANC,ROBARE,ROBAYA,ROBCAJ,ROBCUS,ROBHUN,ROBICA,ROBJUN,
ROBL1,ROBL2,ROBL3,ROBL4,ROBLAL,ROBLAM,ROBLO,ROBPI,ROBPUN,
ROBSA,ROBTA)

dbtoday=filter(db2, fecha>=Sys.Date()-3&fecha<=Sys.Date())
print(length(dbtoday$Qty))
print(length(ROBO_TOTAL$ROBO))
Part_total=length(ROBO_TOTAL$ROBO)/length(dbtoday$Qty)
print(Part_total*100)
table(dbtoday$BRANCH)
barplot(prop.table(table(dbtoday$BRANCH)))
table(ROBO_TOTAL$BRANCH)
ROB_USUARIO=as.data.frame(table(ROBO_TOTAL$VENDEDOR))
ROB_USUARIO=ROB_USUARIO%>%
arrange(desc(ROB_USUARIO$Freq))
setwd('D:/BITEL/KPI/ROBOS/SUMMARY')
write.xlsx(ROB_USUARIO, file = "ROB_USUARIO.xlsx", row.names = FALSE)
x <- table(ROBO_TOTAL$BRANCH)
y <- table(ROBO_TOTAL$BRANCH)
data <- data.frame(x)
fig <- plot_ly(data, y = ~Var1, x = ~Freq, type = 'bar',orientation='h',
text = data$Freq, textposition = 'auto',
marker = list(color = 'rgba(50, 171, 96, 0.6)',
line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))
)
fig <- fig %>% layout(title = "Chanel Online Stole Report",
xaxis = list(title = ""),
yaxis = list(title = ""))
fig
