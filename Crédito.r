#Rotina para coletar e apresentar em gr�ficos algumas s�ries do banco central
#Feito por: Felipe Simpl�cio Ferreira
#�ltima atualiza��o: 02/01/2019


#Definindo diret�rios a serem utilizados

getwd()
setwd("C:/Users/User/Documents")

#Carregando pacotes que ser�o utilizados
library(RCurl)
library(XML)
library(xlsx)

#Definindo data final (padr�o usar a data de hoje) das s�ries que ser�o coletadas
datafinal = format(Sys.time(), "%d/%m/%Y")

# SALDO % PIB
datainicial = "01/03/2011" #Padr�o para todas as s�ries come�arem juntas
serie=c("20622","20623","20624","20625","20626","20627","20628", "20629", "20630", "21299", "21300", "21301", "21302")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base1 = serie1
  else
    base1 = merge(base1, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base1[,-1]=apply(base1[,-1],2,function(x)as.numeric(gsub(",",".",x)))
# C�lculo da varia��o em 12 meses:
# A fun��o apply ir� aplicar a fun��o em cada coluna da base1[,-1] (em cada s�rie do bcb)
variacao=apply(base1[,-1],2,function(x){
  variacao_YoY=rep(NA,12)
  for(i in 13:dim(base1)[1]) variacao_YoY[i]=x[i]-x[i-12]
  return(variacao_YoY)
})

base1=cbind(base1,variacao)
base1=base1[,c(1,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,21,10,23,11,24,12,25,13,26,14,27)]

names(base1)=c("Data","Saldo da carteira de cr�dito em rela��o ao PIB - % - 20622","Varia��o YoY1",
               "Saldo da carteira de cr�dito a pessoas jur�dicas em rela��o ao PIB - % - 20623","Varia��o YoY2",
               "Saldo da carteira de cr�dito a pessoas f�sicas em rela��o ao PIB - % - 20624","Varia��o YoY3",
               "Saldo da carteira de cr�dito com recursos livres em rela��o ao PIB - % - 20625","Varia��o YoY4",
               "Saldo de cr�dito livre - Pessoas jur�dicas / PIB - % - 20626","Varia��o YoY5", 
               "Saldo de cr�dito livre - Pessoas f�sicas / PIB - % - 20627","Varia��o YoY6",
               "Saldo da carteira de cr�dito com recursos direcionados em rela��o ao PIB - % - 20628","Varia��o YoY7",
               "Saldo de cr�dito direcionado - Pessoas jur�dicas / PIB - % - 20629","Varia��o YoY8",
               "Saldo de cr�dito direcionado - Pessoas f�sicas / PIB - % - 20630","Varia��o YoY9",
               "Saldo das opera��es de cr�dito das institui��es financeiras sob controle privado em rela��o a %PIB - 21299", "Varia��o YoY10",
               "Saldo das opera��es de cr�dito das institui��es financeiras sob controle p�blico em rela��o a %PIB - 21300", "Varia��o YoY11",
               "Saldo das opera��es de cr�dito das institui��es financeiras sob controle privado nacional em rela��o a %PIB - 21301", "Varia��o YoY12",
               "Saldo das opera��es de cr�dito das institui��es financeiras sob controle estrangeiro em rela��o a %PIB - 21302", "Varia��o YoY13")

write.csv2(base1,"01-Saldo %PIB.csv", row.names = F)

# SALDO TIPO RECURSO
datainicial = "01/03/2007"
serie = c("20542","20543","20570","20593","20594","20606","20539","20540","20541")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base2 = serie1
  else
    base2 = merge(base2, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base2[,-1]=apply(base2[,-1],2,function(x)as.numeric(gsub(",",".",x)))

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=36482"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
ipea.table = ipea.table[-1:-4,-3]
names(ipea.table) = c("Data", "IPCA")
ipea.table = ipea.table[rowSums(is.na(ipea.table)) == 0,]
ipea.table = ipea.table[-dim(ipea.table)[1],]
ipea.table = ipea.table[-dim(ipea.table)[1],]
deflator = ipea.table[which(ipea.table$Data=="2007.03"):which(ipea.table$Data==format(as.Date(tail(base2$data,1)),"%Y.%m")),]
deflator = as.numeric(gsub(",","\\.",gsub("\\.","",deflator[,2])))

base2=cbind(base2,deflator)
base2=cbind(base2,apply(base2[,2:10],2,function(x) (x/deflator)*tail(deflator,1)))

names(base2)=c("Data","20542 - Saldo da carteira de cr�dito com recursos livres - Total - R$ (milh�es)",
               "20543 - Saldo da carteira de cr�dito com recursos livres - Pessoas jur�dicas - Total - R$ (milh�es)",
               "20570 - Saldo da carteira de cr�dito com recursos livres - Pessoas f�sicas - Total - R$ (milh�es)",
               "20593 - Saldo da carteira de cr�dito com recursos direcionados - Total - R$ (milh�es)",
               "20594 - Saldo da carteira de cr�dito com recursos direcionados - Pessoas jur�dicas - Total - R$ (milh�es)",
               "20606 - Saldo da carteira de cr�dito com recursos direcionados - Pessoas f�sicas - Total - R$ (milh�es)",
               "20539 - Saldo da carteira de cr�dito - Total - R$ (milh�es)",
               "20540 - Saldo da carteira de cr�dito - Pessoas jur�dicas - Total - R$ (milh�es)",
               "20541 - Saldo da carteira de cr�dito - Pessoas f�sicas - Total - R$ (milh�es)",
               "Deflator  ----  IPCA - geral - �ndice (dez. 1993 = 100)",
               "Recursos Livres Deflacionado (Total)",
               "Recursos Livres Deflacionado (PJ)",
               "Recursos Livres Deflacionado (PF)",
               "Recursos Direcionados Deflacionado (Total)",
               "Recursos Direcionados Deflacionado (PJ)",
               "Recursos Direcionados Deflacionado (PF)",
               "Total Deflacionado (Total)",
               "Total Deflacionado (PJ)",
               "Total Deflacionado (PF)")

recursos=apply(base2[,12:20],2,function(x){
  recursos1=rep(NA,12)
  for(i in 13:dim(base2)[1]) recursos1[i]=((x[i]/x[i-12])-1)*100
  return(recursos1)
})
colnames(recursos)=c("Recursos Livres Deflacionado-Total","Recursos Livres Deflacionado-PJ","Recursos Livres Deflacionado-PF",
                     "Recursos Direcionados Deflacionado-Total","Recursos Direcionados Deflacionado-PJ","Recursos Direcionados Deflacionado-PF",
                     "Total Deflacionado-Total Geral","Total Deflacionado-PJ","Total Deflacionado-PF")
base2=cbind(base2,recursos)

write.csv2(base2,"02-Saldo Tipo Recurso.csv", row.names = F)

# SALDO TIPO CAPITAL
datainicial = "01/10/1994"
serie = c("2007","2043")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base3 = serie1
  else
    base3 = merge(base3, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base3[,-1]=apply(base3[,-1],2,function(x)as.numeric(gsub(",",".",x)))

deflator = ipea.table[which(ipea.table$Data=="1994.10"):which(ipea.table$Data==format(as.Date(tail(base3$data,1)),"%Y.%m")),]
deflator = as.numeric(gsub(",","\\.",gsub("\\.","",deflator[,2])))
base3 = cbind(base3,deflator)
base3[,5:6] = apply(base3[,2:3],2,function(x) (x/deflator)*deflator[length(deflator)])
base3[,7:8] = apply(base3[,5:6],2,function(x) (x/rowSums(base3[,5:6]))*100)
base3[,9:10] = apply(base3[,5:6],2,function(x){
  variacao_YoY=rep(NA,12)
  for(i in 13:dim(base3)[1]) variacao_YoY[i]=((x[i]/x[i-12])-1)*100
  return(variacao_YoY)
})

names(base3)=c("Data","2007 - Saldos das opera��es de cr�dito das institui��es financeiras sob controle p�blico - Total - u.m.c. (milh�es)",
               "2043 - Saldos das opera��es de cr�dito das institui��es financeiras sob controle privado - Total - u.m.c. (milh�es)",
               "IPCA","P�blico","Privado","P�blico (% Total de Cr�dito)","Privado (% Total de Cr�dito)", "P�blico YoY", "Privado YoY")

write.csv2(base3,"03-Saldo tipo capital.csv", row.names = F)

# CONCESS�ES
datainicial="01/03/2011"
serie=c("20634","20635","20662","20685","20686","20698","20631","20632","20633")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base4 = serie1
  else
    base4 = merge(base4, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base4[,-1]=apply(base4[,-1],2,function(x)as.numeric(gsub("\\.","",x)))

deflator = ipea.table[which(ipea.table$Data=="2011.03"):which(ipea.table$Data==format(as.Date(tail(base4$data,1)),"%Y.%m")),]
deflator = as.numeric(gsub(",","\\.",gsub("\\.","",deflator[,2])))

base4=cbind(base4,deflator)

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044792"
ipea.table1 = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
ipea.table1 = ipea.table1[-1:-4,-3]
names(ipea.table1) = c("Data", "Dias �teis")
ipea.table1 = ipea.table1[rowSums(is.na(ipea.table1)) == 0,]
ipea.table1 = ipea.table1[-dim(ipea.table1)[1],]
ipea.table1 = ipea.table1[-dim(ipea.table1)[1],]
dias_uteis= ipea.table1[which(ipea.table1$Data=="2011.03"):which(ipea.table1$Data==format(as.Date(tail(base4$data,1)),"%Y.%m")),]

base4=cbind(base4,dias_uteis[,-1])

neww=apply(base4[,2:10],2,function(x){neww=x/as.numeric(dias_uteis[,2]);return(neww)})
base4=cbind(base4,neww)
neww1=apply(base4[,13:21],2,function(x){neww1=(x/base4$deflator)*base4$deflator[length(base4$deflator)];return(neww1)})
base4=cbind(base4,neww1)

addd=apply(base4[,22:30],2,function(x){
  addd=rep(NA,12)
  for(i in 13:dim(base4)[1]) addd[i]=((x[i]/x[i-12])-1)*100
  return(addd)
})
base4=cbind(base4,addd)

names(base4)=c("Data","20634 Recursos livres - Concess�es de cr�dito com recursos livres - Total - R$ (milh�es)",
               "20635 Recursos livres - Concess�es de cr�dito com recursos livres - Pessoas jur�dicas - Total - R$ (milh�es)",
               "20662 Recursos livres - Concess�es de cr�dito com recursos livres - Pessoas f�sicas - Total - R$ (milh�es)",
               "20685 Recursos Direcionados - Concess�es de cr�dito com recursos direcionados - Total - R$ (milh�es)",
               "20686 Recursos Direcionados - Concess�es de cr�dito com recursos direcionados - Pessoas jur�dicas - Total - R$ (milh�es)",
               "20698 Recursos Direcionados- Concess�es de cr�dito com recursos direcionados - Pessoas f�sicas - Total - R$ (milh�es)",
               "20631 Total- Concess�es de cr�dito - Total - R$ (milh�es)",
               "20632 Total - Concess�es de cr�dito - Pessoas jur�dicas - Total - R$ (milh�es)",
               "20633 Total - Concess�es de cr�dito - Pessoas f�sicas - Total - R$ (milh�es)",
               "Deflator","N�mero de dias �teis","Recursos Livres Di�rio - Total","Recursos Livres Di�rio - PJ","Recursos Livres Di�rio - PF",
               "Recursos Direcionados Di�rio - Total","Recursos Direcionados Di�rio - PJ","Recursos Direcionados Di�rio - PF",
               "Total Di�rio - Total","Total Di�rio - PJ","Total Di�rio - PF",
               "Recursos Livres Deflacionado (reais do �ltimo m�s)  Di�rio - Total","Recursos Livres Deflacionado (reais do �ltimo m�s)  Di�rio - PJ",
               "Recursos Livres Deflacionado (reais do �ltimo m�s)  Di�rio - PF","Recursos Direcionados Deflacionado  (reais do �ltimo m�s)  Di�rio - Total",
               "Recursos Direcionados Deflacionado  (reais do �ltimo m�s)  Di�rio - PJ","Recursos Direcionados Deflacionado  (reais do �ltimo m�s)  Di�rio - PF",
               "Total Deflacionado  (reais do �ltimo m�s)  Di�rio - Total","Total Deflacionado  (reais do �ltimo m�s)  Di�rio - PJ",
               "Total Deflacionado  (reais do �ltimo m�s)  Di�rio - PF", "Recursos Livres Deflacionado Di�rio - Recursos Livres-Total",
               "Recursos Livres Deflacionado Di�rio - Recursos Livres-PJ","Recursos Livres Deflacionado Di�rio - Recursos Livres-PF",
               "Recursos Direcionados Deflacionado Di�rio - Recursos Direcionados-Total",
               "Recursos Direcionados Deflacionado Di�rio - Recursos Direcionados-PJ","Recursos Direcionados Deflacionado Di�rio - Recursos Direcionados-PF",
               "Total Deflacionado Di�rio - Total Geral","Total Deflacionado Di�rio - Pessoa Jur�dica","Total Deflacionado Di�rio - Pessoa F�sica")

write.csv2(base4,"04-Concess�es.csv", row.names = F)

# ENDIVIDAMENTO
datainicial="01/01/2005"
serie=c("19882","20400","20622")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base5 = serie1
  else
    base5 = merge(base5, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base5[,-1]=apply(base5[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
base5[,5]=base5[,2]-base5[,3]
base5[,6] = base5[,3]-base5[,5]
base5[,7] = base5[,3]/base5[,2]
base5[,8] = base5[,5]/base5[,2]
base5=base5[,c(1,2,3,5,6,7,8,4)]

add1=apply(base5[,2:4],2,function(x){
  add1=rep(NA,12)
  for(i in 13:dim(base5)[1]) add1[i]=x[i]-x[i-12]
  return(add1)
})

base5=cbind(base5[,1:7],add1,X20622=base5[,8])
names(base5)=c("Data","19882 - Endividamento das fam�lias com o Sistema Financeiro Nacional em rela��o � renda acumulada dos �timos doze meses - %",
               "20400 - Endividamento das fam�lias com o Sistema Financeiro Nacional exceto cr�dito habitacional em rela��o � renda acumulada dos �ltimos doze meses - %",
               "Cr�dito Habitacional", "Diferen�a entre n�o habitacional e habitacional", "N�o habitacional", "Habitacional",
               "19882 - Endividamento das fam�lias com o Sistema Financeiro Nacional em rela��o � renda acumulada dos �ltimos doze meses - var p.p",
               "20400 - Endividamento das fam�lias com o Sistema Financeiro Nacional exceto cr�dito habitacional em rela��o � renda acumulada dos �ltimos doze meses - var p.p",
               "Cr�dito Habitacional - var dos �ltimos 12 meses p.p",
               "20622 - Saldo da carteira de cr�dito em rela��o ao PIB - %")

write.csv2(base5,"05-Endividamento.csv", row.names = F)

# COMPROMETIMENTO RENDA
datainicial="01/03/2005"
serie=c("19881")


for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base6 = serie1
  else
    base6 = merge(base6, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base6[,2]=as.numeric(gsub(",",".",as.character(base6[,2])))

names(base6)=c("Data","19881 - Comprometimento de renda das fam�lias com o servi�o da d�vida com o Sistema Financeiro Nacional - Com ajuste sazonal - %")

write.csv2(base6, "06-Comprometimento Renda.csv", row.names = F)

# INADIMPL�NCIA
datainicial="01/03/2011"
serie=c("21086","21112","21003","21004","21005","21082","21133","21145","21083","21084","21085","21132")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base7 = serie1
  else
    base7 = merge(base7, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base7[,2]=as.numeric(gsub(",",".",as.character(base7[,2])))

names(base7)=c("Data","21086 - Inadimpl�ncia da carteira de cr�dito com recursos livres - Pessoas jur�dicas - Total - %",
               "21112 - Inadimpl�ncia da carteira de cr�dito com recursos livres - Pessoas f�sicas - Total - %",
               "21003 - Percentual da carteira de cr�dito com atraso entre 15 e 90 dias - Total - %",
               "21004 - Percentual da carteira de cr�dito com atraso entre 15 e 90 dias - Pessoas jur�dicas - Total - %",
               "21005 - Percentual da carteira de cr�dito com atraso entre 15 e 90 dias - Pessoas f�sicas - Total - %",
               "21082 - Inadimpl�ncia da carteira de cr�dito - Total","21133 - Inadimpl�ncia da carteira de cr�dito com recursos direcionados - Pessoas jur�dicas - Total - %",
               "21145 - Inadimpl�ncia da carteira de cr�dito com recursos direcionados - Pessoas f�sicas - Total - %",
               "21083 - Inadimpl�ncia da carteira de cr�dito - Pessoas jur�dicas - Total - %",
               "21084 - Inadimpl�ncia da carteira de cr�dito - Pessoas f�sicas - Total - %",
               "21085 - Inadimpl�ncia da carteira de cr�dito com recursos livres - Total - %",
               "21132 - Inadimpl�ncia da carteira de cr�dito com recursos direcionados - Total - %")

write.csv2(base7, "07-Inadimpl�ncia.csv", row.names = F)

# DADOS SPREAD
datainicial="01/03/2011"
serie=c("432","7806", "20715","20716","20714","20784","20785","20783")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base8 = serie1
  else
    base8 = merge(base8, dados, by = "data", all = T)
}

rm(dados)
base8.a = merge(serie1, serie2, by="data",all = F)


base8.a[,-1]=apply(base8.a[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
ind=paste0(substr(base8.a$data,1,4),substr(base8.a$data,6,7))#paste0(ano,mes)

# Coleta o valor da s�rie em todos os �ltimos dias de cada m�s (c�lculo realizado para ambas as vari�veis)
meta=apply(base8.a[,-1],2,function(x) tapply(x[!is.na(x)],ind[!is.na(x)],function(y) y[length(y)]))
meta=meta[1:which(row.names(meta)==format(as.Date(tail(base8.a$data,1)),"%Y%m")),]
#meta=meta[1:which(row.names(meta)==daTa1),] 
#alterar a data para o m�s em questão

base8.b = base8[,-2]
base8.b = base8.b[,-2]
base8.b = na.exclude(base8.b)
base8.b[,-1]=apply(base8.b[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

PJ=base8.b[,2]-base8.b[,5]
PF=base8.b[,3]-base8.b[,6]
Tot=base8.b[,4]-base8.b[,7]

#excluir a �ltima linha da s�rie 'meta' - que tem um m�s a mais
meta = meta[-dim(meta),]
base8=cbind(base8.b$data,base8.b[,-1],PJ,PF,Tot, meta)
rm(list=objects(pattern="^serie"))

names(base8)=c("Data","20715 - Taxa m�dia de juros das opera��es de cr�dito - Pessoas jur�dicas - Total - % a.a.",
               "20716 - Taxa m�dia de juros das opera��es de cr�dito - Pessoas f�sicas - Total - % a.a.",
               "20714 - Taxa m�dia de juros das opera��es de cr�dito - Total - % a.a.",
               "20784 - Spread m�dio das opera��es de cr�dito - Pessoas jur�dicas - Total - p.p.",
               "20785 - Spread m�dio das opera��es de cr�dito - Pessoas f�sicas - Total - p.p.",
               "20783 - Spread m�dio das opera��es de cr�dito - Total - p.p.",
               "Pessoas Jur�dicas - Taxa de Capta��o","Pessoas F�sicas - Taxa de Capta��o","Total - Taxa de Capta��o",
               "432 - Meta Selic","7806 - Swap_DI_Pre_360 dias")

write.csv2(base8, "08-Dados Spread.csv", row.names = F)

# JUROS
datainicial="01/03/2011"
serie=c("20714","20715","20716","20717","20756","4189")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base9 = serie1
  else
    base9 = merge(base9, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base9[,-1]=apply(base9[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

addd=apply(base9[,2:7],2,function(x){
  addd=rep(NA,12)
  for(i in 13:dim(base9)[1]) addd[i]=x[i]-x[i-12]
  return(addd)
})

base9=cbind(base9,addd)
names(base9)=c("Data","20714 - Taxa m�dia de juros das opera��es de cr�dito - Total - % a.a.",
               "20715 - Taxa m�dia de juros das opera��es de cr�dito - Pessoas jur�dicas - Total - % a.a.",
               "20716 - Taxa m�dia de juros das opera��es de cr�dito - Pessoas f�sicas - Total - % a.a.",
               "20717 - Taxa m�dia de juros das opera��es de cr�dito com recursos livres - Total - % a.a.",
               "20756 - Taxa m�dia de juros das opera��es de cr�dito com recursos direcionados - Total - % a.a.",
               "4189 - Taxa de juros - Selic acumulada no m�s anualizada base 252 - % a.a.",
               "Total","Pessoas jur�dicas","Pessoas f�sicas","Cr�dito com recursos livres","Cr�dito com recursos direcionados")

write.csv2(base9, "09-Juros.csv", row.names = F)

# SALDO TOTAL S�RIE ENCADEADA

#S�rie do SGS at� 2052 - Saldo das opera��es de cr�dito por atividade econ�mica - Total - u.m.c. (milh�es) convertida em milh�es de reais
sant=c(0.00763737163636364,0.0092705920,0.01148718872727270,0.01415395490909090,0.01781688472727270,0.02239473490909090,0.029041720,
       0.0351240,0.03897418181818180,0.04247018181818180,0.04817818181818180,0.05398763636363640,0.06442945454545450,0.08312618181818180,0.1083814545454550,
       0.1527080,0.2143069090909090,0.2956927272727270,0.4701992727272730,0.7378978181818180,1.2415494545454500,1.8087800,2.190777818181820,
       2.3339545454545500,2.5794778181818200,2.924971636363640,3.2885156363636400,3.7804025454545500,4.3673400,5.1762203636363600,6.091557090909090,
       7.2630145454545500,7.977308363636360,8.699271636363640,9.629185818181820,10.56405163636360,11.97966618181820,13.426841090909100,15.335374909090900,
       18.26000727272730,22.18029854545450,28.23702872727270,35.45712654545450,45.3669916363636,56.276580363636400,68.936931636363600,84.37988400,
       102.59764363636400,131.3664970909090,162.44511200,201.90960472727300,254.55575054545500,322.01104654545500,409.02659600,523.3797443636360,
       659.39918290909100,843.01461890909100,1067.0451163636400,1385.65107200,1837.915629818180,2410.8236258181800,3215.15987381818,4348.0683636363600,
       5907.304000,8288.3356363636400,11426.5381818182000,15789.4600,22015.29163636360,31618.662909090900,45129.325818181800,63896.388000,
       94894.860363636400,140753.36981818200,
       151409,161527,170174,177730,183926,186855,193124,200568,207986,215148,221881,226151,223167,224536,229351,233029,237908,238642,237354,240543,244552,
       245390,246296,247998,248124,247311,248883,252646,251759,252508,254730,257381,259971,263140,267569,272685,276712,281115,285784,290083,292602,259688,
       259246,264562,264250,266187,268013,267097,266783,268188,271290,271374,278752,276886,285576,288522,281130,278606,280166,280156,282091,284366,287791,
       292802,295822,288505,289196,287572,286087,291605,312234,307005,309902,310257,308668,312892,320782,326829,326692,335281,343911,349560,355933,319495,
       324557,332139,340654,343564,345003,336373,337739,340040,341231,344649,349677,357542,364473,359768,381782,378611,381850,384393,384507,388323,386965,
       383305,386301,388580,389879,392427,397521,403150,413116,418254,417773,420850,423347,435553,445866,452933,457299,464462,473707,487243,493286,498723,
       505693,511635,520283,529998,533929,541315,548817,557832,564351,575970,590119,607032,608728,615672,625790,637440,654128,658510,669514,674302,684391,
       697581,717046,732590,738455,748518)

paste_d=function(x,y)paste(x,y,sep="/")
da=as.vector(outer(substr(paste0(0,1:12),nchar(paste0(0,1:12))-1,nchar(paste0(0,1:12))),1988:2006,paste_d))
da=c(da,"01/2007","02/2007")[-c(1:5)]
da=data.frame(da,sant)
names(da)=c("Data","X20539")

datainicial="01/03/2007"
serie=c("20539")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base10 = serie1
  else
    base10 = merge(base10, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

names(base10)=c("Data","X20539")
base10$Data = format(as.Date(base10$Data),"%m/%Y")
base10=rbind(da,base10)

ipca = ipea.table
ipca=ipca[which(ipca$Data=="1988.06"):dim(ipca)[1],]
ipca = ipca[-dim(ipca)[1],]
ipca = as.numeric(gsub(",","\\.",gsub("\\.","",ipca[,2])))


base10=cbind(base10,ipca)
saldo_cred=(base10$X20539/base10$ipca)*base10$ipca[which(base10$Data==tail(base10$Data,1))]
base10=cbind(base10,saldo_cred)

add1=apply(base10[,c(4,2)],2,function(x){
  add1=rep(NA,12)
  for(i in 13:dim(base10)[1]) add1[i]=((x[i]/x[i-12])-1)*100
  return(add1)
})

base10=cbind(base10,add1)
names(base10)=c("Data","Saldo cr�dito total s�rie antiga e nova - R$ milh�es",
                "IPCA - geral - �ndice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estat�stica, Sistema Nacional de �ndices de Pre�os ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA12",
                "Saldo cr�dito total s�rie antiga e nova - R$ milh�es do �ltimo m�s","Varia��o da s�rie deflacionada","Varia��o da s�rie original")

write.csv2(base10, "10-Saldo total s�rie encadead.csv", row.names = F)

# DADOS INADIMPL PF REC LIVRES
datainicial="01/06/2000"
dataf="31/12/2012"
serie="7938"
v1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&datainicialnicial=",datainicial,"&datafinalinal=",dataf,sep="")),sep=";")
v1[,2]=as.numeric(gsub(",","\\.",v1[,2]))

datainicial="01/03/2011"
serie="21112"
v2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&datainicialnicial=",datainicial,"&datafinalinal=",datafinal,sep="")),sep=";")
v2[,2]=as.numeric(gsub(",","\\.",v2[,2]))

v3=c(rep(NA,which(v1$data=="01/02/2011")),v2[,2])
base11=data.frame(unique(c(as.character(v1[,1]),as.character(v2[,1]))),c(v1[,2],rep(NA,length(v3)-length(v1[,2]))),v3)
base11[,4]=base11[,2]/base11[,3]
base11[,5]=base11[,2]-base11[,3]
add1=rep(NA,12)
for(i in 13:dim(base11)[1]) add1[i]=base11[i,3]-base11[i-12,3]
base11[,6]=add1

names(base11)=c("Data","7938 - Opera��es de cr�dito com recursos livres referenciais para taxa de juros - Inadimpl�ncia acima de 90 dias em rela��o ao total da modalidade - Total pessoa f�sica - %   (S�rie antiga)",
                "21112 - Inadimpl�ncia da carteira de cr�dito com recursos livres - Pessoas f�sicas - Total - %  (S�rie nova)","Antiga/nova","Antiga-nova","Varia��o YoY Nova")
base11=cbind(Data=substr(base11[,1],4,10),base11[,-1])

write.csv2(base11,"11-Dados Inadimp PF rec livres.csv", row.names = F)

# DADOS PJ LIVRE LONGO
datainicial="01/06/2000"
serie=c("7937","21086")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base12 = serie1
  else
    base12 = merge(base12, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base12[,-1]=apply(base12[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

names(base12)=c("Data","7937 - Opera��es de cr�dito com recursos livres referenciais   para taxa de juros - Inadimpl�ncia acima de 90 dias em rela��o ao total da modalidade - Total pessoa jur�dica - %",
                "21086 - Inadimpl�ncia da carteira de cr�dito com recursos livres - Pessoas jur�dicas - Total - %")

write.csv2(base12,"12-Dados PJ LIVRE longo.csv", row.names = F)

write.xlsx(base1 ,file ="cr�dito.xlsx" ,sheetName ="Saldo %PIB" ,append =F )
write.xlsx(base2 ,file ="cr�dito.xlsx" ,sheetName ="Saldo Tipo Recurso" ,append =T )
write.xlsx(base3 ,file ="cr�dito.xlsx" ,sheetName ="Saldo tipo capital" ,append =T )
write.xlsx(base4 ,file ="cr�dito.xlsx" ,sheetName ="Concess�es" ,append =T )
write.xlsx(base5 ,file ="cr�dito.xlsx" ,sheetName ="Endividamento" ,append =T )
write.xlsx(base6 ,file ="cr�dito.xlsx" ,sheetName ="Comprometimento Renda" ,append =T )
write.xlsx(base7 ,file ="cr�dito.xlsx" ,sheetName ="Inadimpl�ncia" ,append =T )
write.xlsx(base8 ,file ="cr�dito.xlsx" ,sheetName ="Dados Spread" ,append =T )
write.xlsx(base8 ,file ="cr�dito.xlsx" ,sheetName ="Juros" ,append =T )
write.xlsx(base9 ,file ="cr�dito.xlsx" ,sheetName ="Saldo total s�rie encadead" ,append =T )
write.xlsx(base11 ,file ="cr�dito.xlsx" ,sheetName ="dados Inadimp PF rec livres" ,append =T )
write.xlsx(base12 ,file ="cr�dito.xlsx" ,sheetName ="Dados PJ LIVRE longo" ,append =T )