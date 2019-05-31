#Rotina para coletar e apresentar em gráficos algumas séries do banco central
#Feito por: Felipe Simplício Ferreira
#última atualização: 24/05/2019


#Definindo diretórios a serem utilizados

getwd()
setwd("C:/Users/e270780232/Documents")

#Carregando pacotes que serão utilizados
#library(dplyr)
#library(xlsx)
library(ggplot2)
library(RCurl)
library(XML)
library(xlsx)
library(seasonal)

#Definindo data final (padrão usar a data de hoje) das séries que serão coletadas
datafinal = format(Sys.time(), "%d/%m/%Y")

# SALDO % PIB
datainicial = "01/03/2011" #Padrão para todas as séries começarem juntas
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
# Cálculo da variação em 12 meses:
# A função apply irá aplicar a função em cada coluna da base1[,-1] (em cada série do bcb)
variacao=apply(base1[,-1],2,function(x){
  variacao_YoY=rep(NA,12)
  for(i in 13:dim(base1)[1]) variacao_YoY[i]=x[i]-x[i-12]
  return(variacao_YoY)
})

base1=cbind(base1,variacao)
base1=base1[,c(1,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,21,10,23,11,24,12,25,13,26,14,27)]

names(base1)=c("Data","Saldo da carteira de crédito em relação ao PIB - % - 20622","Variação YoY1",
              "Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623","Variação YoY2",
              "Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624","Variação YoY3",
              "Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625","Variação YoY4",
              "Saldo de crédito livre - Pessoas jurídicas / PIB - % - 20626","Variação YoY5", 
              "Saldo de crédito livre - Pessoas físicas / PIB - % - 20627","Variação YoY6",
              "Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628","Variação YoY7",
              "Saldo de crédito direcionado - Pessoas jurídicas / PIB - % - 20629","Variação YoY8",
              "Saldo de crédito direcionado - Pessoas físicas / PIB - % - 20630","Variação YoY9",
              "Saldo das operações de crédito das instituições financeiras sob controle privado em relação a %PIB - 21299", "Variação YoY10",
              "Saldo das operações de crédito das instituições financeiras sob controle público em relação a %PIB - 21300", "Variação YoY11",
              "Saldo das operações de crédito das instituições financeiras sob controle privado nacional em relação a %PIB - 21301", "Variação YoY12",
              "Saldo das operações de crédito das instituições financeiras sob controle estrangeiro em relação a %PIB - 21302", "Variação YoY13")

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

names(base2)=c("Data","20542 - Saldo da carteira de crédito com recursos livres - Total - R$ (milhões)",
               "20543 - Saldo da carteira de crédito com recursos livres - Pessoas jurídicas - Total - R$ (milhões)",
               "20570 - Saldo da carteira de crédito com recursos livres - Pessoas físicas - Total - R$ (milhões)",
               "20593 - Saldo da carteira de crédito com recursos direcionados - Total - R$ (milhões)",
               "20594 - Saldo da carteira de crédito com recursos direcionados - Pessoas jurídicas - Total - R$ (milhões)",
               "20606 - Saldo da carteira de crédito com recursos direcionados - Pessoas físicas - Total - R$ (milhões)",
               "20539 - Saldo da carteira de crédito - Total - R$ (milhões)",
               "20540 - Saldo da carteira de crédito - Pessoas jurídicas - Total - R$ (milhões)",
               "20541 - Saldo da carteira de crédito - Pessoas físicas - Total - R$ (milhões)",
               "Deflator  ----  IPCA - geral - Índice (dez. 1993 = 100)",
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

names(base3)=c("Data","2007 - Saldos das operações de crédito das instituições financeiras sob controle público - Total - u.m.c. (milhões)",
               "2043 - Saldos das operações de crédito das instituições financeiras sob controle privado - Total - u.m.c. (milhões)",
               "IPCA","Público","Privado","Público (% Total de Crédito)","Privado (% Total de Crédito)", "Público YoY", "Privado YoY")

write.csv2(base3,"03-Saldo tipo capital.csv", row.names = F)

# CONCESSÕES
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
names(ipea.table1) = c("Data", "Dias úteis")
ipea.table1 = ipea.table1[rowSums(is.na(ipea.table1)) == 0,]
ipea.table1 = ipea.table1[-dim(ipea.table1)[1],]
ipea.table1 = ipea.table1[-dim(ipea.table1)[1],]
dias_uteis= ipea.table1[which(ipea.table1$Data=="2011.03"):which(ipea.table1$Data==format(as.Date(tail(base4$data,1)),"%Y.%m")),]

base4=cbind(base4,dias_uteis[,-1])

neww=apply(base4[,2:10],2,function(x){neww=x/as.numeric(dias_uteis[,2]);return(neww)})
base4=cbind(base4,neww)
neww1=apply(base4[,13:21],2,function(x){neww1=(x/base4$deflator)*base4$deflator[length(base4$deflator)];return(neww1)})
base4=cbind(base4,neww1)

# Cria coluna com o número de dias do mês para qualquer data dada
# Necessário para implementar a dessazonalização (Ipeadata só possui dias úteis)
# Primeiro verifica se é bissexto (divisível por 4)
# E depois atribui número de dias de cada mês quando o ano é bissexto ou não (objeto dmes)
bissexto.fun=function(x){
  anos=as.numeric(substr(x$data,1,4))
  bissexto=anos/4
  bissexto[which((bissexto-trunc(bissexto,digits=0))==0)]=1
  bissexto[which((bissexto-trunc(bissexto,digits=0))!=0)]=0
  bissexto=as.logical(bissexto)
  
  fev=as.numeric(substr(x$data,6,7))==02 #"==2" para "garimpar" fevereiro
  dmes=rowSums(outer(as.numeric(substr(x$data,6,7)),c(1,3,5,7,8,10,12),"=="))
  dmes[which(dmes==1)]=31
  dmes[which(dmes==0)]=30
  dmes[fev==T]=28
  dmes[bissexto&fev==T]=29
  dmes
  dias_nao_uteis=dmes-as.numeric(dias_uteis[,2])
  indicador=as.numeric(dias_uteis[,2])-(5/2)*dias_nao_uteis
  return(indicador)
}

indicador=bissexto.fun(base4)
indicador=ts(indicador,start = c(2011,3), freq = 12)

dessas=function(x){
  dessas_dados1=seas(x=ts(x,start = c(2011,3), freq = 12),xreg = indicador, arima.model = "(1 0 0)(1 0 1)",
                     forecast.maxlead = 0,transform.function = "log",x11.mode = "mult")
  return(as.numeric(final(dessas_dados1))                                                                                                                              )
}
dessas_dados1=apply(base4[,22:30],2,function(x)dessas(x))

base4=cbind(base4,dessas_dados1)

addd=apply(base4[,22:30],2,function(x){
  addd=rep(NA,12)
  for(i in 13:dim(base4)[1]) addd[i]=((x[i]/x[i-12])-1)*100
  return(addd)
})
base4=cbind(base4,addd)

names(base4)=c("Data","20634 Recursos livres - Concessões de crédito com recursos livres - Total - R$ (milhões)",
               "20635 Recursos livres - Concessões de crédito com recursos livres - Pessoas jurídicas - Total - R$ (milhões)",
               "20662 Recursos livres - Concessões de crédito com recursos livres - Pessoas físicas - Total - R$ (milhões)",
               "20685 Recursos Direcionados - Concessões de crédito com recursos direcionados - Total - R$ (milhões)",
               "20686 Recursos Direcionados - Concessões de crédito com recursos direcionados - Pessoas jurídicas - Total - R$ (milhões)",
               "20698 Recursos Direcionados- Concessões de crédito com recursos direcionados - Pessoas físicas - Total - R$ (milhões)",
               "20631 Total- Concessões de crédito - Total - R$ (milhões)",
               "20632 Total - Concessões de crédito - Pessoas jurídicas - Total - R$ (milhões)",
               "20633 Total - Concessões de crédito - Pessoas físicas - Total - R$ (milhões)",
               "Deflator","Número de dias Úteis","Recursos Livres Diário - Total","Recursos Livres Diário - PJ","Recursos Livres Diário - PF",
               "Recursos Direcionados Diário - Total","Recursos Direcionados Diário - PJ","Recursos Direcionados Diário - PF",
               "Total Diário - Total","Total Diário - PJ","Total Diário - PF",
               "Recursos Livres Deflacionado (reais do último mês)  Diário - Total","Recursos Livres Deflacionado (reais do último mês)  Diário - PJ",
               "Recursos Livres Deflacionado (reais do último mês)  Diário - PF","Recursos Direcionados Deflacionado  (reais do último mês)  Diário - Total",
               "Recursos Direcionados Deflacionado  (reais do último mês)  Diário - PJ","Recursos Direcionados Deflacionado  (reais do último mês)  Diário - PF",
               "Total Deflacionado  (reais do último mês)  Diário - Total","Total Deflacionado  (reais do último mês)  Diário - PJ",
               "Total Deflacionado  (reais do último mês)  Diário - PF","Dessazonalizado - Recursos Livres Deflacionado (reais do último mês) Diário - Total livres",
               "Dessazonalizado - Recursos Livres Deflacionado (reais do último mês) Diário - PJ livres","Dessazonalizado - Recursos Livres Deflacionado (reais do último mês) Diário - PF livres",
               "Dessazonalizado - Recursos Direcionados Deflacionado (reais do último mês) Diário - Total direcionado",
               "Dessazonalizado - Recursos Direcionados Deflacionado (reais do último mês) Diário - PJ direcionado",
               "Dessazonalizado - Recursos Direcionados Deflacionado (reais do último mês) Diário - PF direcionado",
               "Dessazonalizado - Total Deflacionado (reais do último mês) Diário - Total","Dessazonalizado - Total Deflacionado (reais do último mês) Diário - PJ",
               "Dessazonalizado - Total Deflacionado (reais do último mês) Diário - PF","Recursos Livres Deflacionado Diário - Recursos Livres-Total",
               "Recursos Livres Deflacionado Diário - Recursos Livres-PJ","Recursos Livres Deflacionado Diário - Recursos Livres-PF",
               "Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total",
               "Recursos Direcionados Deflacionado Diário - Recursos Direcionados-PJ","Recursos Direcionados Deflacionado Diário - Recursos Direcionados-PF",
               "Total Deflacionado Diário - Total Geral","Total Deflacionado Diário - Pessoa Jurídica","Total Deflacionado Diário - Pessoa Física")

write.csv2(base4,"04-Concessões.csv", row.names = F)

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
names(base5)=c("Data","19882 - Endividamento das famílias com o Sistema Financeiro Nacional em relação à renda acumulada dos útimos doze meses - %",
               "20400 - Endividamento das famílias com o Sistema Financeiro Nacional exceto crédito habitacional em relação à renda acumulada dos últimos doze meses - %",
               "Crédito Habitacional", "Diferença entre não habitacional e habitacional", "Não habitacional", "Habitacional",
               "19882 - Endividamento das famílias com o Sistema Financeiro Nacional em relação à renda acumulada dos últimos doze meses - var p.p",
               "20400 - Endividamento das famílias com o Sistema Financeiro Nacional exceto crédito habitacional em relação à renda acumulada dos últimos doze meses - var p.p",
               "Crédito Habitacional - var dos últimos 12 meses p.p",
               "20622 - Saldo da carteira de crédito em relação ao PIB - %")

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

names(base6)=c("Data","19881 - Comprometimento de renda das famílias com o serviço da dívida com o Sistema Financeiro Nacional - Com ajuste sazonal - %")

write.csv2(base6, "06-Comprometimento Renda.csv", row.names = F)

# INADIMPLÊNCIA
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

names(base7)=c("Data","21086 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total - %",
               "21112 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Total - %",
               "21003 - Percentual da carteira de crédito com atraso entre 15 e 90 dias - Total - %",
               "21004 - Percentual da carteira de crédito com atraso entre 15 e 90 dias - Pessoas jurídicas - Total - %",
               "21005 - Percentual da carteira de crédito com atraso entre 15 e 90 dias - Pessoas físicas - Total - %",
               "21082 - Inadimplência da carteira de crédito - Total","21133 - Inadimplência da carteira de crédito com recursos direcionados - Pessoas jurídicas - Total - %",
               "21145 - Inadimplência da carteira de crédito com recursos direcionados - Pessoas físicas - Total - %",
               "21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %",
               "21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %",
               "21085 - Inadimplência da carteira de crédito com recursos livres - Total - %",
               "21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %")

write.csv2(base7, "07-Inadimplência.csv", row.names = F)

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

# Coleta o valor da série em todos os últimos dias de cada mês (cálculo realizado para ambas as variáveis)
meta=apply(base8.a[,-1],2,function(x) tapply(x[!is.na(x)],ind[!is.na(x)],function(y) y[length(y)]))
meta=meta[1:which(row.names(meta)==format(as.Date(tail(base8.a$data,1)),"%Y%m")),]
#meta=meta[1:which(row.names(meta)==daTa1),] 
#alterar a data para o mês em questÃ£o

base8.b = base8[,-2]
base8.b = base8.b[,-2]
base8.b = na.exclude(base8.b)
base8.b[,-1]=apply(base8.b[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

PJ=base8.b[,2]-base8.b[,5]
PF=base8.b[,3]-base8.b[,6]
Tot=base8.b[,4]-base8.b[,7]

#excluir a última linha da série 'meta' - que tem um mês a mais
{if(dim(base8.b)[1]<dim(meta)[1])
  meta = meta[-dim(meta)[1],]
else
  meta = meta}
base8=cbind(base8.b$data,base8.b[,-1],PJ,PF,Tot, meta)
rm(list=objects(pattern="^serie"))

dessas_dados2=apply(base8[,c(4,10:12,5:7)],2,function(x)dessas(x))

base8=cbind(base8,dessas_dados2)
names(base8)=c("Data","20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
               "20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
               "20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
               "20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.",
               "20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.",
               "20783 - Spread médio das operações de crédito - Total - p.p.",
               "Pessoas Jurídicas - Taxa de Captação","Pessoas Físicas - Taxa de Captação","Total - Taxa de Captação",
               "432 - Meta Selic","7806 - Swap_DI_Pre_360 dias","dessaz Taxa de aplicação (eixo direito)",
               "dessaz Taxa de captação","dessaz Meta Selic","dessaz SWAP DI 360","dessaz spread Pessoa  jurídica",
               "dessaaz spread Pessoa  física","dessaz spread Total")

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

base9[,-1]=apply(base9[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

addd=apply(base9[,2:7],2,function(x){
  addd=rep(NA,12)
  for(i in 13:dim(base9)[1]) addd[i]=x[i]-x[i-12]
  return(addd)
})

base9=cbind(base9,addd)
base9 = base9[which(serie1$data=="2011-03-01"):which(serie1$data==tail(serie1$data,1)),]
rm(list=objects(pattern="^serie"))

dessas_dados3=apply(base9[,c(2:7)],2,function(x)dessas(x))
base9=cbind(base9,dessas_dados3)

names(base9)=c("Data","20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
               "20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
               "20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
               "20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.",
               "20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.",
               "4189 - Taxa de juros - Selic acumulada no mês anualizada base 252 - % a.a.",
               "Total","Pessoas jurídicas","Pessoas físicas","Crédito com recursos livres","Crédito com recursos direcionados",
               "Taxa de juros SELIC",
               "dessaz 20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
               "dessaz 20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
               "dessaz 20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
               "dessaz 20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.",
               "dessaz 20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.",
               "dessaz 4189 - Taxa de juros - Selic acumulada no mês anualizada base 252 - % a.a.")

write.csv2(base9, "09-Juros.csv", row.names = F)

# SALDO TOTAL SÉRIE ENCADEADA

#Série do SGS até 2052 - Saldo das operações de crédito por atividade econômica - Total - u.m.c. (milhões) convertida em milhões de reais
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
{if(dim(base10)[1]<dim(ipca)[1])
  ipca = ipca[-dim(ipca)[1],]
else
  ipca = ipca}
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
names(base10)=c("Data","Saldo crédito total série antiga e nova - R$ milhões",
               "IPCA - geral - Índice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA12",
               "Saldo crédito total série antiga e nova - R$ milhões do último mês","Variação da série deflacionada","Variação da série original")

write.csv2(base10, "10-Saldo total série encadead.csv", row.names = F)

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

names(base11)=c("Data","7938 - Operações de crédito com recursos livres referenciais para taxa de juros - Inadimplência acima de 90 dias em relação ao total da modalidade - Total pessoa física - %   (Série antiga)",
                "21112 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Total - %  (Série nova)","Antiga/nova","Antiga-nova","Variação YoY Nova")
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

names(base12)=c("Data","7937 - Operações de crédito com recursos livres referenciais   para taxa de juros - Inadimplência acima de 90 dias em relação ao total da modalidade - Total pessoa jurídica - %",
                "21086 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total - %")

write.csv2(base12,"12-Dados PJ LIVRE longo.csv", row.names = F)

# RESUMO
`R$ Bilhões`=c(
  base2$`20539 - Saldo da carteira de crédito - Total - R$ (milhões)`[length(base2$`20539 - Saldo da carteira de crédito - Total - R$ (milhões)`)],
  base2$`20541 - Saldo da carteira de crédito - Pessoas físicas - Total - R$ (milhões)`[length(base2$`20541 - Saldo da carteira de crédito - Pessoas físicas - Total - R$ (milhões)`)],
  base2$`20540 - Saldo da carteira de crédito - Pessoas jurídicas - Total - R$ (milhões)`[length(base2$`20540 - Saldo da carteira de crédito - Pessoas jurídicas - Total - R$ (milhões)`)],
  base2$`20542 - Saldo da carteira de crédito com recursos livres - Total - R$ (milhões)`[length(base2$`20542 - Saldo da carteira de crédito com recursos livres - Total - R$ (milhões)`)],
  base2$`20570 - Saldo da carteira de crédito com recursos livres - Pessoas físicas - Total - R$ (milhões)`[length(base2$`20570 - Saldo da carteira de crédito com recursos livres - Pessoas físicas - Total - R$ (milhões)`)],
  base2$`20543 - Saldo da carteira de crédito com recursos livres - Pessoas jurídicas - Total - R$ (milhões)`[length(base2$`20543 - Saldo da carteira de crédito com recursos livres - Pessoas jurídicas - Total - R$ (milhões)`)],
  base2$`20593 - Saldo da carteira de crédito com recursos direcionados - Total - R$ (milhões)`[length(base2$`20593 - Saldo da carteira de crédito com recursos direcionados - Total - R$ (milhões)`)],
  base2$`20606 - Saldo da carteira de crédito com recursos direcionados - Pessoas físicas - Total - R$ (milhões)`[length(base2$`20606 - Saldo da carteira de crédito com recursos direcionados - Pessoas físicas - Total - R$ (milhões)`)],
  base2$`20594 - Saldo da carteira de crédito com recursos direcionados - Pessoas jurídicas - Total - R$ (milhões)`[length(base2$`20594 - Saldo da carteira de crédito com recursos direcionados - Pessoas jurídicas - Total - R$ (milhões)`)]
)

`Variação real* 12 meses (%)`=c(
  base2$`Total Deflacionado-Total Geral`[length(base2$`Total Deflacionado-Total Geral`)],
  base2$`Total Deflacionado-PF`[length(base2$`Total Deflacionado-PF`)],
  base2$`Total Deflacionado-PJ`[length(base2$`Total Deflacionado-PJ`)],
  base2$`Recursos Livres Deflacionado-Total`[length(base2$`Recursos Livres Deflacionado-Total`)],
  base2$`Recursos Livres Deflacionado-PF`[length(base2$`Recursos Livres Deflacionado-PF`)],
  base2$`Recursos Livres Deflacionado-PJ`[length(base2$`Recursos Livres Deflacionado-PJ`)],
  base2$`Recursos Direcionados Deflacionado-Total`[length(base2$`Recursos Direcionados Deflacionado-Total`)],
  base2$`Recursos Direcionados Deflacionado-PF`[length(base2$`Recursos Direcionados Deflacionado-PF`)],
  base2$`Recursos Direcionados Deflacionado-PJ`[length(base2$`Recursos Direcionados Deflacionado-PJ`)]
)

`% do PIB`=c(
  base1$`Saldo da carteira de crédito em relação ao PIB - % - 20622`[length(base1$`Saldo da carteira de crédito em relação ao PIB - % - 20622`)],
  base1$`Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624`[length(base1$`Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624`)],
  base1$`Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623`[length(base1$`Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623`)],
  base1$`Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625`[length(base1$`Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625`)],
  base1$`Saldo de crédito livre - Pessoas físicas / PIB - % - 20627`[length(base1$`Saldo de crédito livre - Pessoas físicas / PIB - % - 20627`)],
  base1$`Saldo de crédito livre - Pessoas jurídicas / PIB - % - 20626`[length(base1$`Saldo de crédito livre - Pessoas jurídicas / PIB - % - 20626`)],
  base1$`Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628`[length(base1$`Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628`)],
  base1$`Saldo de crédito direcionado - Pessoas físicas / PIB - % - 20630`[length(base1$`Saldo de crédito direcionado - Pessoas físicas / PIB - % - 20630`)],
  base1$`Saldo de crédito direcionado - Pessoas jurídicas / PIB - % - 20629`[length(base1$`Saldo de crédito direcionado - Pessoas jurídicas / PIB - % - 20629`)]
)

`Variação em 12 meses em pontos de porcentagem do PIB`=c(
  base1$`Variação YoY1`[length(base1$`Variação YoY1`)],
  base1$`Variação YoY3`[length(base1$`Variação YoY3`)],
  base1$`Variação YoY2`[length(base1$`Variação YoY2`)],
  base1$`Variação YoY4`[length(base1$`Variação YoY4`)],
  base1$`Variação YoY6`[length(base1$`Variação YoY4`)],
  base1$`Variação YoY5`[length(base1$`Variação YoY5`)],
  base1$`Variação YoY7`[length(base1$`Variação YoY7`)],
  base1$`Variação YoY9`[length(base1$`Variação YoY9`)],
  base1$`Variação YoY8`[length(base1$`Variação YoY8`)]
)

tabela1=cbind(`R$ Bilhões`,`Variação real* 12 meses (%)`,`% do PIB`,`Variação em 12 meses em pontos de porcentagem do PIB`)
row.names(tabela1)=c("Total","Pessoa Física","Pessoa Jurídica","Crédito Livre","CL - Pessoa Física","CF - Pessoa Jurídica","Crédito Direcionado", "CD - Pessoa Física", "CD - Pessoa Jurídica")
write.csv2(tabela1,"13-Tabela1.csv")

`R$ bilhões`=c(
  base4$`20631 Total- Concessões de crédito - Total - R$ (milhões)`[length(base4$`20631 Total- Concessões de crédito - Total - R$ (milhões)`)],
  base4$`20633 Total - Concessões de crédito - Pessoas físicas - Total - R$ (milhões)`[length(base4$`20633 Total - Concessões de crédito - Pessoas físicas - Total - R$ (milhões)`)],
  base4$`20632 Total - Concessões de crédito - Pessoas jurídicas - Total - R$ (milhões)`[length(base4$`20632 Total - Concessões de crédito - Pessoas jurídicas - Total - R$ (milhões)`)],
  base4$`20634 Recursos livres - Concessões de crédito com recursos livres - Total - R$ (milhões)`[length(base4$`20634 Recursos livres - Concessões de crédito com recursos livres - Total - R$ (milhões)`)],
  base4$`20685 Recursos Direcionados - Concessões de crédito com recursos direcionados - Total - R$ (milhões)`[length(base4$`20685 Recursos Direcionados - Concessões de crédito com recursos direcionados - Total - R$ (milhões)`)]
)
`Variação real* 12 meses(%)`=c(
  base4$`Total Deflacionado Diário - Total Geral`[length(base4$`Total Deflacionado Diário - Total Geral`)],
  base4$`Total Deflacionado Diário - Pessoa Física`[length(base4$`Total Deflacionado Diário - Pessoa Física`)],
  base4$`Total Deflacionado Diário - Pessoa Jurídica`[length(base4$`Total Deflacionado Diário - Pessoa Jurídica`)],
  base4$`Recursos Livres Deflacionado Diário - Recursos Livres-Total`[length(base4$`Recursos Livres Deflacionado Diário - Recursos Livres-Total`)],
  base4$`Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total`[length(base4$`Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total`)]
)

`Concessões (Média diária)`=cbind(`R$ bilhões`,`Variação real* 12 meses(%)`)
row.names(`Concessões (Média diária)`)=c("Total","Pessoa Física","Pessoa Jurídica","Crédito Livre","Credito Direcionado")
write.csv2(`Concessões (Média diária)`,"14-Concessões (Média diária).csv")

`Tx de juros %a.a`=c(
  base9$`20714 - Taxa média de juros das operações de crédito - Total - % a.a.`[length(base9$`20714 - Taxa média de juros das operações de crédito - Total - % a.a.`)],
  base9$`20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.`[length(base9$`20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.`)],
  base9$`20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.`[length(base9$`20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.`)],
  base9$`20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.`[length(base9$`20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.`)],
  base9$`20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.`[length(base9$`20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.`)]
)

`Tx de juros Variação 12 Meses (p.p.)`=c(
  base9$Total[length(base9$Total)],
  base9$`Pessoas físicas`[length(base9$`Pessoas físicas`)],
  base9$`Pessoas jurídicas`[length(base9$`Pessoas jurídicas`)],
  base9$`Crédito com recursos livres`[length(base9$`Crédito com recursos livres`)],
  base9$`Crédito com recursos direcionados`[length(base9$`Crédito com recursos direcionados`)]
)

`Inadimplência (%)`=c(
  base7$`21082 - Inadimplência da carteira de crédito - Total`[length(base7$`21082 - Inadimplência da carteira de crédito - Total`)],
  base7$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`[length(base7$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`)],
  base7$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`[length(base7$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`)],
  base7$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`[length(base7$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`)],
  base7$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`[length(base7$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`)]
)

`Inadimplência (%) Variação 12 Meses (p.p.)`=c(
  `Inadimplência (%)`[1]- as.numeric(base7$`21082 - Inadimplência da carteira de crédito - Total`[length(base7$`21082 - Inadimplência da carteira de crédito - Total`)-12]),
  `Inadimplência (%)`[2]- as.numeric(base7$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`[length(base7$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`)-12]),
  `Inadimplência (%)`[3]- as.numeric(base7$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`[length(base7$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`)-12]),
  `Inadimplência (%)`[4]- as.numeric(base7$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`[length(base7$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`)-12]),
  `Inadimplência (%)`[5]- as.numeric(base7$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`[length(base7$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`)-12])
)

tabela2 = as.data.frame(tabela1)
tabela2 = tabela2[-5:-6,]
tabela2 = tabela2[-6:-7,]
assign(paste("Crédito - resumo"),cbind(tabela2,`Concessões (Média diária)`,`Tx de juros %a.a`,`Tx de juros Variação 12 Meses (p.p.)`,`Inadimplência (%)`,`Inadimplência (%) Variação 12 Meses (p.p.)`))

write.csv2(`Crédito - resumo`, "15-Crédito resumo.csv")

# RESUMO JUROS E SPREAD
datainicial="01/03/2011"
serie="20786"
Liv=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
serie="20825"
Dir=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
Liv[,2]=as.numeric(gsub(",","\\.",Liv[,2]))
Dir[,2]=as.numeric(gsub(",","\\.",Dir[,2]))

`Spread p.p*`=c(
  base8$`20783 - Spread médio das operações de crédito - Total - p.p.`[length(base8$`20783 - Spread médio das operações de crédito - Total - p.p.`)],
  base8$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`[length(base8$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`)],
  base8$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`[length(base8$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`)],
  Liv$valor[length(Liv$valor)],Dir$valor[length(Dir$valor)]
)

`Spread Variação 12 Meses (P.P.)`=c(
  `Spread p.p*`[1]-base8$`20783 - Spread médio das operações de crédito - Total - p.p.`[length(base8$`20783 - Spread médio das operações de crédito - Total - p.p.`)-12],
  `Spread p.p*`[2]-base8$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`[length(base8$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`)-12],
  `Spread p.p*`[3]-base8$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`[length(base8$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`)-12],
  `Spread p.p*`[4]-Liv$valor[length(Liv$valor)-12],
  `Spread p.p*`[5]-Dir$valor[length(Dir$valor)-12]
)

selic=base8$`432 - Meta Selic`[length(base8$`432 - Meta Selic`)]

`Resumo juros e spread`=cbind(`Tx de juros %a.a`,`Tx de juros Variação 12 Meses (p.p.)`,`Spread p.p*`,`Spread Variação 12 Meses (P.P.)`)
`Resumo juros e spread`=rbind(`Resumo juros e spread`,c(selic,selic-base8$`432 - Meta Selic`[length(base8$`432 - Meta Selic`)-12],NA,NA))
row.names(`Resumo juros e spread`)=c("Total","Pessoa Física","Pessoa Jurídica","Crédito Livre","Crédito Direcionado","Selic")

write.csv2(`Resumo juros e spread`,"16-Resumo juros e spread.csv")

# ICC - Custo do crédito
datainicial="01/06/2013"
serie=c("25351", "25352", "25353", "27645", "27646", "27647", "27672", "27674", "27675", "27685", "27684")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base17 = serie1
  else
    base17 = merge(base17, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

base17[,-1]=apply(base17[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

names(base17)=c("25351 - Indicador de Custo do Crédito - ICC - Total - % a.a.", "25352 - Indicador de Custo do Crédito - ICC - Pessoas jurídicas - Total - % a.a.",
"25353 - Indicador de Custo do Crédito - ICC - Pessoas físicas - Total - % a.a.",	"27645 - Indicador de Custo do Crédito - ICC - Crédito não rotativo - Total - % a.a.",
"27646 - Indicador de Custo do Crédito - ICC - Pessoas jurídicas - Crédito não rotativo - Total - % a.a.",	"27647 - Indicador de Custo do Crédito - ICC - Pessoas físicas - Crédito não rotativo - Total - % a.a.",
"27672 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Cheque especial - % a.a.",	"27674 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Crédito pessoal não consignado vinculado à composição de dívidas - % a.a.",	
"27675 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Crédito pessoal consignado para trabalhadores do setor privado - % a.a.",	"27685 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Cartão de crédito parcelado - % a.a.",
"27684 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Cartão de crédito rotativo - % a.a.")
write.csv2(base17,"17-ICC Custo do crédito.csv", row.names = F)


write.xlsx(base1 ,file ="crédito.xlsx" ,sheetName ="Saldo %PIB" ,append =F )
write.xlsx(base2 ,file ="crédito.xlsx" ,sheetName ="Saldo Tipo Recurso" ,append =T )
write.xlsx(base3 ,file ="crédito.xlsx" ,sheetName ="Saldo tipo capital" ,append =T )
write.xlsx(base4 ,file ="crédito.xlsx" ,sheetName ="Concessões" ,append =T )
write.xlsx(base5 ,file ="crédito.xlsx" ,sheetName ="Endividamento" ,append =T )
write.xlsx(base6 ,file ="crédito.xlsx" ,sheetName ="Comprometimento Renda" ,append =T )
write.xlsx(base7 ,file ="crédito.xlsx" ,sheetName ="Inadimplência" ,append =T )
write.xlsx(base8 ,file ="crédito.xlsx" ,sheetName ="Dados Spread" ,append =T )
write.xlsx(base8 ,file ="crédito.xlsx" ,sheetName ="Juros" ,append =T )
write.xlsx(base9 ,file ="crédito.xlsx" ,sheetName ="Saldo total série encadead" ,append =T )
write.xlsx(base11 ,file ="crédito.xlsx" ,sheetName ="dados Inadimp PF rec livres" ,append =T )
write.xlsx(base12 ,file ="crédito.xlsx" ,sheetName ="Dados PJ LIVRE longo" ,append =T )
write.xlsx(tabela1 ,file ="crédito.xlsx" ,sheetName ="Tabela1" ,append =T )
write.xlsx(`Concessões (Média diária)` ,file ="crédito.xlsx" ,sheetName ="Concessões (Média diária)" ,append =T )
write.xlsx(`Crédito - resumo` ,file ="crédito.xlsx" ,sheetName ="Crédito resumo" ,append =T )
write.xlsx(`Resumo juros e spread` ,file ="crédito.xlsx" ,sheetName ="Resumo juros e spread" ,append =T )
write.xlsx(base17 ,file ="crédito.xlsx" ,sheetName ="ICC - Custo do crédito" ,append =T )


#############################################################################################################

# GRÁF % PIB
ggplot(base1, aes(base1$Data, base1$`Saldo da carteira de crédito em relação ao PIB - % - 20622`)) +
  geom_line() + xlab("Data") + ylab("Em % do PIB") + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo da carteira de crédito em relação ao PIB - %")
ggsave("01-Gráf PIB.png")

# GRÁF LIV E DIR
ggplot(base2) + geom_line(aes(base2$Data, base2$`Recursos Livres Deflacionado-Total`), colour = "black") +
  geom_line(aes(base2$Data, base2$`Recursos Direcionados Deflacionado-Total`), colour = "gold") +
  geom_bar(aes(base2$Data, base2$`Total Deflacionado-Total Geral`), stat = "identity",colour ="blue") + 
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo da carteira de crédito", "Deflacionado pelo IPCA, var% t/t-12")
ggsave("02-Gráf Liv e dir.png")

# GRÁF PF E PJ
ggplot(base2) + geom_line(aes(base2$Data, base2$`Total Deflacionado-PJ`), colour = "black") +
  geom_line(aes(base2$Data, base2$`Total Deflacionado-PF`), colour = "gold") +
  geom_line(aes(base2$Data, base2$`Total Deflacionado-Total Geral`), stat = "identity",colour ="blue") + 
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo das operações de crédito do SFN", "Deflacionado pelo IPCA, var% t/t-12")
ggsave("03-Gráf PF e PJ.png")

# GRÁF TIPO DE CAP
ggplot(base3) + geom_line(aes(base3$Data, base3$Público), colour = "gold") +
  geom_line(aes(base3$Data, base3$Privado), colour = "blue") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo de crédito por tipo de controle de capital", "R$ milhões do último mês")
ggsave("04-Gráf tipo de cap.png")

# GRÁF TIPO DE CAP %
ggplot(base3) + geom_line(aes(base3$Data, base3$`Público (% Total de Crédito)`), colour = "gold") +
  geom_line(aes(base3$Data, base3$`Privado (% Total de Crédito)`), colour = "blue") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo por tipo de capital", "Participação %")
ggsave("05-Gráf tipo de cap perc.png")

# GRÁF TIPO DE CONTROLE
ggplot(base3) + geom_line(aes(base3$Data, base3$`Público YoY`), colour = "gold") +
  geom_line(aes(base3$Data, base3$`Privado YoY`), colour = "blue") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo de crédito por tipo de controle de capital", "Variação real em 12 meses (em %)")
ggsave("06-Gráf tipo de cap controle var real.png")

# GRÁF CONCESS PJ PF
ggplot(base4) + geom_bar(aes(base4$Data, base4$`Total Deflacionado Diário - Total Geral`), stat = "identity", colour = "gold") +
  geom_bar(aes(base4$Data, base4$`Total Deflacionado Diário - Pessoa Jurídica`), stat = "identity", colour = "blue") +
  geom_bar(aes(base4$Data, base4$`Total Deflacionado Diário - Pessoa Física`), stat = "identity", colour = "black") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Variação % sobre o mesmo mês do ano anterior de novas concessões", "Média por dia útil, valores deflacionados diários")
ggsave("07-Gráf concess PJ PF.png")

# GRÁF CONCESS LIV E DIR
ggplot(base4) + geom_bar(aes(base4$Data, base4$`Recursos Livres Deflacionado Diário - Recursos Livres-Total`), stat = "identity", colour = "black") +
  geom_bar(aes(base4$Data, base4$`Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total`), stat = "identity", colour = "blue") +
  geom_bar(aes(base4$Data, base4$`Total Deflacionado Diário - Total Geral`), stat = "identity", colour = "gold") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Novas concessões de crédito do SFN", "Média por dia útil, deflacionada pelo IPCA, var t/t-12")
ggsave("08-Gráf concess liv e dir.png")

# GRÁF ENDIVIDAMENTO
ggplot(base5) + geom_line(aes(base5$Data, base5$`20622 - Saldo da carteira de crédito em relação ao PIB - %`), colour = "gold") +
  geom_line(aes(base5$Data, base5$`20400 - Endividamento das famílias com o Sistema Financeiro Nacional exceto crédito habitacional em relação à renda acumulada dos últimos doze meses - %`), colour = "blue") +
  geom_line(aes(base5$Data, base5$`19882 - Endividamento das famílias com o Sistema Financeiro Nacional em relação à renda acumulada dos útimos doze meses - %`), colour = "black") +
  geom_line(aes(base5$Data, base5$`Crédito Habitacional`), colour = "grey") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Endividamento familiar - %")
ggsave("09-Gráf endividamento.png")

# GRÁF COMPROMET
ggplot(base6, aes(base6$Data, base6$`19881 - Comprometimento de renda das famílias com o serviço da dívida com o Sistema Financeiro Nacional - Com ajuste sazonal - %`)) +
  geom_line() + xlab("Data") + ylab("Em % da renda") + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Comprometimento da renda familiar com o serviço da dívida", "Dessazonalizado - %")
ggsave("10-Gráf compromet.png")

# GRÁF INADIMPLÊNCIA RECURSOS
# GRAF INADIMPLÊNCIA PESSOAS
