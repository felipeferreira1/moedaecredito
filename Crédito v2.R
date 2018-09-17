#################################################################################################################
# GECON
# ÁREA: CRÉDITO E JUROS
# PLANILHA CRÉDITO
# PEDRO HENRIQUE T. O. SOUSA e FELIPE SIMPLÍCIO FERREIRA
# DATA: 19-06-2018
#################################################################################################################

# PACOTES REQUERIDOS:
# INSTALAR QUANDO NECESSÁRIO
# EXEMPLO: install.packages("pryr")
require(rbcb)
require(RCurl)
require(XML)
require(ecoseries)
require(xlsx)

# DEFINIR PASTAS DE RESULTADOS:
getwd()
setwd("//SRJN3/area_corporativa/Projeto GAP-DIMAC/Automatizações/Dados de crédito - Mensal")

#ATUALIZAR DATAS (alinhar com mes do ultimo dado divulgado)
daTa1="201804"
daTa2="2018.04" 
daTa3="04/2018"

# 1)SALDO % PIB
serie=c(20622,20623,20624,20625,20626,20627,20628, 20629, 20630)

vec_ind1 = get_series(serie[1], start_date = "2007-03-01")
vec_ind2 = get_series(serie[2], start_date = "2007-03-01")
vec_ind3 = get_series(serie[3], start_date = "2007-03-01")
vec_ind4 = get_series(serie[4], start_date = "2007-03-01")
vec_ind5 = get_series(serie[5], start_date = "2007-03-01")
vec_ind6 = get_series(serie[6], start_date = "2007-03-01")
vec_ind7 = get_series(serie[7], start_date = "2007-03-01")
vec_ind8 = get_series(serie[8], start_date = "2007-03-01")
vec_ind9 = get_series(serie[9], start_date = "2007-03-01")


base=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2],vec_ind7[,2],vec_ind8[,2],vec_ind9[,2])
rm(list=objects(pattern="vec_ind[0-9]"))

str(base)
names(base)=c("Data","20622","20623","20624","20625","20626","20627","20628", "20629", "20630")
base[,-1]=apply(base[,-1],2,function(x)as.numeric(gsub(",",".",x)))
str(base)

# Cálculo da variação em 12 meses:
# A função apply irá aplicar a função em cada coluna da base[,-1] (em cada série do bcb)
variacao=apply(base[,-1],2,function(x){
  variacao_YoY=rep(NA,12)
  for(i in 13:dim(base)[1]) variacao_YoY[i]=x[i]-x[i-12]
  return(variacao_YoY)
})


base=cbind(base,variacao)
base=base[,c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)]
names(base)=c("Data","Saldo da carteira de crédito em relação ao PIB - % - 20622","Variação YoY1",
              "Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623","Variação YoY2",
              "Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624","Variação YoY3",
              "Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625","Variação YoY4",
              "Saldo de crédito livre - Pessoas jurídicas / PIB - % - 20626","Variação YoY5", 
              "Saldo de crédito livre - Pessoas físicas / PIB - % - 20627","Variação YoY6",
              "Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628","Variação YoY7",
              "Saldo de crédito direcionado - Pessoas jurídicas / PIB - % - 20629","Variação YoY8",
              "Saldo de crédito direcionado - Pessoas físicas / PIB - % - 20630","Variação YoY9")
write.xlsx(base,"01-Saldo %PIB.xlsx", row.names = F)

# 2)SALDO TIPO RECURSO
serie=c(20542,20543,20570,20593,20594,20606,20539,20540,20541)


vec_ind1 = get_series(serie[1])
vec_ind2 = get_series(serie[2])
vec_ind3 = get_series(serie[3])
vec_ind4 = get_series(serie[4])
vec_ind5 = get_series(serie[5])
vec_ind6 = get_series(serie[6])
vec_ind7 = get_series(serie[7], start_date = "2007-03-01")
vec_ind8 = get_series(serie[8])
vec_ind9 = get_series(serie[9])

base1=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2],vec_ind7[,2],vec_ind8[,2],vec_ind9[,2])
rm(list=objects(pattern="vec_ind[0-9]"))
str(base1)

base1[,-1]=apply(base1[,-1],2,function(x)as.numeric(gsub("\\.","",x)))
str(base1)
url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=36482"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
deflator= ipea.table[which(ipea.table$V1=="2007.03"):which(ipea.table$V1==daTa2),]
deflator=as.numeric(gsub(",","\\.",gsub("\\.","",deflator[,2])))

base1=cbind(base1,deflator)
base1=cbind(base1,apply(base1[,2:10],2,function(x) x/deflator))

names(base1)=c("Data","20542 - Saldo da carteira de crédito com recursos livres - Total - R$ (milhões)",
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

recursos=apply(base1[,12:20],2,function(x){
  recursos1=rep(NA,12)
  for(i in 13:dim(base1)[1]) recursos1[i]=((x[i]/x[i-12])-1)*100
  return(recursos1)
})
colnames(recursos)=c("Recursos Livres Deflacionado-Total","Recursos Livres Deflacionado-PJ","Recursos Livres Deflacionado-PF",
                     "Recursos Direcionados Deflacionado-Total","Recursos Direcionados Deflacionado-PJ","Recursos Direcionados Deflacionado-PF",
                     "Total Deflacionado-Total Geral","Total Deflacionado-PJ","Total Deflacionado-PF")
base1=cbind(Data=substr(base1[,1],4,10),base1[,-1],recursos)

write.xlsx(base1,"02-Saldo Tipo Recurso.xlsx", row.names = F)

# 3)SALDO TIPO CAPITAL
serie=c(2007,2043)

vec_ind1 = get_series(serie[1], start_date = "1994-10-01")
vec_ind2 = get_series(serie[2], start_date = "1994-10-01")

base2=cbind(vec_ind1,vec_ind2[,2])
rm(list=objects(pattern="vec_ind[0-9]"))

str(base2)
names(base2)=c("Data","2007","2043")
base2[,-1]=apply(base2[,-1],2,function(x)as.numeric(gsub("\\.","",x)))
str(base2)

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=36482"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
ipca= ipea.table[which(ipea.table$V1=="1994.10"):which(ipea.table$V1==daTa2),]
ipca=as.numeric(gsub(",","\\.",gsub("\\.","",ipca[,2])))

base2=cbind(base2,ipca)
base2[,5:6]=apply(base2[,2:3],2,function(x) (x/ipca)*ipca[length(ipca)])
base2[,7:8]=apply(base2[,5:6],2,function(x) (x/rowSums(base2[,5:6]))*100)

names(base2)=c("Data","2007 - Saldos das operações de crédito das instituições financeiras sob controle público - Total - u.m.c. (milhões)",
               "2043 - Saldos das operações de crédito das instituições financeiras sob controle privado - Total - u.m.c. (milhões)",
               "IPCA","Público","Privado","Público (% Total de Crédito)","Privado (% Total de Crédito)")
base2=cbind(Data=substr(base2[,1],4,10),base2[,-1])

write.xlsx(base2,"03-Saldo tipo capital.xlsx", row.names=F)

# 4)CONCESSÕES
serie=c(20634,20635,20662,20685,20686,20698,20631,20632,20633)

vec_ind1 = get_series(serie[1])
vec_ind2 = get_series(serie[2])
vec_ind3 = get_series(serie[3])
vec_ind4 = get_series(serie[4])
vec_ind5 = get_series(serie[5])
vec_ind6 = get_series(serie[6])
vec_ind7 = get_series(serie[7])
vec_ind8 = get_series(serie[8])
vec_ind9 = get_series(serie[9])

base3=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2],vec_ind7[,2],vec_ind8[,2],vec_ind9[,2])
rm(list=objects(pattern="vec_ind[0-9]"))

str(base3)
base3[,-1]=apply(base3[,-1],2,function(x)as.numeric(gsub("\\.","",x)))
str(base3)


url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=36482"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
ipca= ipea.table[which(ipea.table$V1=="2011.03"):which(ipea.table$V1==daTa2),]
ipca=as.numeric(gsub(",","\\.",gsub("\\.","",ipca[,2])))

base3=cbind(base3,ipca)

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044792"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
dias_uteis= ipea.table[which(ipea.table$V1=="2011.03"):which(ipea.table$V1==daTa2),]

base3=cbind(base3,dias_uteis=dias_uteis[,2])

neww=apply(base3[,2:10],2,function(x){neww=x/as.numeric(dias_uteis[,2]);return(neww)})
base3=cbind(base3,neww)
neww1=apply(base3[,13:21],2,function(x){neww1=(x/base3$ipca)*base3$ipca[length(base3$ipca)];return(neww1)})
base3=cbind(base3,neww1)
base3=cbind(Data=substr(base3[,1],4,10),base3[,-1])

names(base3)=c("Data","20634 Recursos livres - Concessões de crédito com recursos livres - Total - R$ (milhões)",
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
               "Recursos Livres Deflacionado  Diário - Total","Recursos Livres Deflacionado Diário - PJ",
               "Recursos Livres Deflacionado Diário - PF","Recursos Direcionados Deflacionado Diário - Total",
               "Recursos Direcionados Deflacionado  (reais desse mes)  Diário - PJ","Recursos Direcionados Deflacionado  (reais desse mes)  Diário - PF",
               "Total Deflacionado  (reais desse mes)  Diário - Total","Total Deflacionado  (reais desse mes)  Diário - PJ",
               "Total Deflacionado  (reais desse mes)  Diário - PF"
               
)

write.xlsx(base3,"04-Concessões.xlsx", row.names=F)

# 5)ENDIVIDAMENTO
serie=c(19882,20400,20622)

vec_ind1 = get_series(serie[1])
vec_ind2 = get_series(serie[2])
vec_ind3 = get_series(serie[3], start_date="2005-01-01")
vec_ind3 <- vec_ind3[-149, ] #exclui a última observação porque as outras séries possuem uma observação a menos 


base4=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2])
rm(list=objects(pattern="vec_ind[0-9]"))

str(base4)
names(base4)=c("Data","19882","20400","20622")
base4[,-1]=apply(base4[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
str(base4)
base4[,5]=base4[,2]-base4[,3]
base4=base4[,c(1,2,3,5,4)]

add1=apply(base4[,2:4],2,function(x){
  add1=rep(NA,10)
  for(i in 11:dim(base4)[1]) add1[i]=x[i]-x[i-10]
  return(add1)
})

base4=cbind(base4[,1:4],add1,X20622=base4[,5])
names(base4)=c("Data","19882 - Endividamento das famílias com o Sistema Financeiro Nacional em relação à renda acumulada dos útimos doze meses - %",
               "20400 - Endividamento das famílias com o Sistema Financeiro Nacional exceto crédito habitacional em relação à renda acumulada dos últimos doze meses - %",
               "Crédito Habitacional",
               "19882 - Endividamento das famílias com o Sistema Financeiro Nacional em relação à renda acumulada dos últimos doze meses - var %",
               "20400 - Endividamento das famílias com o Sistema Financeiro Nacional exceto crédito habitacional em relação à renda acumulada dos últimos doze meses - var %",
               "Crédito Habitacional - var %",
               "20622 - Saldo da carteira de crédito em relação ao PIB - %")
base4=cbind(Data=substr(base4[,1],4,10),base4[,-1])

write.xlsx(base4,"05-Endividamento.xlsx", row.names=F)

# 6)COMPROMETIMENTO RENDA
serie=c(19881)

base5= get_series(serie[1], start_date = "2005-03-01")

str(base5)
names(base5)=c("Data","19881 - Comprometimento de renda das famílias com o serviço da dívida com o Sistema Financeiro Nacional - Com ajuste sazonal - %")

write.xlsx(base5,"06-Comprometimento Renda.xlsx")

# 7)INADIMPLÊNCIA
serie=c(21086,21112,21003,21004,21005,21082,21133,21145,21083,21084,21085,21132)


vec_ind1 = get_series(serie[1])
vec_ind2 = get_series(serie[2])
vec_ind3 = get_series(serie[3])
vec_ind4 = get_series(serie[4])
vec_ind5 = get_series(serie[5])
vec_ind6 = get_series(serie[6])
vec_ind7 = get_series(serie[7])
vec_ind8 = get_series(serie[8])
vec_ind9 = get_series(serie[9])
vec_ind10 = get_series(serie[10])
vec_ind11 = get_series(serie[11])
vec_ind12 = get_series(serie[12])

base6=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2],vec_ind7[,2],vec_ind8[,2],vec_ind9[,2],vec_ind10[,2],vec_ind11[,2],vec_ind12[,2])
rm(list=objects(pattern="vec_ind[0-9]"))

str(base6)
base6[,-1]=apply(base6[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
names(base6)=c("Data","21086 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total - %",
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

write.xlsx(base6,"07-Inadimplência.xlsx", row.names = F)

# 8)DADOS SPREAD 
serie=c(20715,20716,20714,20784,20785,20783,432,7806)

vec_ind1 = get_series(serie[1])
vec_ind2 = get_series(serie[2])
vec_ind3 = get_series(serie[3])
vec_ind4 = get_series(serie[4])
vec_ind5 = get_series(serie[5])
vec_ind6 = get_series(serie[6])
vec_ind7 = get_series(serie[7], start_date = "2011-03-01")
vec_ind8 = get_series(serie[8], start_date = "2011-03-01")


base7.a=merge(vec_ind7,vec_ind8,by="date",all=F)
base7.a[,-1]=apply(base7.a[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
ind=paste0(substr(base7.a$date,1,4),substr(base7.a$date,6,7))#paste0(ano,mes)

# Coleta o valor da série em todos os últimos dias de cada mês (cálculo realizado para ambas as variáveis)
meta=apply(base7.a[,-1],2,function(x) tapply(x[!is.na(x)],ind[!is.na(x)],function(y) y[length(y)]))
meta=meta[1:which(row.names(meta)==daTa1),]

base7.b=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2])
base7.b[,-1]=apply(base7.b[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

PJ=base7.b[,2]-base7.b[,5]
PF=base7.b[,3]-base7.b[,6]
Tot=base7.b[,4]-base7.b[,7]
#excluir a última linha da série 'meta' - que tem um mês a mais - NÃO PRECISA MAIS DISSO!
base7=cbind(substr(base7.b$date,4,10),base7.b[,-1],PJ,PF,Tot,meta)
names(base7)=c("Data","X20715","X20716","X20714","X20784","X20785","X20783","PJ","PF","TOT","META","X7806")
rm(list=objects(pattern="vec_ind[0-9]"))

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044792"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
dias_uteis= ipea.table[which(ipea.table$V1=="2011.03"):which(ipea.table$V1==daTa2),]

names(base7)=c("Data","20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
               "20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
               "20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
               "20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.",
               "20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.",
               "20783 - Spread médio das operações de crédito - Total - p.p.",
               "Pessoas Jurídicas - Taxa de Captação","Pessoas Físicas - Taxa de Captação","Total - Taxa de Captação",
               "432 - Meta Selic","7806 - Swap_DI_Pre_360 dias")

write.xlsx(base7,"08-Dados Spread.xlsx", row.names = F)

# 9)JUROS
serie=c(20714,20715,20716,20717,20756,4189)

vec_ind1 = get_series(serie[1])
vec_ind2 = get_series(serie[2])
vec_ind3 = get_series(serie[3])
vec_ind4 = get_series(serie[4])
vec_ind5 = get_series(serie[5])
vec_ind6 = get_series(serie[6], start_date = "2011-03-01")
vec_ind6 <- vec_ind6[-sum(dim(vec_ind6)[1],-1):-dim(vec_ind6)[1], ] #exclui as duas últimas observações porque as outras séries possuem duas observações a menos 


base8=cbind(vec_ind1,vec_ind2[,-1],vec_ind3[,-1],vec_ind4[,-1],vec_ind5[,-1],vec_ind6[,-1])
base8[,-1]=apply(base8[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
rm(list=objects(pattern="vec_ind[0-9]"))

addd=apply(base8[,2:6],2,function(x){
  addd=rep(NA,12)
  for(i in 13:dim(base8)[1]) addd[i]=x[i]-x[i-12]
  return(addd)
})
base8=cbind(base8,addd)
names(base8)=c("Data","20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
               "20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
               "20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
               "20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.",
               "20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.",
               "4189 - Taxa de juros - Selic acumulada no mês anualizada base 252 - % a.a.",
               "Total","Pessoas jurídicas","Pessoas físicas","crédito com recursos livres","crédito com recursos direcionados - "
               
)
base8=cbind(Data=substr(base8[,1],4,10),base8[,-1])

write.xlsx(base8,"09-Juros.xlsx", row.names = F)

# 10)SALDO TOTAL SÉRIE ENCADEADA

serie=c(20539)

base9=get_series(serie[1])
names(base9)=c("Data","X20539")

# Baixa o IPCA do SIDRA conforme está na planilha
# lembrar de mudar a data para o mês atual
# Diferenças nos decimais em relação à série do IPEADATA geram grande diferença no resultado
IPCA=series_sidra(x = c(1737), from = 198806, to = daTa1, territory = "brazil",variable = "allxp")
ipca = data.frame(IPCA[[1]])

ipca=ipca[which(ipca$Unidade.de.Medida=="Número-índice"),]
ipca=ipca[which(ipca$Data=="1988-06-01"):dim(ipca)[1],]
base9=cbind(base9,ipca=ipca$Valor)
base9$Data <- format(as.Date(base9$Data), "%m/%Y")
saldo_cred=(base9$X20539/base9$ipca)*base9$ipca[which(base9$Data==daTa3)]
base9=cbind(base9,saldo_cred)


add1=apply(base9[,c(4,2)],2,function(x){
  add1=rep(NA,12)
  for(i in 13:dim(base9)[1]) add1[i]=((x[i]/x[i-12])-1)*100
  return(add1)
})

base9=cbind(base9,add1)
names(base9)=c("Data","Saldo crédito total série antiga e nova - R$ milhões",
               "IPCA - geral - Índice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA12",
               "Saldo crédito total série antiga e nova - R$ milhões de out/2016","","")

write.xlsx(base9,"10-Saldo total série encadeada.xlsx", row.names = F)

# 11) RESUMO
`R$ Bilhões`=c(
  base1$`20539 - Saldo da carteira de crédito - Total - R$ (milhões)`[length(base1$`20539 - Saldo da carteira de crédito - Total - R$ (milhões)`)],
  base1$`20541 - Saldo da carteira de crédito - Pessoas físicas - Total - R$ (milhões)`[length(base1$`20541 - Saldo da carteira de crédito - Pessoas físicas - Total - R$ (milhões)`)],
  base1$`20540 - Saldo da carteira de crédito - Pessoas jurídicas - Total - R$ (milhões)`[length(base1$`20540 - Saldo da carteira de crédito - Pessoas jurídicas - Total - R$ (milhões)`)],
  base1$`20542 - Saldo da carteira de crédito com recursos livres - Total - R$ (milhões)`[length(base1$`20542 - Saldo da carteira de crédito com recursos livres - Total - R$ (milhões)`)],
  base1$`20593 - Saldo da carteira de crédito com recursos direcionados - Total - R$ (milhões)`[length(base1$`20593 - Saldo da carteira de crédito com recursos direcionados - Total - R$ (milhões)`)]
)

`Variação real* 12 meses (%)`=c(
  base1$`Total Deflacionado-Total Geral`[length(base1$`Total Deflacionado-Total Geral`)],
  base1$`Total Deflacionado-PF`[length(base1$`Total Deflacionado-PF`)],
  base1$`Total Deflacionado-PJ`[length(base1$`Total Deflacionado-PJ`)],
  base1$`Recursos Livres Deflacionado-Total`[length(base1$`Recursos Livres Deflacionado-Total`)],
  base1$`Recursos Direcionados Deflacionado-Total`[length(base1$`Recursos Direcionados Deflacionado-Total`)]
)

`% do PIB`=c(
  base$`Saldo da carteira de crédito em relação ao PIB - % - 20622`[length(base$`Saldo da carteira de crédito em relação ao PIB - % - 20622`)],
  base$`Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624`[length(base$`Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624`)],
  base$`Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623`[length(base$`Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623`)],
  base$`Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625`[length(base$`Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625`)],
  base$`Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628`[length(base$`Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628`)]
)

`Variação em 12 meses em pontos de porcentagem do PIB`=c(
  base$`Variação YoY1`[length(base$`Variação YoY1`)],
  base$`Variação YoY3`[length(base$`Variação YoY3`)],
  base$`Variação YoY2`[length(base$`Variação YoY2`)],
  base$`Variação YoY4`[length(base$`Variação YoY4`)],
  base$`Variação YoY5`[length(base$`Variação YoY5`)]
)

tabela1=cbind(`R$ Bilhões`,`Variação real* 12 meses (%)`,`% do PIB`,`Variação em 12 meses em pontos de porcentagem do PIB`)
row.names(tabela1)=c("Total","Pessoa Física","Pessoa Jurídica","Crédito Livre","Crédito Direcionado")
write.xlsx(tabela1,"11-Tabela1.xlsx")

#12) Concessões (média diária)

`R$ bilhões`=c(
  base3$`20631 Total- Concessões de crédito - Total - R$ (milhões)`[length(base3$`20631 Total- Concessões de crédito - Total - R$ (milhões)`)],
  base3$`20633 Total - Concessões de crédito - Pessoas físicas - Total - R$ (milhões)`[length(base3$`20633 Total - Concessões de crédito - Pessoas físicas - Total - R$ (milhões)`)],
  base3$`20632 Total - Concessões de crédito - Pessoas jurídicas - Total - R$ (milhões)`[length(base3$`20632 Total - Concessões de crédito - Pessoas jurídicas - Total - R$ (milhões)`)],
  base3$`20634 Recursos livres - Concessões de crédito com recursos livres - Total - R$ (milhões)`[length(base3$`20634 Recursos livres - Concessões de crédito com recursos livres - Total - R$ (milhões)`)],
  base3$`20685 Recursos Direcionados - Concessões de crédito com recursos direcionados - Total - R$ (milhões)`[length(base3$`20685 Recursos Direcionados - Concessões de crédito com recursos direcionados - Total - R$ (milhões)`)]
)
`Variação real* 12 meses(%)`=c(
  base3$`Total Deflacionado Diário - Total Geral`[length(base3$`Total Deflacionado Diário - Total Geral`)],
  base3$`Total Deflacionado Diário - Pessoa Física`[length(base3$`Total Deflacionado Diário - Pessoa Física`)],
  base3$`Total Deflacionado Diário - Pessoa Jurídica`[length(base3$`Total Deflacionado Diário - Pessoa Jurídica`)],
  base3$`Recursos Livres Deflacionado Diário - Recursos Livres-Total`[length(base3$`Recursos Livres Deflacionado Diário - Recursos Livres-Total`)],
  base3$`Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total`[length(base3$`Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total`)]
)

`Concessões (Média diária)`=cbind(`R$ bilhões`,`Variação real* 12 meses(%)`)
row.names(`Concessões (Média diária)`)=c("Total","Pessoa Física","Pessoa Jurídica","Crédito Livre","Direcion")
write.xlsx(`Concessões (Média diária)`,"12-Concessões (média diária).xlsx")

# 13) Resumo Crédito

`Tx de juros %a.a`=c(
  base8$`20714 - Taxa média de juros das operações de crédito - Total - % a.a.`[length(base8$`20714 - Taxa média de juros das operações de crédito - Total - % a.a.`)],
  base8$`20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.`[length(base8$`20716 - Taxa média de juros das operaçõees de crédito - Pessoas físicas - Total - % a.a.`)],
  base8$`20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.`[length(base8$`20715 - Taxa média de juros das operaçõees de crédito - Pessoas jurídicas - Total - % a.a.`)],
  base8$`20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.`[length(base8$`20717 - Taxa média de juros das operaçõees de crédito com recursos livres - Total - % a.a.`)],
  base8$`20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.`[length(base8$`20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.`)]
)
`Tx de juros Variação 12 Meses (p.p.)`=c(
  base8$Total[length(base8$Total)],
  base8$`Pessoas físicas`[length(base8$`Pessoas físicas`)],
  base8$`Pessoas jurídicas`[length(base8$`Pessoas jurídicas`)],
  base8$`crédito com recursos livres`[length(base8$`crédito com recursos livres`)],
  base8$`crédito com recursos direcionados - `[length(base8$`crédito com recursos direcionados - `)]
)

`Inadimplência (%)`=c(
  base6$`21082 - Inadimplência da carteira de crédito - Total`[length(base6$`21082 - Inadimplência da carteira de crédito - Total`)],
  base6$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`[length(base6$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`)],
  base6$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`[length(base6$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`)],
  base6$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`[length(base6$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`)],
  base6$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`[length(base6$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`)]
)

`Inadimplência (%) Variação 12 Meses (p.p.)`=c(
  `Inadimplência (%)`[1]- base6$`21082 - Inadimplência da carteira de crédito - Total`[length(base6$`21082 - Inadimplência da carteira de crédito - Total`)-12],
  `Inadimplência (%)`[2]-base6$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`[length(base6$`21084 - Inadimplência da carteira de crédito - Pessoas físicas - Total - %`)-12],
  `Inadimplência (%)`[3]-base6$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`[length(base6$`21083 - Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %`)-12],
  `Inadimplência (%)`[4]- base6$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`[length(base6$`21085 - Inadimplência da carteira de crédito com recursos livres - Total - %`)-12],
  `Inadimplência (%)`[5]-base6$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`[length(base6$`21132 - Inadimplência da carteira de crédito com recursos direcionados - Total - %`)-12]
)

assign(paste("Crédito - resumo",base6$Data[length(base6$Data)]),cbind(tabela1,`Concessões (Média diária)`,`Tx de juros %a.a`,`Tx de juros Variação 12 Meses (p.p.)`,`Inadimplência (%)`,`Inadimplência (%) Variação 12 Meses (p.p.)`))
mget(paste("Crédito - resumo",base6$Data[length(base6$Data)]))

write.xlsx(mget(paste("Crédito - resumo",base6$Data[length(base6$Data)])),paste0("13-",gsub("/","-",paste0("Crédito resumo ",base6$Data[length(base6$Data)],".xlsx"))))

#14) DADOS INADIMPL PF REC LIVRES
serie=c(7938, 21112)
v1=get_series(serie[1])
v2=get_series(serie[2])

v3=c(rep(NA,which(v1$date=="2011-02-01")),v2[,2])
base10=data.frame(unique(c(as.character(v1[,1]),as.character(v2[,1]))),c(v1[,2],rep(NA,length(v3)-length(v1[,2]))),v3)
base10[,4]=base10[,2]/base10[,3]
base10[,5]=base10[,2]-base10[,3]
add1=rep(NA,12)
for(i in 13:dim(base10)[1]) add1[i]=base10[i,3]-base10[i-12,3]
base10[,6]=add1

names(base10)=c("Data","7938 - Operações de crédito com recursos livres referenciais para taxa de juros - Inadimplência acima de 90 dias em relação ao total da modalidade - Total pessoa física - %   (Série antiga)",
                "21112 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Total - %  (Série nova)","v3[,2]/v3[,3]","v3[,2]-v3[,3]","v3[i,3]-v3[i-12,3]")
base10=cbind(Data=substr(base10[,1],4,10),base10[,-1])

write.xlsx(base10,paste0(dir_plan,"11-dados Inadimp PF rec livres.xlsx"))

#15) DADOS PJ LIVRE LONGO
serie=c("7937","21086")
datai="01/06/2000"
dataf=daTa4

vec_ind1=get_series(serie[1])
vec_ind2=get_series(serie[2])

base20=merge(vec_ind1,vec_ind2,by="data",all=T)
base20=base20[order(paste0(substr(base20$data,7,10),substr(base20$data,4,5))),]
base20[,-1]=apply(base20[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
rm(list=objects(pattern="vec_ind[0-9]"))

names(base20)=c("Data","7937 - Operações de crédito com recursos livres referenciais   para taxa de juros - Inadimplência acima de 90 dias em relação ao total da modalidade - Total pessoa jurídica - %",
                "21086 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total - %")
base20=cbind(Data=substr(base20[,1],4,10),base20[,-1])

write.xlsx(base20,paste0(dir_plan,"12-Dados PJ LIVRE longo.xlsx"))

