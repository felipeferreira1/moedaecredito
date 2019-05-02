#################################################################################################################
# GECON
# ÁREA: CRÉDITO E JUROS
# PLANILHA CRÉDITO
# PEDRO HENRIQUE T. O. SOUSA e FELIPE SIMPLÍCIO FERREIRA
# DATA: 09-05-2017
#################################################################################################################

# COMENTÁRIOS GERAIS:
# VALORES DAS SÉRIES DO BCB NÃO BATEM EXATAMENTE COM OS VALORES DA PLANILHA
# RESULTADO DA DESSAZONALIZAÇÃO DEPENDE DO MODELO E POR ISSO OS VALORES NÃO BATEM


#ATUALIZAR DATAS
daTa1="201914"
daTa2="2019.04"
daTa3="04/2019"
daTa4="30/04/2019"


# DEFINIR PASTAS DE RESULTADOS:
getwd()
setwd("//SRJN3/area_corporativa/Projeto GAP-DIMAC/Automatizações/Dados de crédito - Mensal")
dir_plot="//SRJN3/area_corporativa/Projeto GAP-DIMAC/Automatizações/Dados de crédito - Mensal/Graficos"
dir_plan="//SRJN3/area_corporativa/Projeto GAP-DIMAC/Automatizações/Dados de crédito - Mensal/Tabelas"

# PACOTES REQUERIDOS:
# INSTALAR QUANDO NECESSÁRIO
# EXEMPLO: install.packages("pryr")
require(XML)
require(pryr)
require(ecoseries) 
require(rjson)
require(tibble)
require(magrittr)
require(zoo)
require(seasonal)
require(xlsx)
require(RCurl)

# SALDO % PIB
serie=c("20622","20623","20624","20625","20626","20627","20628", "20629", "20630", "21299", "21300", "21301", "21302")
datai="01/03/2011"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind4=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[4],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[5],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind6=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[6],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind7=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[7],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind8=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[8],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind9=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[9],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind10=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[10],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind11=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[11],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind12=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[12],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind13=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[13],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")


base=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2], vec_ind6[,2], vec_ind7[,2], vec_ind8[,2], vec_ind9[,2], vec_ind10[,2], vec_ind11[,2], vec_ind12[,2], vec_ind13[,2])
rm(list=objects(pattern="vec_ind[0-13]"))

str(base)
names(base)=c("Data","20622","20623","20624","20625","20626","20627","20628", "20629", "20630", "21299", "21300", "21301", "21302")
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
base=base[,c(1,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,21,10,23,11,24,12,25,13,26,14,27)]
base=cbind(substr(base[,1],4,10),base[,-1])
names(base)=c("Data","Saldo da carteira de crédito em relação ao PIB - % - 20622","Variação YoY1",
              "Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - % - 20623","Variação YoY2",
              "Saldo da carteira de crédito a pessoas físicas em relação ao PIB - % - 20624","Variação YoY3",
              "Saldo da carteira de crédito com recursos livres em relação ao PIB - % - 20625","Variação YoY4",
              "Saldo de crédito livre - Pessoas jurídicas / PIB - % - 20626","Variação YoY5", 
              "Saldo de crédito livre - Pessoas físicas / PIB - % - 20627","Variação YoY6",
              "Saldo da carteira de crédito com recursos direcionados em relação ao PIB - % - 20628","Variação YoY7",
              "Saldo de crédito direcionado - Pessoas jurídicas / PIB - % - 20629","Variação YoY8",
              "Saldo de crédito direcionado - Pessoas físicas / PIB - % - 20630","Variação YoY9",
              "Saldo das operações de crédito das instituições financeiras sob controle privado em relação ao PIB", "Variação YoY10",
              "Saldo das operações de crédito das instituições financeiras sob controle público em relação ao PIB", "Variação YoY11",
              "Saldo das operações de crédito das instituições financeiras sob controle privado nacional em relação ao PIB", "Variação YoY12",
              "Saldo das operações de crédito das instituições financeiras sob controle estrangeiro em relação ao PIB", "Variação YoY13")

write.csv2(base,paste0(dir_plan,"01-Saldo %PIB.csv"))

# GRÁF % PIB
png(paste0(dir_plot,"01-Gráf PIB.png"),width = 800, height = 700)
plot(base$`Saldo da carteira de crédito em relação ao PIB - % - 20622`,type="l",lwd=3,ylab="",xlab="",ylim=c(20,55),axes=F,main="Saldo da carteira de crÃ©dito em relaÃ§Ã£o ao PIB - %")
ind=c(T,rep(F,length(base$Data)%/%30))# Vetor lógico que será utilizado para definir os rótulos no eixo x. 
                                      # O número que vem após o operador %/% representa o número de rótulos que se deseja imprimir na tela.
                                      # Para %/%30, espera-se um número de rótulos próximo de 30 no gráfico final.
# Define-se o eixo x no gráfico com base no vetor lógico "ind".
# at indica a localização dos rótulos no eixo x.
# labels indica quais são os rótulos.
axis(1,at=which(c(rep(ind,length.out=(length(base$Data)-1)),T)==T),labels=base$Data[c(rep(ind,length.out=(length(base$Data)-1)),T)],las=2)
# Define-se o eixo y
axis(2,seq(20,60,5))
# Impressão de valores específicos no gráfico.
legend(x=dim(base)[1]-20,y=base[which(base$Data=="12/2015"),2]+2,paste(base[which(base$Data=="12/2015"),1],round(base[which(base$Data=="12/2015"),2],digits=2),sep="; "),bty="n",cex=0.8)
legend(x=dim(base)[1]-20,y=base[dim(base)[1],2]-4,paste(base[dim(base)[1],1],round(base[dim(base)[1],2],digits=2),sep="; "),bty="n",cex=0.8)
dev.off()

# SALDO TIPO RECURSO
serie=c("20542","20543","20570","20593","20594","20606","20539","20540","20541")
datai="01/03/2007"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind4=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[4],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[5],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind6=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[6],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind7=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[7],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind8=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[8],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind9=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[9],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

base1=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2],vec_ind7[,2],vec_ind8[,2],vec_ind9[,2])
rm(list=objects(pattern="vec_ind[0-9]"))
str(base1)

base1[,-1]=apply(base1[,-1],2,function(x)as.numeric(gsub("\\.","",x)))
str(base1)
require(RCurl)
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

write.csv2(base1,paste0(dir_plan,"02-Saldo Tipo Recurso.csv"))

# GRÁF LIV E DIR
png(paste0(dir_plot,"02-Gráf Liv e dir.png"),width = 800, height = 700)
base1_plot=base1[which(base1$Data=="01/2012"):which(base1$Data=="01/2017"),]

plot(base1_plot[which(base1_plot$Data=="01/2012"):which(base1_plot$Data=="01/2017"),]$`Total Deflacionado-Total Geral`,
    ylim=c(-20,30),type="h",axes=F,ylab="",xlab="",main="Saldo da carteira de crédito - var% real t/t-12 (IPCA)",lwd=8)
lines(base1_plot$`Recursos Livres Deflacionado-Total`,type="l",lwd=3)
lines(base1_plot$`Recursos Direcionados Deflacionado-Total`,type="l",col="red",lwd=3)
ind=c(T,rep(F,length(base1_plot$Data)%/%31))
axis(1,at=which(c(rep(ind,length.out=(length(base1_plot$Data)-1)),T)==T),labels=base1_plot$Data[c(rep(ind,length.out=(length(base1_plot$Data)-1)),T)],las=2)
axis(2,seq(-15,30,5))
dev.off()

# GRÁF PF E PJ
png(paste0(dir_plot,"03-Gráf PF e PJ.png"),width = 800, height = 700)
plot(base1_plot$`Total Deflacionado-Total Geral`,type="l",lwd=3, ylim=c(-15,20),ylab="",xlab="",axes=F,main="Saldo das operações de crédito do SFN, deflacionado pelo IPCA, var% t/t-12")
lines(base1_plot$`Total Deflacionado-PJ`,type="l",col="red",lwd=3)
lines(base1_plot$`Total Deflacionado-PF`,type="l",col="blue",lwd=3)
ind=c(T,rep(F,length(base1_plot$Data)%/%31))
axis(1,at=which(c(rep(ind,length.out=(length(base1_plot$Data)-1)),T)==T),labels=base1_plot$Data[c(rep(ind,length.out=(length(base1_plot$Data)-1)),T)],las=2)
axis(2,seq(-20,20,5))
legend(x=dim(base1_plot)[1]-10,y=base1_plot[dim(base1_plot)[1],28]+0.5,paste(base1_plot[dim(base1_plot)[1],1],round(base1_plot[dim(base1_plot)[1]-1,28],digits=2),sep="; "),bty="n",cex=0.8)
legend("topleft",legend = c("Total Geral","PJ","PF"),bty="n",cex=0.8,lty=1,col=c("black","red","blue"),lwd=3)
dev.off()

# SALDO TIPO CAPITAL
serie=c("2007","2043")
datai="01/10/1994"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

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

write.csv2(base2,paste0(dir_plan,"03-Saldo tipo capital.csv"))

# GRÁF TIPO DE CAP
base2_plot=base2[which(base2$Data=="09/2010"):which(base2$Data=="12/2016"),]
#ordenando os levels do fator
base2_plot$Data=factor(base2_plot$Data,levels(base2_plot$Data)[c(match(base2_plot$Data,levels(base2_plot$Data)))])

png(paste0(dir_plot,"04-Gráf tipo de cap.png"),width = 800, height = 700)
plot(base2_plot$Data,base2_plot$Público,type="l",ylim=c(600000,2600000),axes=F,ylab="",xlab="",main="Saldo de crédito por tipo de controle de capital - R$ milhões de jan./2017")
lines(base2_plot$Privado,type="l",col="red",lwd=3)
ind=c(T,rep(F,length(base2_plot$Data)%/%38))
axis(1,at=which(c(rep(ind,length.out=(length(base2_plot$Data)-1)),T)==T),labels=base2_plot$Data[c(rep(ind,length.out=(length(base2_plot$Data)-1)),T)],las=2)
axis(2,seq(600000,2600000,200000),las=1,cex.axis=0.8)
legend("topleft",legend = c("Público","Privado"),bty="n",cex=0.8,lty=2:1,col=c("black","red"),lwd=3)
dev.off()


# GRÁF TIPO DE CAP %
base2$Data=factor(base2$Data,levels(base2$Data)[c(match(base2$Data,levels(base2$Data)))])

png(paste0(dir_plot,"05-Gráf tipo de cap perc.png"),width = 800, height = 600)
plot(base2$Data,base2$`Público (% Total de Crédito)`,type="l",lwd=3,ylim=c(30,70),las=2,axes=F,main="Saldo por tipo de capital  - participação %")
ind=c(T,rep(F,length(base2$Data)%/%45))
axis(1,at=which(c(rep(ind,length.out=(length(base2$Data)-1)),T)==T),labels=base2$Data[c(rep(ind,length.out=(length(base2$Data)-1)),T)],las=2)
axis(2,seq(30,70,5),las=1)
lines(base2$Data,base2$`Privado (% Total de Crédito)`,col="red",lwd=3)
legend(x=dim(base2)[1]-50,y=base2[dim(base2)[1],7]+4,paste(base2[dim(base2)[1],1],round(base2[dim(base2)[1]-1,7],digits=2),sep="; "),bty="n",cex=0.8)
legend(x=dim(base2)[1]-50,y=base2[dim(base2)[1],8]-2,paste(base2[dim(base2)[1],1],round(base2[dim(base2)[1]-1,8],digits=2),sep="; "),bty="n",cex=0.8)
legend("topleft",legend = c("Público (% Total de Crédito)","Privado (% Total de Crédito)"),bty="n",cex=0.8,lty=c(3,1),lwd=3,col=c("black","red"))
dev.off()

# CONCESSÕES
serie=c("20634","20635","20662","20685","20686","20698","20631","20632","20633")
datai="01/03/2011"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind4=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[4],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[5],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind6=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[6],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind7=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[7],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind8=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[8],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind9=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[9],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

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
#mudar as datas das tabelas do ipeadata

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044792"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
dias_uteis= ipea.table[which(ipea.table$V1=="2011.03"):which(ipea.table$V1=="2017.07"),]

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044795"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
dias_uteis1= ipea.table[which(ipea.table$V1=="2017.07"):which(ipea.table$V1==daTa2),]
dias_uteis=rbind(dias_uteis,dias_uteis1)

dias_uteis2=dias_uteis[-78,]
base3=cbind(base3,dias_uteis2=dias_uteis2[,2])

neww=apply(base3[,2:10],2,function(x){neww=x/as.numeric(dias_uteis2[,2]);return(neww)})
base3=cbind(base3,neww)
neww1=apply(base3[,13:21],2,function(x){neww1=(x/base3$ipca)*base3$ipca[length(base3$ipca)];return(neww1)})
base3=cbind(base3,neww1)
base3=cbind(Data=substr(base3[,1],4,10),base3[,-1])

# Cria coluna com o número de dias do mês para qualquer data dada
# Necessário para implementar a dessazonalização (Ipeadata só possui dias úteis)
# Primeiro verifica se é bissexto (divisível por 4)
# E depois atribui número de dias de cada mês quando o ano é bissexto ou não (objeto dmes)
bissexto.fun=function(x){
  anos=as.numeric(substr(x$Data,4,7))
  bissexto=anos/4
  bissexto[which((bissexto-trunc(bissexto,digits=0))==0)]=1
  bissexto[which((bissexto-trunc(bissexto,digits=0))!=0)]=0
  bissexto=as.logical(bissexto)
  
  fev=as.numeric(substr(x$Data,1,2))==2 #"==2" para "garimpar" fevereiro
  dmes=rowSums(outer(as.numeric(substr(x$Data,1,2)),c(1,3,5,7,8,10,12),"=="))
  dmes[which(dmes==1)]=31
  dmes[which(dmes==0)]=30
  dmes[fev==T]=28
  dmes[bissexto&fev==T]=29
  dmes
  dias_nao_uteis=dmes-as.numeric(dias_uteis[,2])
  indicador=as.numeric(dias_uteis[,2])-(5/2)*dias_nao_uteis
  return(indicador)
}

indicador=bissexto.fun(base3)
indicador=ts(indicador,start = c(2011,3), freq = 12)

dessas=function(x){
                  dessas_dados1=seas(x=ts(x,start = c(2011,3), freq = 12),xreg = indicador, arima.model = "(1 0 0)(1 0 1)",
                                     forecast.maxlead = 0,transform.function = "log",x11.mode = "mult")
       return(as.numeric(final(dessas_dados1))                                                                                                                              )
}
dessas_dados1=apply(base3[,22:30],2,function(x)dessas(x))

base3=cbind(base3,dessas_dados1)

addd=apply(base3[,22:30],2,function(x){
  addd=rep(NA,12)
  for(i in 13:dim(base3)[1]) addd[i]=((x[i]/x[i-12])-1)*100
  return(addd)
})
base3=cbind(base3,addd)

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
               "Recursos Livres Deflacionado (reais de jan/2017)  Diário - Total","Recursos Livres Deflacionado (reais de jan/2017)  Diário - PJ",
               "Recursos Livres Deflacionado (reais de jan/2017)  Diário - PF","Recursos Direcionados Deflacionado  (reais de jan/2017)  Diário - Total",
               "Recursos Direcionados Deflacionado  (reais de jan/2017)  Diário - PJ","Recursos Direcionados Deflacionado  (reais de jan/2017)  Diário - PF",
               "Total Deflacionado  (reais de jan/2017)  Diário - Total","Total Deflacionado  (reais de jan/2017)  Diário - PJ",
               "Total Deflacionado  (reais de jan/2017)  Diário - PF","Dessazonalizado - Recursos Livres Deflacionado (reais de jan/2017) Diário - Total livres",
               "Dessazonalizado - Recursos Livres Deflacionado (reais de jan/2017) Diário - PJ livres","Dessazonalizado - Recursos Livres Deflacionado (reais de jan/2017) Diário - PF livres",
               "Dessazonalizado - Recursos Direcionados Deflacionado (reais de jan/2017) Diário - Total direcionado",
               "Dessazonalizado - Recursos Direcionados Deflacionado (reais de jan/2017) Diário - PJ direcionado",
               "Dessazonalizado - Recursos Direcionados Deflacionado (reais de jan/2017) Diário - PF direcionado",
               "Dessazonalizado - Total Deflacionado (reais de jan/2017) Diário - Total","Dessazonalizado - Total Deflacionado (reais de jan/2017) Diário - PJ",
               "Dessazonalizado - Total Deflacionado (reais de jan/2017) Diário - PF","Recursos Livres Deflacionado Diário - Recursos Livres-Total",
               "Recursos Livres Deflacionado Diário - Recursos Livres-PJ","Recursos Livres Deflacionado Diário - Recursos Livres-PF",
               "Recursos Direcionados Deflacionado Diário - Recursos Direcionados-Total",
               "Recursos Direcionados Deflacionado Diário - Recursos Direcionados-PJ","Recursos Direcionados Deflacionado Diário - Recursos Direcionados-PF",
               "Total Deflacionado Diário - Total Geral","Total Deflacionado Diário - Pessoa Jurídica","Total Deflacionado Diário - Pessoa Física"
               
)

write.csv2(base3,paste0(dir_plan,"04-Concessões.csv"))

# GRÁF CONCESS PJ PF
png(paste0(dir_plot,"06-Gráf concess PJ PF.png"),width = 800, height = 700)
base3_plot=base3[which(base3$Data=="01/2013"):which(base3$Data==daTa3),]
bp=barplot(t(base3_plot[,46:48]),beside=T, col=c("green","blue","gray"),xlab="",ylab="",axisnames=F,axes=F,ylim=c(-30,30))
ind=c(T,rep(F,length(base3$Data)%/%25))
axis(1,at=bp[1,which(c(rep(ind,length.out=(length(base3_plot$Data)-1)),T)==T)],labels=base3_plot$Data[c(rep(ind,length.out=(length(base3_plot$Data)-1)),T)],las=2)
axis(2,seq(-30,30,5),las=1)
legend("topleft",legend = c("Total Geral","Pessoas Jurídicas","Pessoas Físicas"),bty="n",cex=0.8,fill=c("green","blue","gray"))
dev.off()

# GRÁF CONCESS LIV E DIR
png(paste0(dir_plot,"07-Gráf concess liv e dir.png"),width = 800, height = 700)
bp=barplot(t(base3_plot[,c(40,43,46)]),beside=T,col=c("blue","red","green"),
        xlab="",ylab="",axisnames=F,axes=F,ylim=c(-50,90),main="Novas concessões de crédito do SFN, média por dia útil, deflacionada pelo IPCA, var% t/t-12")
ind=c(T,rep(F,length(base3$Data)%/%59))
axis(1,at=bp[1,which(c(rep(ind,length.out=(length(base3_plot$Data)-1)),T)==T)],labels=base3_plot$Data[c(rep(ind,length.out=(length(base3_plot$Data)-1)),T)],las=2)
axis(2,seq(-50,90,10),las=1)
legend("topleft",legend = c("Recursos Livres-Total","Recursos Direcionados-Total","Total Geral"),bty="n",cex=0.8,fill=c("blue","red","green"))
dev.off()

# GRÁF CONC DESSAZ TODAS
png(paste0(dir_plot,"08-Gráf conc dessaz todas.png"),width = 800, height = 700)
plot(ts(base3[,31]),xlab="",ylab="",ylim=c(0,25000),col="#00008B",axes=F,lwd=2)
      lines(ts(base3[,32]),xlab="",ylab="",col="#800000",lwd=2)
      lines(ts(base3[,33]),xlab="",ylab="",col="#6B8E23",lwd=2)
      lines(ts(base3[,34]),xlab="",ylab="",col="#7B68EE",lwd=2)
      lines(ts(base3[,35]),xlab="",ylab="",col="#87CEFA",lwd=2)
      lines(ts(base3[,36]),xlab="",ylab="",col="#F4A460",lwd=2)
      lines(ts(base3[,37]),xlab="",ylab="",col="#6495ED",lwd=2)
      lines(ts(base3[,38]),xlab="",ylab="",col="#CD5C5C",lwd=2)
      lines(ts(base3[,39]),xlab="",ylab="",col="#9ACD32",lwd=2)
ind=c(T,rep(F,length(base3$Data)%/%36))
axis(1,at=which(c(rep(ind,length.out=(length(base3$Data)-1)),T)==T),labels=base3$Data[c(rep(ind,length.out=(length(base3$Data)-1)),T)],las=2)
axis(2,seq(0,25000,5000),las=1)
legend("topright",legend = c("Total livres","PJ livres","PF livres","Total direcionado",
                             "PJ direcionado","PF direcionado","Total","PJ","PF"),bty="n",cex=0.8,lty=rep(1,9),
                            col=c("#00008B","#800000","#6B8E23","#7B68EE","#87CEFA","#F4A460","#6495ED",
                                  "#CD5C5C","#9ACD32"),lwd=2)
dev.off()

# GRÁF CONC DESSAZ LIVRES PF E PJ
png(paste0(dir_plot,"09-Gráf conc dessaz livres PF e PJ.png"),width = 800, height = 700)
plot(ts(base3[,32]),xlab="",ylab="",ylim=c(4000,9000),col="#800000",axes=F,lwd=2)
lines(ts(base3[,33]),xlab="",ylab="",col="#6B8E23",lwd=2)

ind=c(T,rep(F,length(base3$Data)%/%36))
axis(1,at=which(c(rep(ind,length.out=(length(base3$Data)-1)),T)==T),labels=base3$Data[c(rep(ind,length.out=(length(base3$Data)-1)),T)],las=2)
axis(2,seq(4000,90000,500),las=1)
legend("topright",legend = c("PJ livres","PF livres"),bty="n",cex=0.8,lty=rep(1,9),
       col=c("#800000","#6B8E23"),lwd=2)
dev.off()

# GRÁF CONC DESSAZ TODAS (2)
png(paste0(dir_plot,"10-Gráf conc dessaz todas (2).png"),width = 800, height = 700)
plot(ts(base3[,34]),xlab="",ylab="",ylim=c(0,3500),col="#7B68EE",axes=F,lwd=2)
lines(ts(base3[,35]),xlab="",ylab="",col="#87CEFA",lwd=2)
lines(ts(base3[,36]),xlab="",ylab="",col="#F4A460",lwd=2)

ind=c(T,rep(F,length(base3$Data)%/%36))
axis(1,at=which(c(rep(ind,length.out=(length(base3$Data)-1)),T)==T),labels=base3$Data[c(rep(ind,length.out=(length(base3$Data)-1)),T)],las=2)
axis(2,seq(0,3500,500),las=1)
legend("topright",legend = c("Total direcionado","PJ direcionado","PF direcionado"),bty="n",cex=0.8,lty=rep(1,9),
       col=c("#7B68EE","#87CEFA","#F4A460"),lwd=2)
dev.off()

# GRÁF CONCESSÕES DESSAZONALIZADO
png(paste0(dir_plot,"11-Gráf concessões Dessaz.png"),width = 800, height = 700)
plot(ts(base3[,38]),xlab="",ylab="",ylim=c(5000,11000),col="#CD5C5C",axes=F,lwd=2)
lines(ts(base3[,39]),xlab="",ylab="",col="#9ACD32",lwd=2)
ind=c(T,rep(F,length(base3$Data)%/%36))
axis(1,at=which(c(rep(ind,length.out=(length(base3$Data)-1)),T)==T),labels=base3$Data[c(rep(ind,length.out=(length(base3$Data)-1)),T)],las=2)
axis(2,seq(5000,11000,1000),las=1)
legend("topright",legend = c("PJ","PF"),bty="n",cex=0.8,lty=rep(1,9),
       col=c("#CD5C5C","#9ACD32"),lwd=2)
dev.off()

# ENDIVIDAMENTO
# VERIFICAR SE A VARIAÇÃO É EM 10 MESES MESMO 
serie=c("19882","20400","20622")
datai="01/01/2005"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind32 <- vec_ind3[-149, ] #exclui a última observação porque as outras séries possuem uma observação a menos 


base4=cbind(vec_ind1,vec_ind2[,2],vec_ind32[,2])
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

write.csv2(base4,paste0(dir_plan,"05-Endividamento.csv"))

# GRÁF ENDIVIDAMENTO
png(paste0(dir_plot,"12-Gráf endividamento.png"),width = 800, height = 700)
plot(base4[,2],ylim=c(0,60),type="l",axes=F,xlab="",ylab="",lwd=2)
lines(base4[,3],col="red",lwd=2)
lines(base4[,4],col="blue",lwd=2)
lines(base4[,8],col="green4",lwd=2)

ind=c(T,rep(F,length(base4$Data)%/%10))
axis(1,at=which(c(rep(ind,length.out=(length(base4$Data)-1)),T)==T),labels=base4$Data[c(rep(ind,length.out=(length(base4$Data)-1)),T)],las=2)
axis(2,seq(0,60,10),las=1)
legend(x=dim(base4)[1]-30,y=base4[dim(base4)[1]-1,4],paste(base4[dim(base4)[1]-1,1],base4[dim(base4)[1]-1,4],sep="; "),bty="n",cex=0.8)
legend(x=dim(base4)[1]-30,y=base4[dim(base4)[1]-1,3],paste(base4[dim(base4)[1]-1,1],base4[dim(base4)[1]-1,3],sep="; "),bty="n",cex=0.8)
legend(x=dim(base4)[1]-30,y=base4[dim(base4)[1]-1,2],paste(base4[dim(base4)[1]-1,1],base4[dim(base4)[1]-1,2],sep="; "),bty="n",cex=0.8)
legend("topleft",legend = c("Total","Exceto Crédito Habitacional","Crédito habitacional",
                            "20622 - Saldo da carteira de crédito em relação ao PIB-%"),bty="n",cex=0.8,lty=1,col=c("black","red","blue","green4"),lwd=2)
dev.off()

# COMPROMETIMENTO RENDA
serie=c("19881")
datai="01/03/2005"
dataf=daTa4

base5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

str(base5)
base5[,2]=as.numeric(gsub(",",".",as.character(base5[,2])))
base5=data.frame(Data=substr(base5[,1],4,10),base5[,-1])
names(base5)=c("Data","19881 - Comprometimento de renda das famílias com o serviço da dívida com o Sistema Financeiro Nacional - Com ajuste sazonal - %")

write.csv2(base5,paste0(dir_plan,"06-Comprometimento Renda.csv"))

# GRÁF COMPROMET
png(paste0(dir_plot,"13-Gráf compromet.png"),width = 800, height = 700)
plot(base5[,2],type="l",ylim=c(15,25),ylab="",xlab="",axes=F,lwd=2)
ind=c(T,rep(F,length(base5$Data)%/%36))
axis(1,at=which(c(rep(ind,length.out=(length(base5$Data)-1)),T)==T),labels=base5$Data[c(rep(ind,length.out=(length(base5$Data)-1)),T)],las=2)
axis(2,seq(15,25,1),las=1)
legend(x=dim(base5)[1]-18,y=base5[dim(base5)[1],2],paste(base5[dim(base5)[1],1],base5[dim(base5)[1],2],sep="; "),bty="n",cex=0.8)
legend("topleft",legend = c("Comprometimento da renda"),bty="n",cex=0.8,lty=1,col=c("black"),lwd=2)
dev.off()

# INADIMPLÊNCIA
serie=c("21086","21112","21003","21004","21005","21082","21133","21145","21083","21084","21085","21132")
datai="01/03/2011"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind4=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[4],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[5],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind6=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[6],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind7=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[7],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind8=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[8],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind9=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[9],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind10=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[10],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind11=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[11],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind12=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[12],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

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
base6=cbind(Data=substr(base6[,1],4,10),base6[,-1])

write.csv2(base6,paste0(dir_plan,"07-Inadimplência.csv"))

# GRÁF INADIMPLÊNCIA
png(paste0(dir_plot,"14-Gráf inadimp.png"),width = 800, height = 700)
plot(base6[,2],type="l",ylim=c(0,10),ylab="",xlab="",axes=F, lwd=3)
lines(base6[,3],type="l",ylab="",xlab="",col="turquoise3", lwd=3)
ind=c(T,rep(F,length(base6$Data)%/%75))
axis(1,at=which(c(rep(ind,length.out=(length(base6$Data)-1)),T)==T),labels=base6$Data[c(rep(ind,length.out=(length(base6$Data)-1)),T)],las=2)
axis(2,seq(0,10,1),las=1)
legend(x=dim(base6)[1]-10,y=base6[dim(base6)[1],2],paste(base6[dim(base6)[1],1],base6[dim(base6)[1],2],sep="; "),bty="n",cex=0.8)
legend(x=dim(base6)[1]-10,y=base6[dim(base6)[1],3]+1,paste(base6[dim(base6)[1],1],base6[dim(base6)[1],3],sep="; "),bty="n",cex=0.8)
legend("topleft",legend = c("Pessoas Jurídicas","Pessoas Físicas"),bty="n",cex=0.8,lty=1,col=c("black","turquoise3"),lwd=3)
dev.off()

# DADOS SPREAD 
serie=c("20715","20716","20714","20784","20785","20783","432","7806")
datai="01/03/2011"
dataf=daTa4
#definir daTa4 para um mês a frente e ele tem que ser igual ao daTa1

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind4=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[4],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[5],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind6=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[6],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind7=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[7],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind8=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[8],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")


base7.a=merge(vec_ind7,vec_ind8,by="data",all=F)
base7.a[,-1]=apply(base7.a[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
ind=paste0(substr(base7.a$data,7,10),substr(base7.a$data,4,5))#paste0(ano,mes)

# Coleta o valor da série em todos os últimos dias de cada mês (cálculo realizado para ambas as variáveis)
meta=apply(base7.a[,-1],2,function(x) tapply(x[!is.na(x)],ind[!is.na(x)],function(y) y[length(y)]))
meta=meta[1:which(row.names(meta)==201706),]  
#meta=meta[1:which(row.names(meta)==daTa1),] 
#alterar a daTa1 para o mês em questÃ£o

base7.b=cbind(vec_ind1,vec_ind2[,2],vec_ind3[,2],vec_ind4[,2],vec_ind5[,2],vec_ind6[,2])
base7.b[,-1]=apply(base7.b[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))

PJ=base7.b[,2]-base7.b[,5]
PF=base7.b[,3]-base7.b[,6]
Tot=base7.b[,4]-base7.b[,7]
#excluir a última linha da série 'meta' - que tem um mês a mais - NÃO PRECISA MAIS DISSO!
base7=cbind(substr(base7.b$data,4,10),base7.b[,-1],PJ,PF,Tot,meta)
names(base7)=c("Data","X20715","X20716","X20714","X20784","X20785","X20783","PJ","PF","TOT","META","X7806")
rm(list=objects(pattern="vec_ind[0-9]"))

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044792"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
dias_uteis= ipea.table[which(ipea.table$V1=="2011.03"):which(ipea.table$V1=="2016.12"),]

url_ipea="http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=459044795"
ipea.table = readHTMLTable(htmlParse(getURL(url_ipea, useragent="curl/7.39.0 Rcurl/1.95.4.5")), header=T, which=3,stringsAsFactors=F)
dias_uteis1= ipea.table[which(ipea.table$V1=="2017.01"):which(ipea.table$V1==daTa2),]
dias_uteis=rbind(dias_uteis,dias_uteis1)

bissexto.fun=function(x){
  anos=as.numeric(substr(x$Data,4,7))
  bissexto=anos/4
  bissexto[which((bissexto-trunc(bissexto,digits=0))==0)]=1
  bissexto[which((bissexto-trunc(bissexto,digits=0))!=0)]=0
  bissexto=as.logical(bissexto)
  
  fev=as.numeric(substr(x$Data,1,2))==2 #"==2" para "garimpar" fevereiro
  dmes=rowSums(outer(as.numeric(substr(x$Data,1,2)),c(1,3,5,7,8,10,12),"=="))
  dmes[which(dmes==1)]=31
  dmes[which(dmes==0)]=30
  dmes[fev==T]=28
  dmes[bissexto&fev==T]=29
  dmes
  dias_nao_uteis=dmes-as.numeric(dias_uteis[,2])
  indicador=as.numeric(dias_uteis[,2])-(5/2)*dias_nao_uteis
  return(indicador)
}
indicador=bissexto.fun(base7)
indicador=ts(indicador,start = c(2011,3), freq = 12)

dessas=function(x){
  dessas_dados1=seas(x=ts(x,start = c(2011,3), freq = 12),xreg = indicador,arima.model = "(1 0 0)(1 0 1)",
                     forecast.maxlead = 0,transform.function = "log",x11.mode = "mult")
  return(as.numeric(final(dessas_dados1))                                                                                                                              )
}
dessas_dados1=apply(base7[,c(4,10:12,5:7)],2,function(x)dessas(x))

base7=cbind(base7,dessas_dados1)
names(base7)=c("Data","20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
               "20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
               "20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
               "20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.",
               "20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.",
               "20783 - Spread médio das operações de crédito - Total - p.p.",
               "Pessoas Jurídicas - Taxa de Captação","Pessoas Físicas - Taxa de Captação","Total - Taxa de Captação",
               "432 - Meta Selic","7806 - Swap_DI_Pre_360 dias","dessaz Taxa de aplicação (eixo direito)",
               "dessaz Taxa de captação","dessaz Meta Selic","dessaz SWAP DI 360","dessaz spread Pessoa  jurídica",
               "dessaaz spread Pessoa  física","dessaz spread Total")

write.csv2(base7,paste0(dir_plan,"08-Dados Spread.csv"))

# JUROS
serie=c("20714","20715","20716","20717","20756","4189")
datai="01/03/2011"
dataf=daTa4
#retornar o daTa4 ao mês atual

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind3=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[3],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind4=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[4],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind5=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[5],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind6=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[6],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

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

write.csv2(base8,paste0(dir_plan,"09-Juros.csv"))

# GRÁF JUROS
png(paste0(dir_plot,"15-Gráf juros.png"),width = 800, height = 700)
plot(base8[,2],type="l",ylim=c(0,60),ylab="",xlab="",axes=F, lwd=3)
lines(base8[,3],type="l",ylab="",xlab="",col="turquoise3", lwd=3)
lines(base8[,4],type="l",ylab="",xlab="",col="green4", lwd=3)
lines(base8[,5],type="l",ylab="",xlab="",col="red4", lwd=3)
lines(base8[,6],type="l",ylab="",xlab="",col="gray", lwd=3)
lines(base8[,7],type="l",ylab="",xlab="",col="blue3", lwd=3)
ind=c(T,rep(F,length(base8$Data)%/%20))
axis(1,at=which(c(rep(ind,length.out=(length(base8$Data)-1)),T)==T),labels=base8$Data[c(rep(ind,length.out=(length(base8$Data)-1)),T)],las=2)
axis(2,seq(0,60,5),las=1)
legend(x=dim(base8)[1]-7,y=base8[dim(base8)[1]-1,2],paste(base8[dim(base8)[1]-1,1],base8[dim(base8)[1]-1,2],sep="; "),bty="n",cex=0.8)
legend(x=dim(base8)[1]-7,y=base8[dim(base8)[1]-1,3],paste(base8[dim(base8)[1]-1,1],base8[dim(base8)[1]-1,3],sep="; "),bty="n",cex=0.8)
legend(x=dim(base8)[1]-7,y=base8[dim(base8)[1]-1,4],paste(base8[dim(base8)[1]-1,1],base8[dim(base8)[1]-1,4],sep="; "),bty="n",cex=0.8)
legend(x=dim(base8)[1]-7,y=base8[dim(base8)[1]-1,5],paste(base8[dim(base8)[1]-1,1],base8[dim(base8)[1]-1,5],sep="; "),bty="n",cex=0.8)
legend(x=dim(base8)[1]-7,y=base8[dim(base8)[1]-1,6],paste(base8[dim(base8)[1]-1,1],base8[dim(base8)[1]-1,6],sep="; "),bty="n",cex=0.8)
legend(x=dim(base8)[1]-7,y=base8[dim(base8)[1],7]+2,paste(base8[dim(base8)[1],1],base8[dim(base8)[1],7],sep="; "),bty="n",cex=0.8)
legend("topleft",legend = c("20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
                            "20715 - Taxa média de juros das operações de crédito - Pessoas jurídicas - Total - % a.a.",
                            "20716 - Taxa média de juros das operações de crédito - Pessoas físicas - Total - % a.a.",
                            "20717 - Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.",
                            "20756 - Taxa média de juros das operações de crédito com recursos direcionados - Total - % a.a.",
                            "4189 - Taxa de juros - Selic acumulada no mês anualizada base 252 - % a.a.")
                            ,bty="n",cex=0.9,lty=1,col=c("black","turquoise3","green4","red4","gray","blue3"),lwd=3)
dev.off()

# GRÁF JUROS E SELIC
png(paste0(dir_plot,"16-Gráf juros e selic.png"),width = 800, height = 700)
plot(base8[,2],type="l",ylim=c(20,35),ylab="",xlab="",axes=F, lwd=3)
lines(base8[,7]+17,type="l",ylab="",xlab="",col="blue3", lwd=3)
ind=c(T,rep(F,length(base8$Data)%/%35))
axis(1,at=which(c(rep(ind,length.out=(length(base8$Data)-1)),T)==T),labels=base8$Data[c(rep(ind,length.out=(length(base8$Data)-1)),T)],las=2)
axis(2,seq(20,35,2),las=1)
axis(4,at=seq(5+17,15+17,1),labels=seq(5,15,1),las=1)
legend("topleft",legend = c("20714 - Taxa média de juros das operações de crédito - Total - % a.a.",
                            "4189 - Taxa de juros - Selic acumulada no mês anualizada base 252 - % a.a.")
       ,bty="n",cex=0.9,lty=1,col=c("black","blue3"),lwd=3)
dev.off()

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

serie=c("20539")
datai="01/03/2007"
dataf=daTa4

base9=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
base9=data.frame(substr(base9[,1],4,10),as.numeric(base9[,-1]))
names(base9)=c("Data","X20539")
base9=rbind(da,base9)


# Baixa o IPCA do SIDRA conforme está na planilha
# lembrar de mudar a data para o mês atual
# Diferenças nos decimais em relação à série do IPEADATA geram grande diferença no resultado
IPCA=series_sidra(x = c(1737), from = 198801, to = 201705, territory = "brazil",variable = "allxp")
ipca = data.frame(IPCA[[1]])

ipca=ipca[which(ipca$Unidade.de.Medida=="Número-índice"),]
ipca=ipca[which(ipca$Mês=="junho 1988"):dim(ipca)[1],]
base9=cbind(base9,ipca=ipca$Valor)
saldo_cred=(base9$X20539/base9$ipca)*base9$ipca[which(base9$Data=="10/2016")]
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

write.csv2(base9,paste0(dir_plan,"10-Saldo total série encadead.csv"))

# GRÁF SALDO TOTAL SÉRIE ENDADEAD
png(paste0(dir_plot,"17-Gráf saldo total série encadead.png"),width = 800, height = 700)
plot(base9[,4],type="l",ylim=c(0,4000000),ylab="",xlab="",axes=F, lwd=3,main="Saldo crédito total série antiga e nova - R$ milhões de mai/2017")
ind=c(T,rep(F,length(base9$Data)%/%58))
axis(1,at=which(c(rep(ind,length.out=(length(base9$Data)-1)),T)==T),labels=base9$Data[c(rep(ind,length.out=(length(base9$Data)-1)),T)],las=2)
axis(2,seq(0,4000000,500000),las=1)
dev.off()

# GRÁF VAR% SALDO TOTAL ENDADEAD
png(paste0(dir_plot,"18-Gráf var saldo total encadead.png"),width = 800, height = 700)
plot(base9[,5],type="h",ylim=c(-30,30),ylab="",xlab="",axes=F, lwd=1)
ind=c(T,rep(F,length(base9$Data)%/%50))
axis(1,at=which(c(rep(ind,length.out=(length(base9$Data)-1)),T)==T),labels=base9$Data[c(rep(ind,length.out=(length(base9$Data)-1)),T)],las=2)
axis(2,seq(-30,30,10),las=1)
dev.off()

# RESUMO
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
write.csv2(tabela1,paste0(dir_plan,"11-tabela1.csv"))

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
write.csv2(`Concessões (Média diária)`,paste0(dir_plan,"12-Concessões (Média diária).csv"))

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

write.csv2(mget(paste("Crédito - resumo",base6$Data[length(base6$Data)])),paste0(dir_plan,"13-",gsub("/","-",paste0("Crédito resumo ",base6$Data[length(base6$Data)],".csv"))))

# RESUMO JUROS E SPREAD
datai="01/03/2011"
dataf=daTa4
serie="20786"
Liv=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
serie="20825"
Dir=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
Liv[,2]=as.numeric(gsub(",","\\.",Liv[,2]))
Dir[,2]=as.numeric(gsub(",","\\.",Dir[,2]))

`Spread p.p*`=c(
base7$`20783 - Spread médio das operações de crédito - Total - p.p.`[length(base7$`20783 - Spread médio das operações de crédito - Total - p.p.`)],
base7$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`[length(base7$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`)],
base7$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`[length(base7$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`)],
Liv$valor[length(Liv$valor)],Dir$valor[length(Dir$valor)]
)

`Spread Variação 12 Meses (P.P.)`=c(
  `Spread p.p*`[1]-base7$`20783 - Spread médio das operações de crédito - Total - p.p.`[length(base7$`20783 - Spread médio das operações de crédito - Total - p.p.`)-12],
  `Spread p.p*`[2]-base7$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`[length(base7$`20785 - Spread médio das operações de crédito - Pessoas físicas - Total - p.p.`)-12],
  `Spread p.p*`[3]-base7$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`[length(base7$`20784 - Spread médio das operações de crédito - Pessoas jurídicas - Total - p.p.`)-12],
  `Spread p.p*`[4]-Liv$valor[length(Liv$valor)-12],
  `Spread p.p*`[5]-Dir$valor[length(Dir$valor)-12]
)

selic=base7$`432 - Meta Selic`[length(base7$`432 - Meta Selic`)]

`Resumo juros e spread`=cbind(`Tx de juros %a.a`,`Tx de juros Variação 12 Meses (p.p.)`,`Spread p.p*`,`Spread Variação 12 Meses (P.P.)`)
`Resumo juros e spread`=rbind(`Resumo juros e spread`,c(selic,selic-base7$`432 - Meta Selic`[length(base7$`432 - Meta Selic`)-12],NA,NA))
row.names(`Resumo juros e spread`)=c("Total","Pessoa Física","Pessoa Jurídica","Crédito Livre","Crédito Direcionado","Selic")
`Resumo juros e spread`

write.csv2(`Resumo juros e spread`,paste0(dir_plan,"14-Resumo juros e spread.csv"))

# DADOS INADIMPL PF REC LIVRES
datai="01/06/2000"
dataf="31/12/2012"
serie="7938"
v1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
v1[,2]=as.numeric(gsub(",","\\.",v1[,2]))

datai="01/03/2011"
dataf=daTa4
serie="21112"
v2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie,"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
v2[,2]=as.numeric(gsub(",","\\.",v2[,2]))

v3=c(rep(NA,which(v1$data=="01/02/2011")),v2[,2])
base10=data.frame(unique(c(as.character(v1[,1]),as.character(v2[,1]))),c(v1[,2],rep(NA,length(v3)-length(v1[,2]))),v3)
base10[,4]=base10[,2]/base10[,3]
base10[,5]=base10[,2]-base10[,3]
  add1=rep(NA,12)
  for(i in 13:dim(base10)[1]) add1[i]=base10[i,3]-base10[i-12,3]
  base10[,6]=add1

names(base10)=c("Data","7938 - Operações de crédito com recursos livres referenciais para taxa de juros - Inadimplência acima de 90 dias em relação ao total da modalidade - Total pessoa física - %   (Série antiga)",
  "21112 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Total - %  (Série nova)","v3[,2]/v3[,3]","v3[,2]-v3[,3]","v3[i,3]-v3[i-12,3]")
base10=cbind(Data=substr(base10[,1],4,10),base10[,-1])

write.csv2(base10,paste0(dir_plan,"15-dados Inadimp PF rec livres.csv"))


# GRÁF INAD PF LIV LONGO
png(paste0(dir_plot,"19-Gráf Inad PF liv longo.png"),width = 800, height = 700)
plot(base10[,2],type="l",ylab="",xlab="",ylim=c(4,10),axes=F, lwd=3)
lines(base10[,3],type="l",ylab="",xlab="",col="blue3", lwd=3)
ind=c(T,rep(F,length(base10$Data)%/%29))
axis(1,at=which(c(rep(ind,length.out=(length(base10$Data)-1)),T)==T),labels=substr(base10$Data[c(rep(ind,length.out=(length(base10$Data)-1)),T)],4,10),las=2)
axis(2,seq(4,10,.5),las=1)
legend("topleft",legend = c("Série antiga","Série nova"),bty="n",cex=0.9,lty=1,col=c("black","blue3"),lwd=3)
legend(x=dim(base10)[1]-15,y=base10[dim(base10)[1],3],paste(base10[dim(base10)[1],1],base10[dim(base10)[1],3],sep="; "),bty="n",cex=0.8)
legend(x=dim(base10)[1]-30,y=base10[base10$Data=="03/2015",3],substr(paste(base10[base10$Data=="03/2015",1],base10[base10$Data=="03/2015",3],sep="; "),4,16),bty="n",cex=0.8)
legend(x=dim(base10)[1]-30,y=base10[base10$Data=="05/2016",3],substr(paste(base10[base10$Data=="05/2016",1],base10[base10$Data=="05/2016",3],sep="; "),4,16),bty="n",cex=0.8)
dev.off()

# DADOS PJ LIVRE LONGO
serie=c("7937","21086")
datai="01/06/2000"
dataf=daTa4

vec_ind1=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[1],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")
vec_ind2=read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[2],"/dados?formato=csv&dataInicial=",datai,"&dataFinal=",dataf,sep="")),sep=";")

base20=merge(vec_ind1,vec_ind2,by="data",all=T)
base20=base20[order(paste0(substr(base20$data,7,10),substr(base20$data,4,5))),]
base20[,-1]=apply(base20[,-1],2,function(x)as.numeric(gsub(",","\\.",x)))
rm(list=objects(pattern="vec_ind[0-9]"))

names(base20)=c("Data","7937 - Operações de crédito com recursos livres referenciais   para taxa de juros - Inadimplência acima de 90 dias em relação ao total da modalidade - Total pessoa jurídica - %",
                "21086 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total - %")
base20=cbind(Data=substr(base20[,1],4,10),base20[,-1])

write.csv2(base20,paste0(dir_plan,"16-Dados PJ LIVRE longo.csv"))

# GRÁF7  INAD PJ LIV LONGO
png(paste0(dir_plot,"20-Gráf7 inad PJ LIV longo.png"),width = 800, height = 700)
plot(base20[,2],type="l",ylim=c(0,10),ylab="",xlab="",axes=F, lwd=3,lty=3,col="forestgreen")
lines(base20[,3],type="l",ylab="",xlab="", lwd=3,col="forestgreen")
ind=c(T,rep(F,length(base20$Data)%/%30))
axis(1,at=which(c(rep(ind,length.out=(length(base20$Data)-1)),T)==T),labels=base20$Data[c(rep(ind,length.out=(length(base20$Data)-1)),T)],las=2)
axis(2,seq(0,35,2),las=1)
legend("topleft",legend = c("Série antiga","Série nova"),bty="n",cex=0.9,lty=c(3,1),col=c("forestgreen","forestgreen"),lwd=3)
legend(x=dim(base20)[1]-20,y=base20[dim(base20)[1]-1,3],paste(base20[dim(base20)[1]-1,1],base20[dim(base20)[1]-1,3],sep="; "),bty="n",cex=0.8)
legend(x=(which(base20$Data=="12/2013")-5),y=base20[which(base20$Data=="12/2013"),3],paste("12/2013",base20[which(base20$Data=="12/2013"),3],sep="; "),bty="n",cex=0.8)
dev.off()


write.xlsx(base ,file ="crédito.xlsx" ,sheetName ="Saldo %PIB" ,append =F )
write.xlsx(base1 ,file ="crédito.xlsx" ,sheetName ="Saldo Tipo Recurso" ,append =T )
write.xlsx(base2 ,file ="crédito.xlsx" ,sheetName ="Saldo tipo capital" ,append =T )
write.xlsx(base3 ,file ="crédito.xlsx" ,sheetName ="Concessões" ,append =T )
write.xlsx(base4 ,file ="crédito.xlsx" ,sheetName ="Endividamento" ,append =T )
write.xlsx(base5 ,file ="crédito.xlsx" ,sheetName ="Comprometimento Renda" ,append =T )
write.xlsx(base6 ,file ="crédito.xlsx" ,sheetName ="Inadimplência" ,append =T )
write.xlsx(base7 ,file ="crédito.xlsx" ,sheetName ="Dados Spread" ,append =T )
write.xlsx(base8 ,file ="crédito.xlsx" ,sheetName ="Juros" ,append =T )
write.xlsx(base9 ,file ="crédito.xlsx" ,sheetName ="Saldo total série encadead" ,append =T )
write.xlsx(tabela1 ,file ="crédito.xlsx" ,sheetName ="tabela1" ,append =T )
write.xlsx(`Concessões (Média diária)` ,file ="crédito.xlsx" ,sheetName ="Concessões (Média diária)" ,append =T )
write.xlsx(mget(paste("Crédito - resumo",base6$Data[length(base6$Data)])) ,file ="crédito.xlsx" ,sheetName =gsub("/","-",paste0("Crédito resumo ",base6$Data[length(base6$Data)],".csv")) ,append =T )
write.xlsx(`Resumo juros e spread` ,file ="crédito.xlsx" ,sheetName ="Resumo juros e spread" ,append =T )
write.xlsx(base10 ,file ="crédito.xlsx" ,sheetName ="dados Inadimp PF rec livres" ,append =T )
write.xlsx(base20 ,file ="crédito.xlsx" ,sheetName ="Dados PJ LIVRE longo" ,append =T )








