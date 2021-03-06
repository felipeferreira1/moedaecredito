# Rotina para apresentar em gr�ficos algumas s�ries do banco central
# Feito por: Felipe Simpl�cio Ferreira
# �ltima atualiza��o: 27/11/2020

# Carregando pacotes que ser�o utilizados
library(ggplot2)
source("Cr�dito.R")

# Montagem dos gr�ficos

# GR�F % PIB
ggplot(base1, aes(base1$Data, base1$`Saldo da carteira de cr�dito em rela��o ao PIB - % - 20622`)) +
  geom_line() + xlab("Data") + ylab("Em % do PIB") + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo da carteira de cr�dito em rela��o ao PIB - %")
ggsave("01-Gr�f PIB.png")

# GR�F LIV E DIR
ggplot(base2) + geom_line(aes(base2$Data, base2$`Recursos Livres Deflacionado-Total`), colour = "black") +
  geom_line(aes(base2$Data, base2$`Recursos Direcionados Deflacionado-Total`), colour = "gold") +
  geom_bar(aes(base2$Data, base2$`Total Deflacionado-Total Geral`), stat = "identity",colour ="blue") + 
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo da carteira de cr�dito", "Deflacionado pelo IPCA, var% t/t-12")
ggsave("02-Gr�f Liv e dir.png")

# GR�F PF E PJ
ggplot(base2) + geom_line(aes(base2$Data, base2$`Total Deflacionado-PJ`), colour = "black") +
  geom_line(aes(base2$Data, base2$`Total Deflacionado-PF`), colour = "gold") +
  geom_line(aes(base2$Data, base2$`Total Deflacionado-Total Geral`), stat = "identity",colour ="blue") + 
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo das opera��es de cr�dito do SFN", "Deflacionado pelo IPCA, var% t/t-12")
ggsave("03-Gr�f PF e PJ.png")

# GR�F TIPO DE CAP
ggplot(base3) + geom_line(aes(base3$Data, base3$P�blico), colour = "gold") +
  geom_line(aes(base3$Data, base3$Privado), colour = "blue") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo de cr�dito por tipo de controle de capital", "R$ milh�es do �ltimo m�s")
ggsave("04-Gr�f tipo de cap.png")

# GR�F TIPO DE CAP %
ggplot(base3) + geom_line(aes(base3$Data, base3$`P�blico (% Total de Cr�dito)`), colour = "gold") +
  geom_line(aes(base3$Data, base3$`Privado (% Total de Cr�dito)`), colour = "blue") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo por tipo de capital", "Participa��o %")
ggsave("05-Gr�f tipo de cap perc.png")

# GR�F TIPO DE CONTROLE
ggplot(base3) + geom_line(aes(base3$Data, base3$`P�blico YoY`), colour = "gold") +
  geom_line(aes(base3$Data, base3$`Privado YoY`), colour = "blue") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Saldo de cr�dito por tipo de controle de capital", "Varia��o real em 12 meses (em %)")
ggsave("06-Gr�f tipo de cap controle var real.png")

# GR�F CONCESS PJ PF
ggplot(base4) + geom_bar(aes(base4$Data, base4$`Total Deflacionado Di�rio - Total Geral`), stat = "identity", colour = "gold") +
  geom_bar(aes(base4$Data, base4$`Total Deflacionado Di�rio - Pessoa Jur�dica`), stat = "identity", colour = "blue") +
  geom_bar(aes(base4$Data, base4$`Total Deflacionado Di�rio - Pessoa F�sica`), stat = "identity", colour = "black") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Varia��o % sobre o mesmo m�s do ano anterior de novas concess�es", "M�dia por dia �til, valores deflacionados di�rios")
ggsave("07-Gr�f concess PJ PF.png")

# GR�F CONCESS LIV E DIR
ggplot(base4) + geom_bar(aes(base4$Data, base4$`Recursos Livres Deflacionado Di�rio - Recursos Livres-Total`), stat = "identity", colour = "black") +
  geom_bar(aes(base4$Data, base4$`Recursos Direcionados Deflacionado Di�rio - Recursos Direcionados-Total`), stat = "identity", colour = "blue") +
  geom_bar(aes(base4$Data, base4$`Total Deflacionado Di�rio - Total Geral`), stat = "identity", colour = "gold") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Novas concess�es de cr�dito do SFN", "M�dia por dia �til, deflacionada pelo IPCA, var t/t-12")
ggsave("08-Gr�f concess liv e dir.png")

# GR�F ENDIVIDAMENTO
ggplot(base5) + geom_line(aes(base5$Data, base5$`20622 - Saldo da carteira de cr�dito em rela��o ao PIB - %`), colour = "gold") +
  geom_line(aes(base5$Data, base5$`20400 - Endividamento das fam�lias com o Sistema Financeiro Nacional exceto cr�dito habitacional em rela��o � renda acumulada dos �ltimos doze meses - %`), colour = "blue") +
  geom_line(aes(base5$Data, base5$`19882 - Endividamento das fam�lias com o Sistema Financeiro Nacional em rela��o � renda acumulada dos �timos doze meses - %`), colour = "black") +
  geom_line(aes(base5$Data, base5$`Cr�dito Habitacional`), colour = "grey") +
  xlab("Data") + ylab(NULL) + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Endividamento familiar - %")
ggsave("09-Gr�f endividamento.png")

# GR�F COMPROMET
ggplot(base6, aes(base6$Data, base6$`19881 - Comprometimento de renda das fam�lias com o servi�o da d�vida com o Sistema Financeiro Nacional - Com ajuste sazonal - %`)) +
  geom_line() + xlab("Data") + ylab("Em % da renda") + scale_x_date(date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle=90)) + ggtitle("Comprometimento da renda familiar com o servi�o da d�vida", "Dessazonalizado - %")
ggsave("10-Gr�f compromet.png")

# GR�F INADIMPL�NCIA RECURSOS
# GRAF INADIMPL�NCIA PESSOAS