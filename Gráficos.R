# Rotina para apresentar em gráficos algumas séries do banco central
# Feito por: Felipe Simplício Ferreira
# última atualização: 27/11/2020

# Carregando pacotes que serão utilizados
library(ggplot2)
source("Crédito.R")

# Montagem dos gráficos

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