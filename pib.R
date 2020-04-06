# Limpar

rm(list=ls())

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais")

# Carregando pacotes a serem utilizados

library(forecast) 
library(dplyr)
library(lattice)

# Exportando os dados disponíveis no GitHub

site <- "https://raw.githubusercontent.com/caio-azevedo/Series-Temporais/master/data/PIB.csv"

dados<- read.table(site, header=T, sep=";", dec = ",", 
                   col.names = c("ano", "mes", "pib"))


# Configurando os dados para o formato de séries temporais
attach(dados)

pib90 <-ts(pib, start=c(1990,1), frequency = 12)


# Fazendo um recorte de tempo nos dados a partir de 2000

pib2010<- window(pib90, start=2010)

# Plotando o gráfico da série
plot(pib2010)


# Aplicando a Suavização de Holt

pib_holt<-holt(pib2010, level = 95, h=10)


plot(pib_holt)


# Gerar uma nova série através da Suavização Exponencial Simples (SES)
pib_prev<- pib_holt[["model"]][["fitted"]]
pib_prev<-data.frame(pib_prev)


#Retirando os valores previstos da SES para fora da amostra

x<-data.frame(pib_holt, row.names = c(1:10))
x<-x %>% 
  select("Point.Forecast") %>% 
  rename("pib_prev"="Point.Forecast")

pib_prev<-bind_rows(pib_prev, x)



# Juntar as duas séries - original e suavizada - em uma matriz para formar um gráfico único
pib2010<-data.frame(pib2010)

# é necessário criar um vetor nulo pra completar a série original
# a repetição é de acordo com o número de períodos a ser previsto
nulo<-c(rep(NA,10)) 
nulo<-data.frame(nulo)

nulo<-nulo %>% 
  rename("pib2010"="nulo")

pib2010<-bind_rows(pib2010, nulo)

graf<-cbind(pib2010, pib_prev)
colnames(graf)<-c("Série Original", "SEH")


# Retornando para o formato temporal

graf_ts<-ts(graf, start=c(2010,1), frequency = 12)


# Plotar um único gráfico
xyplot(graf_ts, superpose = TRUE, lwd = 2) 

dev.copy(pdf,"pib.pdf")
dev.off()