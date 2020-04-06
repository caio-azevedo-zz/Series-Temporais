# Limpar

rm(list=ls())

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais")

# Carregando pacotes a serem utilizados

library(forecast) 
library(dplyr)
library(lattice)

# Exportando os dados disponíveis no GitHub

site <- "https://raw.githubusercontent.com/caio-azevedo/Series-Temporais/master/data/Taxa%20de%20juros.csv"

dados<- read.table(site, header=T, sep=";", dec = ",", 
                   col.names = c("data", "juros", ""))


# Configurando os dados para o formato de séries temporais
attach(dados)

juros74 <-ts(juros, start=c(1974,1), frequency = 12)


# Fazendo um recorte de tempo nos dados a partir de 2010

juros2010<- window(juros74, start=2010)

# Plotando o gráfico da série
plot(juros2010)


# Aplicando a suavização exponencial simples

juros_ses<-ses(juros2010, level = 95, h=10)


plot(juros_ses)


# Gerar uma nova série através da Suavização Exponencial Simples (SES)
juros_prev<- juros_ses[["model"]][["fitted"]]
juros_prev<-data.frame(juros_prev)


#Retirando os valores previstos da SES para fora da amostra

x<-data.frame(juros_ses, row.names = c(1:10))
x<-x %>% 
  select("Point.Forecast") %>% 
  rename("juros_prev"="Point.Forecast")

juros_prev<-bind_rows(juros_prev, x)



# Juntar as duas séries - original e suavizada - em uma matriz para formar um gráfico único
juros2010<-data.frame(juros2010)

# é necessário criar um vetor nulo pra completar a série original
# a repetição é de acordo com o número de períodos a ser previsto
nulo<-c(rep(NA,10)) 
nulo<-data.frame(nulo)

nulo<-nulo %>% 
  rename("juros2010"="nulo")

juros2010<-bind_rows(juros2010, nulo)

graf<-cbind(juros2010, juros_prev)
colnames(graf)<-c("Série Original", "SES")


# Retornando para o formato temporal

graf_ts<-ts(graf, start=c(2010,1), frequency = 12)


# Plotar um único gráfico
xyplot(graf_ts, superpose = TRUE, lwd = 2) 

dev.copy(pdf,"juros")
dev.off()









