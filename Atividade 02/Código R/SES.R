# Limpar----

rm(list=ls())

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais/Figuras")

# Carregando pacotes a serem utilizados

library(forecast) 
library(dplyr)
library(lattice)
library(xtable)
library(stargazer)

# Exportando os dados disponíveis no GitHub

site <- "https://raw.githubusercontent.com/caio-azevedo/Series-Temporais/master/data/energia.csv"

dados<- read.table(site, header=T, sep=";", dec = ",", 
                   col.names = c("data", "cons", ""))


# Configurando os dados para o formato de séries temporais
attach(dados)

cons76 <-ts(cons, start=c(1976,1), frequency = 12)

plot(cons76)

dev.copy(pdf,"cons76.pdf")
dev.off()

# Fazendo um recorte de tempo nos dados a partir de 2012

cons2012<- window(cons76, start=2012)

# Plotando o gráfico da série
plot(cons2012)

dev.copy(pdf,"cons2012.pdf")
dev.off()


# Aplicando a Suavização de Holt - Winters erros aditivos----

cons_ses<-ses(cons2012, level = 95, h=10)


plot(cons_ses)


# Gerar uma nova série através da Suavização Exponencial Simples (SES)
cons_prev<- cons_ses[["model"]][["fitted"]]
cons_prev<-data.frame(cons_prev)


#Retirando os valores previstos da SES para fora da amostra----

x<-data.frame(cons_ses, row.names = c(1:10))
x<-x %>% 
  select("Point.Forecast") %>% 
  rename("cons_prev"="Point.Forecast")

cons_prev<-bind_rows(cons_prev, x)



# Juntar as duas séries - original e suavizada - em uma matriz para formar um gráfico único
cons2012<-data.frame(cons2012)

# é necessário criar um vetor nulo pra completar a série original
# a repetição é de acordo com o número de períodos a ser previsto
nulo<-c(rep(NA,10)) 
nulo<-data.frame(nulo)

nulo<-nulo %>% 
  rename("cons2012"="nulo")

cons2012<-bind_rows(cons2012, nulo)

graf<-cbind(cons2012, cons_prev)
colnames(graf)<-c("Série Original", "SES")


# Retornando para o formato temporal----

graf_ts<-ts(graf, start=c(2012,1), frequency = 12)


# Plotar um único gráfico----
xyplot(graf_ts, superpose = TRUE, lwd = 2) 

dev.copy(pdf,"cons.pdf")
dev.off()


#Parâmetro estimado e estado inicial

parametros<-data.frame(round(cons_ses[["model"]][["par"]], digits = 4))
colnames(parametros)<-c("Parâmetros estimados")
rownames(parametros)<-c("alpha", "L")

#Critérios de informação

loglik <- cons_ses[["model"]][["loglik"]]
aic <- cons_ses[["model"]][["aic"]]
bic <- cons_ses[["model"]][["bic"]]
aicc <- cons_ses[["model"]][["aicc"]]
mse <- cons_ses[["model"]][["mse"]]
amse <- cons_ses[["model"]][["amse"]]

inf_crit<-data.frame(c(loglik, aic, bic, aicc, mse, amse))
colnames(inf_crit)<-c("Critérios de Informação")
rownames(inf_crit)<-c("LogLik", "AIC", "BIC", "AICC", "MSE", "AMSE")


print(xtable(inf_crit, caption = "Critérios de Informação da SES", 
             label = "tab1.2"),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))

ses<-data.frame(cons_ses)
print(xtable(ses, caption = "Previsão para o consumo de energia no Brasil (em Gwh)
             através da SES",
             label = "tab1.1", digits = 0),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))