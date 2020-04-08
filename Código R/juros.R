# Limpar

rm(list=ls())

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais/Figuras")

# Carregando pacotes a serem utilizados

library(forecast) 
library(dplyr)
library(lattice)

# Exportando os dados disponíveis no GitHub

site <- "https://raw.githubusercontent.com/caio-azevedo/Series-Temporais/master/data/passageiros_embarcados.csv"

dados<- read.table(site, header=T, sep=";", dec = ",", 
                   col.names = c("data", "pas", ""))


# Configurando os dados para o formato de séries temporais
attach(dados)

pas69 <-ts(pas, start=c(1969), frequency = 1)

plot(pas69)

dev.copy(pdf,"pas69.pdf")
dev.off()



# Aplicando a suavização exponencial de Holt

pas_holt<-holt(pas69, level = 95, h=10)


plot(pas_holt)


# Gerar uma nova série através da Suavização Exponencial de Holt (SEH)
pas_prev<- pas_holt[["model"]][["fitted"]]
pas_prev<-data.frame(pas_prev)


#Retirando os valores previstos da SEH para fora da amostra

x<-data.frame(pas_holt, row.names = c(1:10))
x<-x %>% 
  select("Point.Forecast") %>% 
  rename("pas_prev"="Point.Forecast")

pas_prev<-bind_rows(pas_prev, x)



# Juntar as duas séries - original e suavizada - em uma matriz para formar um gráfico único
pas69<-data.frame(pas69)

# é necessário criar um vetor nulo pra completar a série original
# a repetição é de acordo com o número de períodos a ser previsto
nulo<-c(rep(NA,10)) 
nulo<-data.frame(nulo)

nulo<-nulo %>% 
  rename("pas69"="nulo")

pas69<-bind_rows(pas69, nulo)

graf<-cbind(pas69, pas_prev)
colnames(graf)<-c("Série Original", "Holt")


# Retornando para o formato temporal

graf_ts<-ts(graf, start=c(1969), frequency = 1)


# Plotar um único gráfico
xyplot(graf_ts, superpose = TRUE, lwd = 2) 

dev.copy(pdf,"pas.pdf")
dev.off()


#Encontrar os parâmetros estimados

parametros<-data.frame(round(pas_holt[["model"]][["par"]], digits = 4))
parametros<-data.frame(parametros[1:2,])
colnames(parametros)<-c("Parâmetros estimados")
rownames(parametros)<-c("alpha", "beta")

# Estados Iniciais

estado_inic<-data.frame(round(pas_holt[["model"]][["initstate"]], digits = 2))
colnames(estado_inic)<-c("Estados Iniciais")

#Critérios de informação

loglik <- pas_holt[["model"]][["loglik"]]
aic <- pas_holt[["model"]][["aic"]]
bic <- pas_holt [["model"]][["bic"]]
aicc <- pas_holt[["model"]][["aicc"]]
mse <- pas_holt[["model"]][["mse"]]
amse <- pas_holt[["model"]][["amse"]]

inf_crit<-data.frame(c(loglik, aic, bic, aicc, mse, amse))
colnames(inf_crit)<-c("Critérios de Informação")
rownames(inf_crit)<-c("LogLik", "AIC", "BIC", "AICC", "MSE", "AMSE")

print(xtable(estado_inic, caption = "Estados Iniciais utilizados na Previsão da SEH", 
             label = "tab2.2", digits = 0),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))

print(xtable(inf_crit, caption = "Critérios de Informação da SEH", 
             label = "tab2.3"),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))

holt<-data.frame(pas_holt)
print(xtable(holt, caption = "Previsão para a quantidade de passageiros embarcados
             através do Modelo de Holt", 
             label = "tab2.4"),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))







