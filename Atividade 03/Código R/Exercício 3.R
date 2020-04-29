# Limpar

rm(list=ls())

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais/Atividade 03/Figuras")

# Carregando pacotes a serem utilizados

library(forecast) 
library(dplyr)
library(urca)
library(xtable)
library(stargazer)
library(FinTS)
library(normtest)


# Exportando os dados disponíveis no GitHub

site <- "https://raw.githubusercontent.com/caio-azevedo/Series-Temporais/master/Atividade%2002/data/passageiros_embarcados.csv"

dados<- read.table(site, header=T, sep=";", dec = ",", 
                   col.names = c("data", "pas", ""))


# Configurando os dados para o formato de séries temporais
attach(dados)

pas69 <-ts(pas, start=c(1969), frequency = 1)


#Teste de Estacionaridade - Dicker Fuller ----

#Variável dependente é um Passeio aleatório

df1<-ur.df(pas69, type = c("none"), lags=0)
summary(df1)

#Variável dependente é um Passeio aleatório com deslocamento

df2<-ur.df(pas69, type = c("drift"), lags=0)
summary(df2)


# Variável dependente é um passeio aleatório com deslocamento em torno de uma tendência estocástica

df3<-ur.df(pas69, type = c("trend"), lags=0)
summary(df3)

# Tirando a primeira diferença da série para torná-la estacionária

dpas<-diff(pas69)


# Refazendo teste de raiz unitária (Dicker-Fuller) para a série diferenciada
# lags = 0 significa que não estamos fazendo o teste ADF

df<-ur.df(dpas, type = c("trend"), lags = 0)
summary(df)

# Essa função indica quantas diferenças são necessárias para que a série se torne estacionária
#pelo resultados anterior chegamos que somente uma diferença já bastava

ndiffs(pas69, test=c("adf"),type=c("trend"), lags=0)

# Plotando os gráficos da série original e transformada lado-a-lado

par(mfrow=c(2,1))
plot(pas69, col="blue", main="Série Original", bty="l",xlab="Ano", ylab="")
plot(dpas, col="blue", main="Série Defasada em um período", bty="l",xlab="Ano", ylab="")

dev.copy(pdf,"pas.pdf")
dev.off()

# Identificação do Modelo através dos Correlogramas
auto.arima(dpas)

par(mfrow=c(2,1))
acf(dpas, main="Função de Auto-Correlação", xlab="Defasagem", ylab="")
pacf(dpas, main="Função de Auto-Correlação Parcial", xlab="Defasagem", ylab="")

dev.copy(pdf,"cor.pdf")
dev.off()

# Escolher a ordem do modelo em c(p,d,q)
model_1<-arima(dpas, order = c(1,0,2)) 
model_2<-arima(dpas, order = c(1,0,1)) 
model_3<-arima(dpas, order = c(0,0,2)) 
model_4<-arima(dpas, order = c(0,0,1)) 
model_5<-arima(dpas, order = c(1,0,0)) 


#Critério AIC
#Pelo critério AIC model_5 foi escolhido, logo um MA(1)

aic<-AIC(model_1, model_2, model_3, model_4, model_5)


#Critério BIC
# Igualmente ao critério AIC, o BIC sugeriu um MA(1)

bic<-BIC(model_1, model_2, model_3, model_4, model_5)


#Juntando os dois critérios e exportando em Latex
inf<-cbind(aic, bic)

print(xtable(inf, caption = "Critérios de Informação AIC e BIC",
             label = "tabinf", digits = 2),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))


# Teste para os coeficientes estimados dos modelos

stargazer(model_4,model_5, decimal.mark = ",", digit.separator = ".")

stargazer(model_1,model_2,model_3, decimal.mark = ",", digit.separator = ".")


# Diagnóstico

tsdiag(model_5)

dev.copy(pdf,"diag5.pdf")
dev.off()

tsdiag(model_4)

dev.copy(pdf,"diag4.pdf")
dev.off()


# Teste de auto-correlação

Box.test(model_5$residuals, lag=16,type="Ljung-Box", fitdf = 1)
Box.test(model_4$residuals, lag=16,type="Ljung-Box", fitdf = 1)


# Teste de heterocedasticidade condicional

ArchTest(model_5$residuals, lags = 16)
ArchTest(model_4$residuals, lags = 16)


# Teste de normalidade

jb.norm.test(model_5$residuals)
jb.norm.test(model_4$residuals)


# Gráfico kernel da densidade dos erros

plot(density(model_5$residuals, kernel = c("gaussian")),
     main="Densidade", xlab="", ylab="")

dev.copy(pdf,"den5.pdf")
dev.off()

plot(density(model_4$residuals, kernel = c("gaussian")),
     main="Densidade", xlab="", ylab="")

dev.copy(pdf,"den4.pdf")
dev.off()

# Previsão
plot(forecast(model_5, h=5, level=0.95))


# Acurácia
accuracy(model_5)
accuracy(model_4)




kurtosis(model_5$residuals)
skewness(model_5$residuals)

kurtosis(model_4$residuals)
skewness(model_4$residuals)
