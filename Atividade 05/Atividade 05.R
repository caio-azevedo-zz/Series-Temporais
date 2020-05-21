# Limpar----

rm(list=ls())
graphics.off()

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais/Atividade 05/Figuras")

# Carregando pacotes a serem utilizados

library(forecast) 
library(dplyr)
library(uroot)
library(urca)
library(lmtest)
library(normtest)
library(FinTS)
library(stargazer)

# Exportando os dados disponíveis no GitHub

site <- "https://github.com/caio-azevedo/Series-Temporais/raw/master/Atividade%2002/data/cars.csv"
dados<- read.table(site, header=T, sep=";", dec = ",", 
                   col.names = c("data", "cars"))


# Configurando os dados para o formato de séries temporais


cars57 <-ts(dados$cars, start=c(1957,1), frequency = 12)

# Fazendo um recorte de tempo nos dados a partir de 2000

cars<- window(cars57, start=2000, frequency=12)

# Plotando o gráfico da série

par(mfrow=c(2,1))
plot(cars57, xlab="Ano", ylab="")
plot(cars, xlab="Ano", ylab="")

dev.copy(pdf,"serie.pdf")
dev.off()

plot(decompose(cars),xlab="Ano")
dev.copy(pdf,"decompose.pdf")
dev.off()

#Detectando a presença de outliers
outliers<-tsoutliers(cars)

outliers

#Substituindo os outliers
cars[outliers$index[1]]<-cars[outliers$index[1]-12]
cars[outliers$index[2]]<-cars[outliers$index[2]-12]
cars[107]<-cars[107-12]



# Verificando sazonalidade da série
S<-ordered(cycle(cars))

cars.reg<-lm(cars~S)

summary(cars.reg)

stargazer(cars.reg, decimal.mark = ",", digit.separator = ".")


# Criando as séries de cada mês
mes<-as.data.frame(cars)
Jan<-ts(mes[seq(1,length(cars),12),],start = 2000, frequency = 1)
Feb<-ts(mes[seq(2,length(cars),12),],start = 2000, frequency = 1)
Mar<-ts(mes[seq(3,length(cars),12),],start = 2000, frequency = 1)
Apr<-ts(mes[seq(4,length(cars),12),],start = 2000, frequency = 1)
May<-ts(mes[seq(5,length(cars),12),],start = 2000, frequency = 1)
Jun<-ts(mes[seq(6,length(cars),12),],start = 2000, frequency = 1)
Jul<-ts(mes[seq(7,length(cars),12),],start = 2000, frequency = 1)
Aug<-ts(mes[seq(8,length(cars),12),],start = 2000, frequency = 1)
Sep<-ts(mes[seq(9,length(cars),12),],start = 2000, frequency = 1)
Oct<-ts(mes[seq(10,length(cars),12),],start = 2000, frequency = 1)
Nov<-ts(mes[seq(11,length(cars),12),],start = 2000, frequency = 1)
Dec<-ts(mes[seq(12,length(cars),12),],start = 2000, frequency = 1)


# Gráficos das séries por mês
par(mfrow=c(3,2))
plot(Jan)
plot(Feb)
plot(Mar)
plot(Apr)
plot(May)
plot(Jun)

dev.copy(pdf,"mes1.pdf")
dev.off()

par(mfrow=c(3,2))
plot(Jul)
plot(Aug)
plot(Sep)
plot(Oct)
plot(Nov)
plot(Dec)

dev.copy(pdf,"mes2.pdf")
dev.off()

#Correlograma das séries por mês

par(mfrow=c(3,2))
Acf(Jan)
Acf(Feb)
Acf(Mar)
Acf(Apr)
Acf(May)
Acf(Jun)

dev.copy(pdf,"acf_mes1.pdf")
dev.off()

par(mfrow=c(3,2))
Acf(Jul)
Acf(Aug)
Acf(Sep)
Acf(Oct)
Acf(Nov)
Acf(Dec)

dev.copy(pdf,"acf_mes2.pdf")
dev.off()

#Correlogramas da série

par(mfrow=c(2,1))
Acf(cars, main="")
Pacf(cars, main="")

dev.copy(pdf,"correlograma.pdf")
dev.off()

# Teste de raiz unitária sazonal - Procedimento de HEGY
teste_hegy<-hegy.test(cars, deterministic = c(1,1,1), lag.method = "fixed", maxlag = 1)

teste_hegy

#nsdiffs(cars,test = c("hegy"))


# Teste de Canova e Hansen


ch.test(cars, type=c("dummy"))

#nsdiffs(cars,test=c('ch'))

# Teste de raiz unitária (tradicionais)

# Teste ADF

summary(ur.df(cars, type = "trend", lags=14))
summary(ur.df(cars, type = "drift", lags=14))


# Teste de Philipps Perron

summary(ur.pp(cars, type = "Z-tau", model = "constant", lags="long"))



# Teste KPSS

summary(ur.kpss(cars, type = "mu", lags = "long"))

# Teste DF-GLS (ERS)

summary(ur.ers(cars,type="DF-GLS", model = "constant", lag.max = 14))

#ndiffs(cars)

# Criando as séries de cada mês da série diferenciada
mes<-as.data.frame(diff(cars))
Jan<-ts(mes[seq(1,length(cars)-1,12),],start = 2000, frequency = 1)
Feb<-ts(mes[seq(2,length(cars)-1,12),],start = 2000, frequency = 1)
Mar<-ts(mes[seq(3,length(cars)-1,12),],start = 2000, frequency = 1)
Apr<-ts(mes[seq(4,length(cars)-1,12),],start = 2000, frequency = 1)
May<-ts(mes[seq(5,length(cars)-1,12),],start = 2000, frequency = 1)
Jun<-ts(mes[seq(6,length(cars)-1,12),],start = 2000, frequency = 1)
Jul<-ts(mes[seq(7,length(cars)-1,12),],start = 2000, frequency = 1)
Aug<-ts(mes[seq(8,length(cars)-1,12),],start = 2000, frequency = 1)
Sep<-ts(mes[seq(9,length(cars)-1,12),],start = 2000, frequency = 1)
Oct<-ts(mes[seq(10,length(cars)-1,12),],start = 2000, frequency = 1)
Nov<-ts(mes[seq(11,length(cars)-1,12),],start = 2000, frequency = 1)
Dec<-ts(mes[seq(12,length(cars)-1,12),],start = 2000, frequency = 1)


# Gráficos das séries diferenciada por mês
par(mfrow=c(3,2))
plot(Jan)
plot(Feb)
plot(Mar)
plot(Apr)
plot(May)
plot(Jun)

dev.copy(pdf,"dmes1.pdf")
dev.off()

par(mfrow=c(3,2))
plot(Jul)
plot(Aug)
plot(Sep)
plot(Oct)
plot(Nov)
plot(Dec)

dev.copy(pdf,"dmes2.pdf")
dev.off()

#Correlograma das séries por mês

par(mfrow=c(3,2))
Acf(Jan)
Acf(Feb)
Acf(Mar)
Acf(Apr)
Acf(May)
Acf(Jun)

dev.copy(pdf,"acf_dmes1.pdf")
dev.off()

par(mfrow=c(3,2))
Acf(Jul)
Acf(Aug)
Acf(Sep)
Acf(Oct)
Acf(Nov)
Acf(Dec)

dev.copy(pdf,"acf_dmes2.pdf")
dev.off()


# Teste de Canova e Hansen para a série diferenciada

ch.test(diff(cars), type=c("dummy"))

#nsdiffs(cars,test=c('ch'))


#Diferencial sazonal

log(cars) %>% diff() %>% ggtsdisplay() 

dev.copy(pdf,"log_serie.pdf")
dev.off()

fit<-Arima(log(cars), order=c(0,1,4), seasonal=c(2,0,2),
           fixed=c(NA,0,0,NA,NA,NA,0,NA))


summary(fit)
coeftest(fit)

#Diagnóstico dos resíduos

checkresiduals(fit)

dev.copy(pdf,"residuos.pdf")
dev.off()

tsdiag(fit)

dev.copy(pdf,"diagnostico.pdf")
dev.off()


# Teste de auto-correlação

Box.test(fit$residuals, lag=16,type="Ljung-Box", fitdf = 1)


# Teste de heterocedasticidade condicional

ArchTest(fit$residuals)


# Teste de normalidade

jb.norm.test(fit$residuals)

shapiro.test(fit$residuals)


# Densidade de Kernel

graphics.off()

plot(density(fit$residuals, kernel = c("gaussian")),
     main="Modelo SARIMA(0,1,4)(2,0,2)[12]", xlab="", ylab="")

dev.copy(pdf,"densidade.pdf")
dev.off()

#Previsão

fit %>% forecast(h=10) %>% autoplot()

dev.copy(pdf,"forecast.pdf")
dev.off()

#Medidas de acurácia

accuracy(fit)


par(mfrow=c(1,1))
plot(log(cars),
     main='',
     xlab='Ano', ylab='',
     col='blue',
     bty='l')
par(new=TRUE)
plot(fit$fitted,
     axes=F, ann=F,
     col='red',
     bty='l')

legend('topleft',
       c('log(cars)', 'Fitted log(cars)'),
       col=c('blue', 'red'), lty=1:1,
       bty='n')
grid(col='darkgrey')

dev.copy(pdf,"previsoes.pdf")
dev.off()


# Holt-Winters

cars_hw<-hw(log(cars), level = 95, h=10, seasonal = "additive", initial = "optimal")
accuracy(cars_hw)

cars_hw %>% forecast(h=10) %>% autoplot()

dev.copy(pdf,"forecast_hw.pdf")
dev.off()

par(mfrow=c(1,1))
plot(log(cars),
     main='',
     xlab='Ano', ylab='',
     col='blue',
     bty='l')
par(new=TRUE)
plot(cars_hw$fitted,
     axes=F, ann=F,
     col='red',
     bty='l')

legend('topleft',
       c('log(cars)', 'Fitted log(cars)'),
       col=c('blue', 'red'), lty=1:1,
       bty='n')
grid(col='darkgrey')

dev.copy(pdf,"previsoes_hw.pdf")
dev.off()

# Auto arima

fit<-Arima(log(cars), order=c(2,1,0), seasonal=c(2,0,0))




