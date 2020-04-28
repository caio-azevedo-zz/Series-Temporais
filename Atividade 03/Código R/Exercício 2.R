# Limpar

rm(list=ls())

#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Series-Temporais/Atividade 03/Figuras")

# Carregando pacotes a serem utilizados

library(Quandl)
library(dplyr)
library(lattice)
library(ggplot2)
library(lubridate)
library(xtable)

# Exportando os dados a partir do Quandl com uma chave de acesso

Quandl.api_key("1eFqgyJAPx3EFQL-7_xQ")

bcb_1211<-Quandl("BCB/1211")
bcb_1253<-Quandl("BCB/1253")
bcb_7325<-Quandl("BCB/7325")
bcb_13522<-Quandl("BCB/13522")
bcb_24369<-Quandl("BCB/24369")

#Exercício 2

# letra a

# BCB/1253

bcb_1253<-bcb_1253 %>% 
  arrange(Date) %>% 
  select(Value)

bcb_1253<-ts(bcb_1253, frequency=4, start = c(1996,1))

plot(bcb_1253, col="blue", xlab="Ano", ylab="Valor")

dev.copy(pdf,"bcb_1253.pdf")
dev.off()

# BCB/7325

bcb_7325<-bcb_7325 %>% 
  arrange(Date) %>% 
  select(Value)

bcb_7325<-ts(bcb_7325, frequency=1, start = c(1962))

plot(bcb_7325, col="blue", xlab="Ano", ylab="Valor")

dev.copy(pdf,"bcb_7325.pdf")
dev.off()

# BCB/13522

bcb_13522<-bcb_13522 %>% 
  arrange(Date) %>% 
  select(Value)

bcb_13522<-ts(bcb_13522, frequency=12, start = c(1980,12))

plot(bcb_13522, col="blue", xlab="Ano", ylab="Valor")

dev.copy(pdf,"bcb_13522.pdf")
dev.off()



# BCB/24369

bcb_24369<-bcb_24369 %>% 
  arrange(Date) %>% 
  select(Value)

bcb_24369<-ts(bcb_24369, frequency=12, start = c(2012,3))

plot(bcb_24369, col="blue", xlab="Ano", ylab="Valor")

dev.copy(pdf,"bcb_24369.pdf")
dev.off()


# BCB/1211 falta os anos 1990 e 1995
# Essa série possui um problema, os dados a respeito dos anos 1990 e 1995 não foram 
# computados

# Ordenando a base por data

bcb_1211<-bcb_1211 %>% 
  arrange(Date)

# Extraindo os anos da base BCB/1211

ano<-as_date(bcb_1211$Date)
ano<-as.ts(year(ano))

# Selecionando apenas os valores da base
bcb_1211<-bcb_1211 %>% 
  select(Value)

# Transformando em série temporal, porém se descrionar o ano devido o problema já relacionado
bcb_1211<-ts(bcb_1211, frequency=1, start = 1)


# Criando um data frame com todos anos, incluindo 1990 e 1995
df<-data.frame(c(1963:2018))
colnames(df)<-c("ano")


# Juntando as séries BCB/1211 - Valor com os seus respectivos anos
bcb<-as.data.frame(cbind(ano,bcb_1211))


df<-left_join(df,bcb,c("ano"))
plot(df, type="l", col="blue",xlab="Ano", ylab="Valor")

dev.copy(pdf,"bcb_1211.pdf")
dev.off()


# Estatísticas Descritivas



# Gráficos sobrepostos

b_13522<- window(bcb_13522, start=2013, end=2019)
b_24369<-window(bcb_24369, start=2013, end=2019)

ex<-cbind(b_13522, b_24369)
colnames(ex)<-c("BCB_13522","BCB_24369")

xyplot(ex, superpose = TRUE, lwd=2, xlab = "Ano") 

dev.copy(pdf,"sob.pdf")
dev.off()


# Gráfico dispersão

ex1<-as.data.frame(ex)
attach(ex1)

#Configurando o layout do gráfico

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_line(color = "white"),
                panel.background = element_blank(),
                axis.line = element_line(color = "black"),
                axis.title = element_text(size = 14),
                legend.text = element_text(size = 10),
                 axis.text = element_text(size = 14))

qplot(BCB_13522, BCB_24369, data = ex1) + cleanup 

dev.copy(pdf,"disp.pdf")
dev.off()


# Estatísticas Descritivas

sum_1211<-apply(bcb_1211,2,summary)
sum_1253<-apply(bcb_1253,2,summary)
sum_7325<-apply(bcb_7325,2,summary)
sum_13522<-apply(bcb_13522,2,summary)
sum_24369<-apply(bcb_24369,2,summary)

sumario<-cbind(sum_1211, sum_1253, sum_7325, sum_13522, sum_24369)
colnames(sumario)<-c("BCB/1211", "BCB/1253", "BCB/7325", "BCB/13522", "BCB/24369")

#Removendo dados criados

rm(sum_1211, sum_1253, sum_13522, sum_24369, sum_7325)

# Exportando para Latex
print(xtable(sumario, caption = "Sumário das Estatísticas Descritivas", 
             label = "tab3", digits = 2),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))
