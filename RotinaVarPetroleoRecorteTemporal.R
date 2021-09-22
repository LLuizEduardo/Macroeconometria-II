###########################################################################################
#                   EX: MODELO VAR                                                    #
###########################################################################################

###Aplicação #1: Preço, Produção do Petróleo e Índice de Atividade Econômica Global

setwd("C:/Users/Usuário/OneDrive - alu.uern.br/UERN/Semestre 2/Macroeconomia II")

library(dplyr)
library(readxl)

data <- read_excel("data.xlsx")

data <- data[2:563,]

##Selecionando uma partição amostral
data1 <- data %>% 
  filter(Data>"1974-01-01" & Data<"1994-01-01")

data2 <- data %>% 
  filter(Data>"1989-12-01" & Data<"2009-12-01")

data3 <- data %>% 
  filter(Data>"2000-11-01" & Data<"2020-11-01")


#Utilizar função que informará ao R que os dados formam um vetor de séries temporais
library(zoo)
ts1 <- zoo(data1[,c(5,3,2)], order.by = data1$Data)
ts2 <- zoo(data2[,c(5,3,2)], order.by = data2$Data)
ts3 <- zoo(data3[,c(5,3,2)], order.by = data3$Data)

save_plot("ts1.png",plot(ts1))
save_plot("ts2.png",plot(ts2))
save_plot("ts3.png",plot(ts3))

#library(ggplot2) -> Pacote para a construção de gráficos
#autoplot(ts$index_act)

##################################################################
# Estimando as equações do sistema
#################################################################

library(dynlm)

VAR_EQ1_ts1 <- dynlm( delta_prod ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts1)
VAR_EQ1_ts2 <- dynlm( delta_prod ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts2)
VAR_EQ1_ts3 <- dynlm( delta_prod ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts3)

VAR_EQ2_ts1 <- dynlm( index_act ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts1)
VAR_EQ2_ts2 <- dynlm( index_act ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts2)
VAR_EQ2_ts3 <- dynlm( index_act ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts3)

VAR_EQ3_ts1 <- dynlm( oil_price ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts1)
VAR_EQ3_ts2 <- dynlm( oil_price ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts2)
VAR_EQ3_ts3 <- dynlm( oil_price ~ L(delta_prod, 1:2) + L(index_act, 1:2) +
                    L(oil_price,1:2) ,ts3)


summary(VAR_EQ1_ts1)
summary(VAR_EQ1_ts2)
summary(VAR_EQ1_ts3)

summary(VAR_EQ2_ts1)
summary(VAR_EQ2_ts2)
summary(VAR_EQ2_ts3)

summary(VAR_EQ3_ts1)
summary(VAR_EQ3_ts2)
summary(VAR_EQ3_ts3)

######################################################################
# Estimando o modelo VAR diretamente
######################################################################

library(vars)

p<- VARselect(ts, lag.max = 36, type="const")

VAR1 <- VAR(ts1, p=5, type="const")
VAR2 <- VAR(ts2, p=5, type="const")
VAR3 <- VAR(ts3, p=5, type="const")

summary(VAR1)
summary(VAR2)
summary(VAR3)

plot(irf(VAR, n.ahead = 48, impulse = "oil_price", response="oil_price", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "oil_price", response="delta_prod", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "oil_price", response="index_act", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "delta_prod", response="delta_prod", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "delta_prod", response="index_act", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "delta_prod", response="oil_price", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "index_act", response="index_act", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "index_act", response="delta_prod", boot = TRUE, ci = 0.95, runs=1000))
plot(irf(VAR, n.ahead = 48, impulse = "index_act", response="oil_price", boot = TRUE, ci = 0.95, runs=1000))

#Decomposição da Variância
fevd = fevd(VAR, n.ahead=48)
fevd


write.csv(fevd$delta_prod,"p3_delta_prod.csv")
write.csv(fevd$index_act,"p3_index_act.csv")
write.csv(fevd$oil_price,"p3_oil_price.csv")

write.table(fevd$oil_price,"p3_oil_price.rtf")


install.packages("XLConnect")
library(XLConnect)
writeWorksheetToFile("fevd.xlsx",
                     data = fevd$delta_prod,
                     sheet = "A")
writeWorksheetToFile("fevd.xlsx",
                     data = fevd$index_act,
                     sheet = "B")
writeWorksheetToFile("fevd.xlsx",
                     data = fevd$oil_price,
                     sheet = "C")

save(amostra, data, VAR, file= "aplicacao.Rda")
