#############################################################################
# PEQUENO MODELO MACROECONOMICO
############################################################################


library(quantmod)
#getSymbols('WTISPLC', src='FRED')
#getSymbols('^IXIC', src='yahoo', from='1984-11-01')

getSymbols('GNPC96', src='FRED')
gnp <- GNPC96
rm(GNPC96)

getSymbols('BOGZ1FA825019905Q', src='FRED')
Inv <-BOGZ1FA825019905Q
rm(BOGZ1FA825019905Q)

getSymbols('GNPDEF', src='FRED')

getSymbols('MANMM101USQ189S', src='FRED')
M1 <- MANMM101USQ189S

getSymbols('LRUN25TTUSQ156S', src='FRED')
u <- LRUN25TTUSQ156S

getSymbols('WGS10YR', src='FRED')
t <- WGS10YR

rm(WGS10YR,LRUN25TTUSQ156S,MANMM101USQ189S)
library(ggplot2)
autoplot(gnp)
autoplot(Inv)
autoplot(u)
autoplot(M1)
autoplot(t)

#TRIMESTRALIZAR A TX DE JUROS:

T <- apply.quarterly(t,mean)

rm(t,WGS10YR)

#DEFLACIONAR A SERIE DE INVESTIMENTO

I=merge.xts(Inv, GNPDEF, join = "inner")

I=I[20:296,]

I$i=(I$BOGZ1FA825019905Q/I$GNPDEF)*100
autoplot(I)


I$i=log(I$i)


I$GNPDEF=log(I$GNPDEF)


BASE=merge(I[,2:3],gnp, join = "inner")

BASE=merge(BASE,M1, join = "inner")

BASE=merge(BASE,u, join = "inner")

BASE1=merge(BASE,T, join = "inner")

library(zoo)
base=data.frame(BASE)
base$data=as.yearqtr(index(BASE))

t=data.frame(T)
t$data=as.yearqtr(index(T))

BASE1=merge(base,t,by = "data")

colnames(BASE1)=c('data','p','i', 'y', 'm', 'u', 'r')

BASE1$y=log(BASE1$y)
BASE1$m=log(BASE1$m)


BASE=xts(BASE1[,2:7],order.by = BASE1$data)
autoplot(BASE)

library(vars)
VARselect(BASE, lag.max = 8, type = "both")
var.est1 <- VAR(BASE, p = 2, type = "both")
summary(var.est1)

library(tseries) # for adf.test function
adf.test(BASE$p) # nao estacionaria
adf.test(BASE$i) # estacionaria
adf.test(BASE$y) # nao estacionaria
adf.test(BASE$m) # nao estacionaria
adf.test(BASE$u) # estacionaria
adf.test(BASE$r) # 


# TORNANDO ESTACIONARIA AS SERIES NAO ESTACIONARIAS:
BASE$p=diff(BASE$p, lag = 1, differences = 1)
BASE$y=diff(BASE$y, lag = 1, differences = 1)*100
BASE$m=diff(BASE$m, lag = 1, differences = 1)*100
BASE$p=100*BASE$p

VARselect(BASE[2:236,], lag.max = 8, type = "const")
var.est1 <- VAR(BASE[2:236,], p = 2, type = "const")
summary(var.est1)


e <- rbind(c(var.est1$varresult$p$residuals),
           c(var.est1$varresult$i$residuals),
           c(var.est1$varresult$y$residuals),
           c(var.est1$varresult$m$residuals),
           c(var.est1$varresult$u$residuals),
           c(var.est1$varresult$r$residuals))

#rownames(e) <- c('e1', 'e2', 'e3')

##Hip?teses do modelo estrutural:
# i) Somente o r, y e i afetam de maneira conteporanea o preco.
# ii) Somente investimento e tx de juros (i, r) afetam de maneira contemporanea o produto.
# iii) A oferta de moeda é afetada por r, y e p
# iv) Exceto a oferta de moeda(m) todas as variaveis afetam contemporaneamente o desemprego.
# v) Somente a oferta de moeda afeta a taxa de juros (r).
#Matriz de efeitos contempor?neos
a.mat <- diag(6)
diag(a.mat) <- NA
a.mat[1,c(2,3,6)] <- NA
a.mat[3,c(2,6  )] <- NA
a.mat[4,c(1,3,6)] <- NA
a.mat[5,c(1:3,6)] <- NA
a.mat[6,c(4    )] <- NA
print(a.mat)


#Matriz de identifica??o dos choques estruturais (matriz diagonal)
b.mat <- diag(6)
diag(b.mat) <- NA
print(b.mat)

# b) Estima o modeo VAR Estrutural
svar <- SVAR(var.est1,estmethod = "direct", Amat = a.mat, Bmat = b.mat,
             hessian = TRUE, method="BFGS")

#Identificando os choques estruturais
A <- svar$A

#Para identificar os choques ? necess?rio pr?-multiplicar a matriz de erros de previs?o pela
#matriz de efeitos contempor?neos
Z <- A%*%e

#Transpondo a matriz
Z <- t(Z)

#Transformando em s?ries temporas
Z <- ts(Z, start=c(1962,3), frequency=4)

plot(Z)


#Tomando a m?dia anual dos res?duos (Figura 1 do artigo do Killian)

Z.A <- aggregate(Z, nf=1, mean)

plot(Z.A)

library(ggplot2)
plot(Z.A)


# c) Impulso resposta

irf.p =irf(svar,impulse="p",response=c("i","y","m","u","r"),n.ahead = 16, ortho = TRUE, boot = TRUE)
irf.i =irf(svar,impulse="i",response=c("p","y","m","u","r"),n.ahead = 16, ortho = TRUE, boot = TRUE)
irf.y =irf(svar,impulse="y",response=c("i","p","m","u","r"),n.ahead = 16, ortho = TRUE, boot = TRUE)
irf.m =irf(svar,impulse="m",response=c("i","y","p","u","r"),n.ahead = 16, ortho = TRUE, boot = TRUE)
irf.u =irf(svar,impulse="u",response=c("i","y","m","p","r"),n.ahead = 16, ortho = TRUE, boot = TRUE)
irf.r =irf(svar,impulse="r",response=c("i","y","m","u","p"),n.ahead = 16, ortho = TRUE, boot = TRUE)



plot(irf.p)
plot(irf.i)
plot(irf.y)
plot(irf.m)
plot(irf.u)
plot(irf.r)






















# gamma is the number of standard deviations for the irf
gamma=-1
irf.oil.neg = irf.preco
n=length(irf.oil.neg$irf$delta_prod)
for(i in 1:n){
  irf.oil.neg$irf$delta_prod[i]   = irf.oil.neg$irf$delta_prod[i]*gamma
  irf.oil.neg$Lower$delta_prod[i] = irf.oil.neg$Lower$delta_prod[i]*gamma
  irf.oil.neg$Upper$delta_prod[i] = irf.oil.neg$Upper$delta_prod[i]*gamma
} 
plot(irf.delta_prod.e)
plot(irf.oil.neg)


