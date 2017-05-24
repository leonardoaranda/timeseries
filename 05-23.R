library(lmtest)
library(car)

N = 250

tiempo = 0:(N-1)

m = 0.3

b0 = 10

x <- 15*sin(2*pi*tiempo/N) + 5*sin(5*2*pi*tiempo/N) +
  
rnorm(N,sd=10) + m*tiempo + b0

x <- ts(x)

plot(tiempo,x, type="l")

fit.tslm <-tslm(x ~ trend)

f <-forecast( fit.tslm, h=20,level=c(80,95))

plot( f , ylab="x", xlab="tiempo")

lines ( fitted ( fit.tslm), col="blue")

summary(fit.tslm)

par(mfrow=c(1,2))

res <- ts(resid( fit.tslm))

plot.ts( res , ylab="res (x)")

abline (0,0)

Acf( res )


print(dwtest( fit.tslm, alt ="two.sided"))

# Durbin−Watson test

# data: fit .tslm

# DW = 1.2978, p−value = 1.719e−08

# alternative hypothesis: true autocorrelation is not 0

par(mfrow=c(1,1))

bins <- hist(res , breaks="FD", xlab="Residuos",main="Histograma de residuos")

xx <- -40:40

lines (xx, 1300*dnorm(xx,0,sd(res)), col=2)

# Descomposicion clasica

library(fpp)

data( elecequip ) # viene en el fpp

plot( elecequip )

fit.decomp <- decompose(elecequip, type="additive")

plot(fit.decomp)

# Forecasting STL

library (fpp)

data(elecequip) # viene en el fpp

plot(elecequip)

fit.stl <- stl( elecequip , t.window=15, s.window="periodic",robust=TRUE)

plot(fit.stl)

eeadj <- seasadj(fit.stl) # remueve la componente estacional

plot(naive(eeadj), xlab="index",main="Forecasting de datos con estacionalidad removida")

fcast <- forecast( fit.stl , method="naive")

plot( fcast , ylab="index",main="Forecasting completo")

