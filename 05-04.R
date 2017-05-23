dato<-read.csv("EstudioEdadIngresos2.csv")

print(names(dato))

attach(dato)

op <- par(mfrow=c(2,2))
plot(Edad,ActitudCompraPrePre,main="A")
plot(Edad,Ingresos,main="B")
plot(Ingresos,ActitudCompraPrePre,main="C")

print(cor.test(Edad,Ingresos))
print(cor.test(Edad,ActitudCompraPrePre))
print(cor.test(Ingresos,ActitudCompraPrePre))

op <- par(mfrow=c(1,1))
plot(Edad,ActitudCompraPrePre)
reg <- lm(ActitudCompraPrePre ~ Edad)
abline(reg)

print(summary(reg))

# analisis de regresion no lineal

coef.a <- 1000
coef.b <- 0.3

x <- 0:20
y <- coef.a + exp(coef.b*x) + 20*rnorm(length(x))

plot(x,y)

lines(x,coef.a + exp(coef.b*x))


regexp <- nls(y ~ a+ exp(b*x),start=list(a=500,b=0.5),trace = TRUE)

print(summary(regexp))

plot(x,y)

lines(x,1003+exp(0.2999*x))
lines(x,992+exp(2952152*x),lty=2)
lines(x,1014+exp(0.3042097*x),lty=2)

# analisis de covarianza

# dos rectas con la misma pendiente con diferente ordenada al origen

dir = '/..../' # directorio del csv


print(names(dato))

covar.ActitudCompraPre.Edad=lm(ActitudCompraPre~Edad+Grupo)
print(summary(covar.ActitudCompraPre.Edad))
plot(Edad[Grupo=="A"],ActitudCompraPre[Grupo=="A"],
     xlim=c(20,70),ylim=c(min(ActitudCompraPre)-0.5,max(ActitudCompraPre)+0.5),
     main="",pch='A',
     xlab="Edad",ylab="Actitud de Compra")
abline(covar.ActitudCompraPre.Edad$coefficients[1],covar.ActitudCompraPre.Edad$coefficients[2])
points(dato$Edad[dato$Grupo=="B"],dato$ActitudCompraPre[dato$Grupo=="B"],pch='B')
abline(covar.ActitudCompraPre.Edad$coefficients[1]+covar.ActitudCompraPre.Edad$coefficients[3],
       covar.ActitudCompraPre.Edad$coefficients[2],lty="dashed")

print(confint(covar.ActitudCompraPre.Edad))


# Detrending

dos.locales <- read.csv("DosLocales.csv")
print(names(dos.locales))

m <- 0.1

localAtend <- dos.locales$localA + m*dos.locales$t

plot(dos.locales$t,localAtend,type="l",main="Ventas locales A con tendencia",xlab="dias",ylab="ventas k$")

fft.localA <- Mod(fft(dos.locales$localA))
fft.localAtend <- Mod(fft(localAtend))

plot(fft.localA,type="l",xlim=c(0,length(fft.localA)/2))
lines(fft.localAtend,col="red")

