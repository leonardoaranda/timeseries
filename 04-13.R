# Generación de una linea de tiempo y una serie temporal
# Teorema de muestreo. Frecuencia maxima minima

tiempo <- seq(from=0,to=255,by=1)
n <- length(tiempo)

x <- rnorm(n,mean=2,sd=0.5)
print(paste('media=',mean=(x)))
print(paste('de=',sd(x)))

plot(tiempo,x,type='l')
abline(h=mean(x),lty=2)
abline(h=mean(x)+2*sd(x),lty=3)
abline(h=mean(x)-2*sd(x),lty=3)

boxplot(x)
abline(h=mean(x),lty=2)
abline(h=mean(x)+2*sd(x),lty=3)
abline(h=mean(x)-2*sd(x),lty=3)

hx <- hist(x)
abline(v=mean(x),lty=2)
abline(v=mean(x)+2*sd(x),lty=3)
abline(v=mean(x)-2*sd(x),lty=3)


max.hist <- max(hx$counts)
mu <- mean(x)
sigma <- sd(x)
ejex <- seq(from=-min(x),to=max(x),by=0.01)
fx.normal <- (1/sqrt(2*pi*sigma))*exp((ejex-mu)*(ejex-mu)/(-2*sigma*sigma))
fx.normal <- (max.hist/max(fx.normal))*fx.normal
plot(ejex,fx.normal,main="Distribución normal",type='l')

hx <- hist(x)
abline(v=mean(x),lty=2)
abline(v=mean(x)+2*sd(x),lty=3)
abline(v=mean(x)-2*sd(x),lty=3)
lines(ejex,fx.normal,col='red')


x.df <- data.frame(t=tiempo,x=x)
write.csv(x.df,'xt.csv')

x.df2 <- read.csv('xt.csv')
plot(x.df2$t,x.df2$x,type='l')

x.ts <- ts(rnorm(n,mean=2,sd=0.5),start=0,frequency=10)

print(frequency(x.ts))
print(deltat(x.ts))
print(time(x.ts))

print(x.ts[1])
print(x.ts[5])


print(time(x.ts)[1])
print(time(x.ts)[5])

# senoidales

ciclos <- 1
amplitud <- 1
seno1 <- amplitud*sin(ciclos*2*pi*tiempo/n)
plot(tiempo,seno1,type='l')

ciclos <- 2
amplitud <- 1
seno2 <- amplitud*sin(ciclos*2*pi*tiempo/n)
plot(tiempo,seno2,type='l')

ciclos <- 10
amplitud <- 2
seno10 <- amplitud*sin(ciclos*2*pi*tiempo/n)
plot(tiempo,seno10,type='l')

ciclos <- 20
amplitud <- 0.5
seno20 <- amplitud*sin(ciclos*2*pi*tiempo/n)
plot(tiempo,seno20,type='l')

niveldc <- rep(1,n)
plot(tiempo,niveldc,type='l')

seno121020dc <- seno1+seno2+seno10+seno20+niveldc
plot(tiempo,seno121020dc,type='l')

fft.seno1 <- fft(seno1)
plot(Mod(fft.seno1),type='l')

seno1a <- Re(fft(fft.seno1,inverse=TRUE)/n)
plot(tiempo,seno1a,type='l')

fft.seno2 <- fft(seno2)
plot(Mod(fft.seno2),type='l')

fft.seno10 <- fft(seno10)
plot(Mod(fft.seno10),type='l')

fft.seno20 <- fft(seno20)
plot(Mod(fft.seno20),type='l')

fft.niveldc <- fft(niveldc)
plot(Mod(fft.niveldc),type='l')

fft.seno121020dc <- fft(seno121020dc)
plot(Mod(fft.seno121020dc),type='l')

# remover la componente de frecuencia cero (dc)

fft.seno121020dc[1] <- 0
plot(Mod(fft.seno121020dc),type='l')

seno121020dc2 <- fft(fft.seno121020dc,inverse=TRUE)/n
plot(tiempo,Re(seno121020dc2),type='l')

plot(tiempo,seno121020dc,type='l',ylim=c(-4,5))
lines(tiempo,seno121020dc2)
abline(h=1,lty=2)
abline(h=0,lty=2)

p1 <- rep(0,n)
p1[1] <- 1
plot(tiempo,p1,type='l')

p2 <- rep(0,n)
p2[1:2] <- 1
plot(tiempo,p2,type='l')

p5 <- rep(0,n)
p5[1:5] <- 1
plot(tiempo,p5,type='l')

p10 <- rep(0,n)
p10[1:10] <- 1
plot(tiempo,p10,type='l')

p20 <- rep(0,n)
p20[1:20] <- 1
plot(tiempo,p20,type='l')

p50 <- rep(0,n)
p50[1:50] <- 1
plot(tiempo,p50,type='l')

fft.p1 <- fft(p1)
plot(Mod(fft.p1),type='l')

fft.p2 <- fft(p2)
plot(Mod(fft.p2),type='l')

fft.p5 <- fft(p5)
plot(Mod(fft.p5),type='l')


fft.p10 <- fft(p10)
plot(Mod(fft.p10),type='l')

fft.p20 <- fft(p20)
plot(Mod(fft.p20),type='l')

fft.p50 <- fft(p50)
plot(Mod(fft.p50),type='l')

plot(Mod(fft.p2),type='l',ylim=c(0,50),xlim=c(0,64))
lines(Mod(fft.p5))
lines(Mod(fft.p10))
lines(Mod(fft.p20))
lines(Mod(fft.p50))

plot(Mod(fft.p2)/max(Mod(fft.p2)),type='l',ylim=c(0,1),xlim=c(0,64))
lines(Mod(fft.p5)/max(Mod(fft.p5)))
lines(Mod(fft.p10)/max(Mod(fft.p10)))
lines(Mod(fft.p20)/max(Mod(fft.p20)))
lines(Mod(fft.p50)/max(Mod(fft.p50)))
abline(h=0.7,lty=2)

DeltaT <- 0.001 # segundos
FrecuenciaMuestreo <- 1/DeltaT #Hertz
DeltaFrecMuestreo <- FrecuenciaMuestreo/n
Frecuencia <- DeltaFrecMuestreo*tiempo

plot(Frecuencia,Mod(fft.p2)/max(Mod(fft.p2)),type='l',ylim=c(0,1),xlim=c(0,250),ylab='Mod FFT p2 a p50 (normalizado)',xlab='Frecuencia (Hz)')
lines(Frecuencia,Mod(fft.p5)/max(Mod(fft.p5)))
lines(Frecuencia,Mod(fft.p10)/max(Mod(fft.p10)))
lines(Frecuencia,Mod(fft.p20)/max(Mod(fft.p20)))
lines(Frecuencia,Mod(fft.p50)/max(Mod(fft.p50)))
abline(h=0.7,lty=2)

# teorema de parseval

s2 <- sin(2*2*pi*tiempo/n)

E.dt <- sum(s2*s2)
print(paste('Energia DT = ',E.dt))

fft.s2 <- fft(s2)
E.df <- sum(Mod(fft.s2)*Mod(fft.s2))/n
print(paste('Energia DF = ',E.df))
