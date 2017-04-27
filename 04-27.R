dos.locales <- read.csv("DosLocales.csv")
names(dos.locales)

N <- length(dos.locales$localA)
print(paste("N =",N))

localAfal <- approx(dos.locales$t,dos.locales$localA,n=3*N)
localAfas = spline(dos.locales$t,dos.locales$localA,n=3*N)

# Aumentar la frecuencia de muestreo
plot(dos.locales$t,dos.locales$localA,type="l",main="Ventas Locales A",xlab="dias", ylab="ventask$", xlim=c(45,55),ylim=c(8,17))
points(dos.locales$t,dos.locales$localA,pch=20,col="black")

for(j in 1:N){
  abline(v=j, lty =2)
}

lines(localAfal$x,localAfal$y,col="red")
points(localAfal$x,localAfal$y,pch=20,col="red")
lines(localAfas$x,localAfas$y,col="blue")
points(localAfas$x,localAfas$y,pch=20,col="green")


# Efecto en el dominio de la frecuencia.
# (No pasa naranja.)

fft.localA = Mod(fft(dos.locales$localA))
fft.localAfal = Mod(fft(localAfal$y))
fft.localAfas = Mod(fft(localAfas$y))

tiempo = 0:(N-1)
tiempo3 = 0:(3*N-1)

FrecuenciaMuestreo = 1
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo*tiempo

FrecuenciaMuestreo = 3
DeltaFrecMuestreo = FrecuenciaMuestreo/(3*N)
FrecuenciaInterpolada = DeltaFrecMuestreo*tiempo3

op <- par(mfrow = c(1, 1))
plot(Frecuencia,fft.localA,type='l',xlim=c(0,0.1))
lines(FrecuenciaInterpolada,fft.localAfal/3,col='red')
lines(FrecuenciaInterpolada,fft.localAfas/3,col='green')

#interpolamos por ejemplo para correlacionar dos series de tiempo que estan a distinta frecuencia de muestreo.

# Caso 2, disminuir la frecuencia de muestreo

N = length(dos.locales$localA)
print(paste('N =',N))

localAfal = approx(dos.locales$t,dos.locales$localA,n=N/3)
localAfas = spline(dos.locales$t,dos.locales$localA,n=N/3)

plot(dos.locales$t,dos.locales$localA,type = 'l',main='Ventas Locales A',xlab='dias',ylab='ventas k$',xlim=c(45,55),ylim=c(8,17))
points(dos.locales$t,dos.locales$localA,pch=20,col='black')

for(j in 1:N)
{
  abline(v=j,lty=2)
}

lines(localAfal$x,localAfal$y,col='red')
points(localAfal$x,localAfal$y,pch=20,col='red')
lines(localAfas$x,localAfas$y,col='blue')
points(localAfas$x,localAfas$y,pch=20,col='green')

#efecto en la frecuencia.


fft.localA = Mod(fft(dos.locales$localA))
fft.localAfal = Mod(fft(localAfal$y))
fft.localAfas = Mod(fft(localAfas$y))

tiempo = 0:(N-1)
tiempo3 = 0:(N/3)

FrecuenciaMuestreo = 1
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo*tiempo

FrecuenciaMuestreo = 1/3
DeltaFrecMuestreo = FrecuenciaMuestreo/(N/3)
FrecuenciaInterpolada = DeltaFrecMuestreo*tiempo3

op <- par(mfrow = c(1, 1))
plot(Frecuencia,fft.localA,type='l',xlim=c(0,0.1))
lines(FrecuenciaInterpolada,3*fft.localAfal,col='red')
lines(FrecuenciaInterpolada,3*fft.localAfas,col='green')