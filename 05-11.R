# convolucion
N = 256
tiempo = 0:(N-1)

# Convolucion de dos series de tiempo en el DT
# f1 pulso

f1 = rep(0,N)
f1[100:200] = 1

# f2 exponencial decreciente
f2 = 1*exp(-0.03*tiempo)
op <- par(mfrow = c(1, 2))
plot(tiempo,f1, type="l")
plot(tiempo,f2, type="l")

# algoritmo de convolucion en el dominio del tiempo

f3 = rep(NA,N)
for( i in 1:N){
  suma = 0
  j = 1
  while(j<=i){
    suma = suma + f1[j]*f2[i-j+1]
    j = j+1
  }
  f3[i] = suma
  print(i)
}

op <- par(mfrow = c(1,1))
plot(tiempo,f3, type="l", col="red")

# convolucion en el dominio del tiempo

# convolucion en el DF

fft.f1 = fft(f1)
fft.f2 = fft(f2)
op <- par(mfrow = c(1, 2))
plot(Mod(fft.f1), type="l")
plot(Mod(fft.f2), type="l")
fft.f3 = fft.f1*fft.f2

plot(Mod(fft.f3),type="l")
f3b = Re(fft(fft.f3, inverse =TRUE)/N)
plot(tiempo,f3b, type="l")

lines (tiempo,f3, col="red")

# correlacion cruzada con convolve

f1f2 = convolve(f1, f2, conj = TRUE)

plot(tiempo, f1f2 , type="l")

# correlacion curzada de dos pulsos

#autocorrelacion con el bitcoin para ver cuan aleatoria es.

p5d5 = rep(0,N)

p5d5[5:10] = 1
p5d25 = rep(0,N)
p5d25[25:30] = 1
plot(tiempo,p5d5,type="l" , ylab="p5d5 & p5d25")

# correlacioin cruzada te permite analizar el retardo entre series temporales

lines(tiempo,p5d25,col="red")

#correlacion curzada de dos senoides

xs1 = sin(4*2*pi*tiempo/N)

xs2 = sin(4*2*pi*tiempo/N+pi/2)

# xs2 esta defasada en pi/2 = 16 muestras

plot(tiempo,xs1, type="l")

lines (tiempo,xs2, col="red")

ccf (xs1,xs2, lag.max = 50)
ccf(p5d5,p5d25,lag.max = 50)

