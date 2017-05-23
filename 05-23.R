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