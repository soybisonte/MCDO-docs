library(dplyr)


# dbinom(x,n,p)   P(X = x)
dbinom(2,4,0.75)   # P(X = 2)

# obtienen P(X <= 2) usando dbinom
dbinom(0,4,0.75) + dbinom(1,4,0.75) +  dbinom(2,4,0.75)

# obtienen P(X <= 2) usando la función de distribución acumulada
pbinom(2,4,0.75)

# P(X > 2)
pbinom(2,4,0.75,lower.tail = FALSE)

# P(X >= 2)
pbinom(1,4,0.75,lower.tail = FALSE)

# P(X > 2) = P(X=3) + P(X=4)
dbinom(2,4,0.75) + dbinom(3,4,0.75) +  dbinom(4,4,0.75)


# generacion de numeros pseudoaleatorios con distr. binomial

# rbinom(n =  # de pruebas, size = rango en donde quiero los valores, prob =  prob de extito en cada intento)
# 0, 1 o 2
rbinom(10,2,0.3)
# 0, 1, 2, 3 o 4
rbinom(10,4,0.3)
# probabilidad de éxito
p = 0.3
# numero de ensayos
n = 10

# valores del 0 al:
m = 5

set.seed(0.01)
rbinom(n,m,p)

n <- 300
k <- 50
p <- 0.15
x <- rbinom(n,k,p)
par(mfrow = c(2,2))
plot(x)

plot(histograma(x,20))

xf <- factor(x)
tbl <- xf %>%
  table() %>%
  as.data.frame() %>% # hace la tabla con la frec de cada valor
  setNames(c("Dato", "FREC"))%>% # pone nombre a las col de la tabla
  # agregamos la columna de la frec. relativa y el porc
  mutate(Frec_R = round(FREC/sum(FREC),4), PORC = Frec_R*100)%>% 
  print()

plot(tbl$Frec_R, type = "h", col = "red3", lwd = 5, xlab = "Valor de la v.a. X",
     ylab = "Frec. relativa")


n<-13
p<-0.15
y <- numeric()
for (i in 1:13){
  y <- c(y,dbinom(i,n,p))
}

plot(y, type = "h", col = "blue", lwd = 5, xlab = "Valor de la v.a. X",
     ylab = " ")



y=0
l=1
py <- dpois(y,l)
print(py)
1-dpois(0,1)

l=2
dpois(1,l) + dpois(2,l) + dpois(3,l)

ppois(3,l)-dpois(0,l)
# proba. de que haga de 5 a 10 vistas
# P( 5 <= Y <= 10 )

ppois(10,l) - ppois(4,l)

# P( 5 <  Y <= 10 )

ppois(10,l) - ppois(5,l)


dpois(5,3)

dpois(10,6)

1-ppois(1,4.5)

# con lambda = 3
y = 0:11
prob <- numeric()
for(i in y){
  prob <- c(prob,dpois(i,5))
}

print(prob)
plot(y,prob, type = "h")

1-ppois(2,8)

(ppois(5,8)-ppois(2,8))/(1-ppois(2,8))

(ppois(5,8)-ppois(2,8))


par(mfrow = c(1,2))
y = 0:15
prob <- numeric()
for(i in y){
  prob <- c(prob,dpois(i,8))
}
plot(y,prob,main="Poisson")


n=8000
y = 0:n
p=1/1000
prob_b <- numeric()
for(i in y){
  prob_b <- c(prob_b,dbinom(i,n,p))
}

plot(y[0:15],prob_b[0:15],main="Binomial")


plot(y[0:15],prob[0:15],main="Poisson azul, Bin rojo", col="red",type="h")
par(new=TRUE)
plot(y[0:15],prob_b[0:15],main="",col="blue")












































































































