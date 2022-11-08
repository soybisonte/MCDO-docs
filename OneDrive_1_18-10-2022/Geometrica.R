# latex2exp
library(latex2exp)

# funcion para dibujar histogramas

histograma <- function(datos, cubetas){
  minimo <- min(datos)
  maximo <- max(datos)
  ancho <- maximo-minimo
  cubeta <- ancho/cubetas
  intervalos <- seq(minimo,maximo,cubeta)
  barras <- cut(datos, breaks = intervalos)
  cat("Cubetas: ",intervalos)
  return(barras)
}



# <-     =
p <- 0.12
n <- 35
g <- numeric()
k <- seq(0,n,1) # 0 1 2 3 4 5
# X=0: e   p
# X=1: fe  (1-p)^1 * p
# X=2: ffe (1-p)^2 * p
# x=3: fffe (1-p)^3 * p


# X=35: ffff...ffff_35 e_(36) (1-p)^35 * p
for (x in k){
  g <- c(g,p*(1-p)^x)
}
print(g)

plot(k,g, pch=20,type = "h",lwd = 3, col="red")

# rgeom: 

n <- 5000
# p <- 0.2
datos <- rgeom(n,p)
plot(datos)

plot(histograma(datos, cubetas=20),
     xlab = "Clases, Cubetas, Intervalos",
     ylab = "Frecuencia",
     main = TeX('Histograma $Geom(5000,0.2)$'))

# Usando la funcion dgeom
y <- dgeom(k,p)


plot(g,pch=20,xaxt="n")
axis(1, at =seq(0,n,1),labels = 0:n)
axis(2, at= seq(0,0.2,1),labels = 0:0.2)

par(new=TRUE)
plot(histograma(datos, cubetas=30),axes=FALSE)






















































