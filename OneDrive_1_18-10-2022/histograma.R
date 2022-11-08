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