funcion_normal=function(mu,var,a=F,b=F){
  # los párametros de la función son:
  #   mu: media
  #   var: varianza
  #   a,b: intervalo para P(a < X < b)
  sd=sqrt(var)    # desviación
  
  xs = seq(-4,4,0.005)*sd+mu    # intervalo para ver la función, valores de X->Z
  
  pr_x = dnorm(xs,mu,sd)        # calculamos la función de densidad
  
  # grafica de la función de densidad para el problema original
  # la grafica sale muy gruesa porque se pintan muchos puntos:
  # type="n" la quita y luego con lines la agregamos
  # podemos observar la originar con pch=20, quitamos type
  plot(xs,pr_x,axes=F,type = "n",xlab="X", ylab="Densidad de probabilidad",  
       cex.lab=0.8,  # tamaño de letra de xlab y ylab
       main=substitute(
         paste("Distribución Normal ",mu==m,", ", sigma==s), # unimos texto + parámetros
         list(m=mu,s=sd)  # indico que m=mu y s=sd
       ),
       cex.main=0.9 # tamaño de letra del título
  )
  
  lines(xs,pr_x,col=2)    # pintamos con una linea continua la función de densidad dnorm
  xtick <- seq(mu-4*sd,mu+4*sd,by=1) # crea los valores que aparecen en el eje horizontal
  axis(side=1, pos=0,at=xtick,labels=TRUE) # pone xtick al eje horizontal
  abline(0,0,col=1)  # pinta una linea horizontal para alargar el eje horizontal
  
  h=dnorm(mu,mu,sd) # la usamos para pintal la linea de la media más adelante
  # opciones para calcular:
  #     P(X <= b)
  #     P(X >= a)
  #     P(a <= X <= b)
  if (a==F){  # si solo se le da b calcula P(X <= b)
    cat("\t \n \n P(X <= x) \n")
    a=mu-4*sd
    u=seq(a,b,length=500)
    area = dnorm(u,mu,sd)
    px=pnorm(b,mu,sd)
    cat("P(X <= ",b,") = ", px)
    text(mu-1.2*sd,0.95*h,paste("P(X <= ",b,") = ",format(round(px,4))),adj=0.85, col=1,cex=0.75)
  }else if(b == F){# si solo se da "A" calcula P(X>=a)
    cat("\t \n \n P(X >= x) \n")
    b=mu+4*sd
    u=seq(a,b,length=500)
    area = dnorm(u,mu,sd)
    px=1-pnorm(a,mu,sd)
    cat("P(X >= ",a,") = ", px)
    text(mu-1.2*sd,0.95*h,paste("P(X >= ",a,") =",format(round(px,4))),adj=0.85, col=1,cex=0.75)
  }else{
    cat("\t \n \n P(a <= X <= b) \n")
    u=seq(a,b,length=500)
    area = dnorm(u,mu,sd)
    px=pnorm(b,mu,sd)-pnorm(a,mu,sd)
    cat("P(",a," <= X <= ",b,")= ",px)
    text(mu-0.7*sd,0.999*h,paste("P(",a," <= X <= ",b,")= ",
                                 format(round(px,4))),adj=0.85,
         col=1,cex=0.7)
  }
  
  
  #pinta el área de la prob que queremos calcular
  lines(u,area,type="h",col=2)
  
  
  abline(v=mu,col="black",lw=2,lty=2)
  return(px)
}


