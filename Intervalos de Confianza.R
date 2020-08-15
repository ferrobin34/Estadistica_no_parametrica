
#Para mejorar esto se podría buscar el intervalo mas pequeño.

library(dplyr)
#Programemos la función de intervalos de confianza para cuantiles
cuantil.ei <- function(p,niv.cfza,muestra){
  n <- length(muestra)
  #ordenamos la muestra de menor a mayor
  muestra.ord <- sort(muestra)
  #Fucnión que calculará la confianza de cada intervalo posible
  probabilidad <- function(x,y) pbinom(y-1,n,p) - pbinom(x,n,p)
  
  #Creamos matriz de las probabilidades(confianzas) de los posibles intervalos
  probabilidades <- outer(1:n,1:n,probabilidad)
  #Escogemos los intervalos con confianza mayor o igual a niv.cfza(la que esta dada)
  candidatos <- which(probabilidades >= niv.cfza, arr.ind = T)
  
  #Vemos si existe al menos un intervalo para el nivel de confianza dado esto es
  #Si el objeto interv.candidatos no es vacio
  if(nrow(candidatos) > 0 ){
    #Escogemos uno de los candidatos, por comodidad el primero
    aviso <- "Es posible alcanzar el nivel de confianza."
    intervalo <- c(   muestra.ord[ candidatos[1,1] ],
                      muestra.ord[ candidatos[1,2] ]  )
    confianza <- probabilidad(candidatos[1,1] ,candidatos[1,2] )
    return(list(aviso = aviso, intervalo = intervalo, confianza = confianza))
    
  }else{
    #Si no existe ningún intervalo para el nivel de confianza lanzamos un mensaje
    #Y el mejor intervalo posible
    aviso <- "No es posible alcanzar el nivel de confianza."
    intervalo <- c( muestra.ord[1], muestra.ord[n])
    confianza <- if(probabilidad(1,n) > 0) probabilidad(1,n)
    return(list(aviso, intervalo, confianza))
  }
}

cuantil.ei(0.5,0.95, rnorm(100))
