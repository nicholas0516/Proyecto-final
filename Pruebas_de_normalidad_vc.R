### PRUEBAS DE NORMALIDAD POR MEDIO DEL TEST DE JARQUE BERA
## H0 = la muestra proviene de una poblacion normal
## H1 = la muestra NO proviene de una poblacion normal

#Pruebas de normalidad a la variable de adaptabilidad del estudiante a las clases virtuales.

> jb.norm.test(Proyecto$Adaptabilidad_a_clases)

Jarque-Bera test for normality

data:  Proyecto$Adaptabilidad_a_clases
JB = 0.52254, p-value = 0.728   ## Dado que el p-value es mayor que la significancia 
                                ## del 5 % se establece como normal.

#Pruebas de normalidad a la variable de adaptabilidad de los profesores a la virtualizacion.

> jb.norm.test(Proyecto$`Adaptabilidad_de_los_ profesores`)

Jarque-Bera test for normality

data:  Proyecto$`Adaptabilidad_de_los_ profesores`
JB = 7.9585, p-value = 0.025    ## Dado que el p-value es mayor que la significancia 
                                ## del 5 % se establece como normal.

#Pruebas de normalidad de la variable cancelacion de materias

s <- function(x){
  ##funcion para establecer a si ~ 0 y no ~ 1
  if(x == "Si") { 0
  }
  else{1}
}               
cancelacion_materias <- as.vector(unlist(lapply(Proyecto$Cancelación_materias, s)))

## Se realizo un tratamiento a las variable para facilitar su analisis, dejando 
## a la respuesta "Si" con un valor de 0 y "No" con un valor de 1. 

> jb.norm.test(cancelacion_materias)

Jarque-Bera test for normality

data:  cancelacion_materias
JB = 10.291, p-value = 0.014  ## Dado que el p-value es menor que el grado de significancia
                              ## se establece que no es normal, pero a pesar del resultado
                              ## a traves del teorema central del limite se puede concluir 
                              ## que son normales dado el tamaño de las muestras.

#Pruebas de normalidad a la variable calidad del internet

d <- function(x){
  ##funcion para establecer a si ~ 0 y no ~ 1
  if(x == "Buena") { 0
  }
  else{1}
}
calidad_internet <- as.vector(unlist( lapply(Proyecto$`Calidad_del_internet_en _clases`, d)))

## Se realizo un tratamiento a la variable para facilitar su analisis, dejando 
## a la respuesta "Buena" con un valor de 0 y "Mala" con un valor de 1. 

> jb.norm.test(calidad_internet)

Jarque-Bera test for normality

data:  calidad_internet
JB = 7.4861, p-value = 0.026  ## Dado que el p-value es menor que el grado de significancia
                              ## se establece que no es normal, pero a pesar del resultado
                              ## a traves del teorema central del limite se puede concluir 
                              ## que son normales dado el tamaño de las muestras.


#Pruebas de normalidad a la variable afectacion de la salud mental en el rendimiento academico.
  
d <- function(x){
  ## funcion para eliminar los datos "NA"
  bad <- is.na(x)
  x[!bad]
}
e <- function(x){
  ##funcion para establecer a si ~ 0 y no ~ 1
     if (x == "No"){1}
       else{0}
   }
  
salud_rendimiento <- d(Proyecto$`Salud_mental_rendimiento _cademico`)

salud_rendimiento_final <- as.vector(unlist( lapply(salud_rendimiento, e)))

## Se realizo un tratamiento a la variable para facilitar su analisis, dejando 
## a la respuesta "Si" con un valor de 0 y "No" con un valor de 1. 

> jb.norm.test(salud_rendimiento_final)

Jarque-Bera test for normality

data:  salud_rendimiento_final
JB = 10.46, p-value = 0.0115  ## Dado que el p-value es menor que el grado de significancia
                              ## se establece que no es normal, pero a pesar del resultado
                              ## a traves del teorema central del limite se puede concluir 
                              ## que son normales dado el tamaño de las muestras.
