##Analisis de regresion 

#Regresion de la adaptabilidad del estudiantes respecto a la adaptabilidad del profesor

regresion <- lm(Proyecto$Adaptabilidad_a_clases ~ Proyecto$`Adaptabilidad_de_los_ profesores`, data = Proyecto)
summary(regresion)
plot(Proyecto$`Adaptabilidad_de_los_ profesores`, Proyecto$Adaptabilidad_a_clases, xlab = 'Aptabilidad del profesor', ylab = 'Adapatabilidad del estudiante')
abline(regresion)
cor(Proyecto$`Adaptabilidad_de_los_ profesores`, Proyecto$Adaptabilidad_a_clases)


#Regresion de la adpatabilidad del estudiante respecto a las megas de internet del estudiante

regresion1 <- lm(Proyecto_2$Adaptabilidad_a_clases ~ Proyecto_2$Megas_del_internet, data = Proyecto_2)
summary(regresion1)
plot(Proyecto_2$Megas_del_internet, Proyecto_2$Adaptabilidad_a_clases, xlab = 'Megas_del_internet', ylab = 'Adapatabilidad del estudiante')
abline(regresion1)
cor(Proyecto_2$Megas_del_internet, Proyecto_2$Adaptabilidad_a_clases)


#Regresion de la adpatablidad del estudiante respecto a la relacion con las personas con las que viva

regresion2 <- lm(Proyecto_3$Adaptabilidad_a_clases ~ Proyecto_3$Relacion_con_sus_residentes, data = Proyecto_3)
summary(regresion2)
plot(Proyecto_3$Relacion_con_sus_residentes, Proyecto_3$Adaptabilidad_a_clases, xlab = 'Relacion con las personas con las que vive', ylab = 'Adapatabilidad del estudiante')
abline(regresion2)
cor(Proyecto_3$Relacion_con_sus_residentes, Proyecto_3$Adaptabilidad_a_clases)
    
