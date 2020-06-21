##Analisis de regresion 

#Regresion de la adaptabilidad del estudiantes respecto a la adaptabilidad del profesor

regresion <- lm(Proyecto$Adaptabilidad_a_clases ~ Proyecto$`Adaptabilidad_de_los_ profesores`, data = Proyecto)
summary(regresion)

Call:
  lm(formula = Proyecto$Adaptabilidad_a_clases ~ Proyecto$`Adaptabilidad_de_los_ profesores`, 
     data = Proyecto)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.03827 -0.53420 -0.03827  0.46580  1.46580 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                                   1.5505     0.4393   3.529 0.001086 ** 
  Proyecto$`Adaptabilidad_de_los_ profesores`   0.4959     0.1248   3.974 0.000296 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.683 on 39 degrees of freedom
Multiple R-squared:  0.2882,	Adjusted R-squared:  0.2699 
F-statistic: 15.79 on 1 and 39 DF,  p-value: 0.0002963

plot(Proyecto$`Adaptabilidad_de_los_ profesores`, Proyecto$Adaptabilidad_a_clases, xlab = 'Aptabilidad del profesor', ylab = 'Adapatabilidad del estudiante')

abline(regresion)

cor(Proyecto$`Adaptabilidad_de_los_ profesores`, Proyecto$Adaptabilidad_a_clases)
[1] 0.5368309

#Regresion de la adpatabilidad del estudiante respecto a las megas de internet del estudiante

regresion1 <- lm(Proyecto_2$Adaptabilidad_a_clases ~ Proyecto_2$Megas_del_internet, data = Proyecto_2)

summary(regresion1)
Call:
  lm(formula = Proyecto_2$Adaptabilidad_a_clases ~ Proyecto_2$Megas_del_internet, 
     data = Proyecto_2)

Residuals:
  Min      1Q  Median      3Q     Max 
-1.3989 -0.3990 -0.1179  0.6010  1.9445 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                   2.961794   0.216223  13.698 3.61e-15 ***
  Proyecto_2$Megas_del_internet 0.012490   0.008944   1.397    0.172    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7861 on 33 degrees of freedom
Multiple R-squared:  0.0558,	Adjusted R-squared:  0.02719 
F-statistic:  1.95 on 1 and 33 DF,  p-value: 0.171

plot(Proyecto_2$Megas_del_internet, Proyecto_2$Adaptabilidad_a_clases, xlab = 'Megas_del_internet', ylab = 'Adapatabilidad del estudiante')

abline(regresion1)

cor(Proyecto_2$Megas_del_internet, Proyecto_2$Adaptabilidad_a_clases)
[1] 0.2362224

#Regresion de la adpatablidad del estudiante respecto a la relacion con las personas con las que viva


regresion2 <- lm(Proyecto_3$Adaptabilidad_a_clases ~ Proyecto_3$Relacion_con_sus_residentes, data = Proyecto_3)

summary(regresion2)

Call:
  lm(formula = Proyecto_3$Adaptabilidad_a_clases ~ Proyecto_3$Relacion_con_sus_residentes, 
     data = Proyecto_3)

Residuals:
  Min      1Q  Median      3Q     Max 
-1.5521 -0.2856 -0.1967  0.4479  1.4479 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)  
(Intercept)                              1.7749     0.7110   2.496   0.0170 *
  Proyecto_3$Relacion_con_sus_residentes   0.3555     0.1688   2.106   0.0418 *
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7752 on 38 degrees of freedom
Multiple R-squared:  0.1045,	Adjusted R-squared:  0.08098 
F-statistic: 4.436 on 1 and 38 DF,  p-value: 0.04184

plot(Proyecto_3$Relacion_con_sus_residentes, Proyecto_3$Adaptabilidad_a_clases, xlab = 'Relacion con las personas con las que vive', ylab = 'Adapatabilidad del estudiante')

abline(regresion2)

cor(Proyecto_3$Relacion_con_sus_residentes, Proyecto_3$Adaptabilidad_a_clases)
[1] 0.3233329   
