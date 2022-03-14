#Modelo de Regresi?n lineal
porcentaje<-0.7
datos<-iris
set.seed(123)

datos$y<- as.numeric(datos$Species)
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]


#-------------------------------------------------
# Regresi?n Lineal Simple 
#-------------------------------------------------


fitLMPW<-lm(Petal.Length~Petal.Width, data = train)

#Estimar el lenght del p?talo a partir de su width
#-------
predL<-predict(fitLMPW, newdata = test)
#Verificando la predicci?n
resultados<-data.frame(test$Petal.Length,predL)

#An?lisis de residuos
# Los residuos se calcula restando la predicci?n de la variable respuesta
residuales <- test$Petal.Length-predL
#No obstante el modelo ya lo calcula por lo que se pueden usar
# Si se mira el resumen del modelo podemos analizar el comportamiento de los residuos.
summary(fitLMPW)

#En la gr?fica Residuals vs Fitted, se puede ver que los residuos se distribuyen de forma
# m?s o menos aleatoria alrededor de 0
# En el gr?fico qq se puede ver que puede ser un gr?fico normal


library(ModelMetrics)
rmse(test$Petal.Length,predL)
plot(test$Petal.Length, test$Petal.Width)
points(predL, test$Petal.Width, col="red",pch=15)

par(mfrow = c(2,2))
plot(fitLMPW)


#Predecir la clase de la flor por la longitud del p?talo
fitLMSpByPL<-lm(y~Petal.Length, data = train)
summary(fitLMSpByPL)
# Multiple R-squared:  0.905,	Adjusted R-squared:  0.9041

#El modelo explica los datos en un 90% la predicci?n debe ser buena

predMSpByPL<-predict(fitLMSpByPL,newdata = test)
resultados1<-data.frame(test$y,round(predMSpByPL,0))
names(resultados1)<-c("real","prediccion")


confusionMatrix(resultados1$real,resultados1$prediccion)
#Accuracy : 0.8889 

#-------------------------------------------------
# Regresi?n Lineal M?ltiple 
#-------------------------------------------------

fitLM<-lm(y ~ ., data = train)

summary(fitLM)
#El modelo se ajusta perfectamente a los datos
#Multiple R-squared:      1,	Adjusted R-squared:      1
#Advertencia que pone R:
# Warning message:
#   In summary.lm(fitLM) : essentially perfect fit: summary may be unreliable


predicted<-predict(fitLM,newdata = test)

test$prediccion <- predicted

cfm<-confusionMatrix(test$y,test$prediccion)
cfm
#Accuracy : 1 
#HAY SUBREAJUSTE. Esto se debe a que hay multicolinealidad en las variables participantes en el modelo




cor(datos$Petal.Length,datos$Petal.Width, method = "spearman")
#La correlaci?n es del 93% con Spearman porque las variables no siguen una distrbuci?n normal
#Esta correlaci?n tan fuerte est? interfiriendo en el modelo. 
#Quitar la variable Petal.Width 


fitLM1<-lm(y ~ Sepal.Length + Petal.Length, data = train)

summary(fitLM1)
# Call:
#   lm(formula = y ~ Sepal.Length + Petal.Length, data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.61519 -0.17330  0.01859  0.15534  0.53081 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.92146    0.26913   3.424  0.00089 ***
#   Sepal.Length -0.13138    0.06094  -2.156  0.03344 *  
#   Petal.Length  0.48667    0.02831  17.190  < 2e-16 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2477 on 102 degrees of freedom
# Multiple R-squared:  0.9092,	Adjusted R-squared:  0.9074 
# F-statistic: 510.6 on 2 and 102 DF,  p-value: < 2.2e-16

#Ambos par?metros son significativos por lo que aportan al modelo
#El modelo describe el  90% de los datos por lo que la predicci?n debe ser buena

pred<-predict(fitLM1,newdata = test)
test$prediccionModeloAjustado<-round(pred,0)

cfm1<-confusionMatrix(test$y, test$prediccionModeloAjustado)
cfm1
#Accuracy : 0.9333 