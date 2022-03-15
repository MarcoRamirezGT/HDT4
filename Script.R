library(ModelMetrics)


porcentaje<-0.7
set.seed(123)

#Calculo de percentiles
data<-read.csv('train.csv')
percentil <- quantile(data$SalePrice)


#Percentiles
estado<-c('Estado')
data$Estado<-estado


#Economica=0
#Intermedia=1
#Cara=2
data <- within(data, Estado[SalePrice<=129975] <- 0)

data$Estado[(data$SalePrice>129975 & data$SalePrice<=163000)] <- 1
data$Estado[data$SalePrice>163000] <- 2


#Regresion
corte <- sample(nrow(data),nrow(data)*porcentaje)
train<-data[corte,]
test<-data[-corte,]


#Regresion lineal
fitLMPW<-lm(SalePrice~ ., data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])

predL<-predict(fitLMPW, newdata = test)


#Verificando la predicci?n
resultados<-data.frame(test$SalePrice,predL)
head(resultados, n=5)



#An?lisis de residuos
# Los residuos se calcula restando la predicci?n de la variable respuesta
residuales <- test$SalePrice-predL
residuales
#No obstante el modelo ya lo calcula por lo que se pueden usar
# Si se mira el resumen del modelo podemos analizar el comportamiento de los residuos.
summary(fitLMPW)


rmse(test$SalePrice,predL)
plot(test$SalePrice, test$LotArea)
points(predL, test$LotArea, col="red",pch=15)

par(mfrow = c(2,2))
plot(fitLMPW)



#Predecir el precio de la casa en base a los datos.
fitLMSpByPL<-lm(SalePrice~ ., data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
summary(fitLMSpByPL)
# Multiple R-squared:  0.905,	Adjusted R-squared:  0.9041

#El modelo explica los datos en un 90% la predicci?n debe ser buena

predMSpByPL<-predict(fitLMSpByPL,newdata = test)
resultados1<-data.frame(test$SalePrice,round(predMSpByPL,0))
names(resultados1)<-c("real","prediccion")


confusionMatrix(resultados1$real,resultados1$prediccion)
#Accuracy : 0.8889 

#-------------------------------------------------
# Regresi?n Lineal M?ltiple 
#-------------------------------------------------

# fitLM<-lm(y~. ,data = train)
fitLM<-lm(SalePrice~.,data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])

summary(fitLM)
#El modelo se ajusta perfectamente a los datos
#Multiple R-squared:      1,	Adjusted R-squared:      1
#Advertencia que pone R:
# Warning message:
#   In summary.lm(fitLM) : essentially perfect fit: summary may be unreliable


predicted<-predict(fitLM,newdata = test)

test$prediccion <- predicted

cfm<-confusionMatrix(test$SalePrice,test$prediccion)
cfm
