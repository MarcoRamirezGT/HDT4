library(ModelMetrics)
library(ggplot2)

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


#Verificando la predicci?n
resultados<-data.frame(test$SalePrice,predL)
head(resultados, n=5)


#Multicolinealidad y correlación de las variables del modelo
pairs(data$SalePrice ~ data$GrLivArea)
pairs(data$SalePrice ~ data$YearBuilt)
pairs(data$SalePrice ~ data$BsmtUnfSF)
pairs(data$SalePrice ~ data$TotalBsmtSF)
pairs(data$SalePrice ~ data$GarageArea)
pairs(data$SalePrice ~ data$YearRemodAdd)
pairs(data$SalePrice ~ data$LotArea)


cor(data$SalePrice,data$GrLivArea)
#Regresion lineal
fitLMPW<-lm(SalePrice~ ., data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])

predL<-predict(fitLMPW, newdata = test)
dev.off()
ggplot(data=train,mapping = aes(x=SalePrice,y=GrLivArea ))+
  geom_point(color='red',size=2)+
  geom_smooth(method = 'lm',se=TRUE,color='black')+
  labs(title = 'Precio de venta ~ Pies cuadrados de vivienda',x="Precio de venta",y='Pies cuadrados de venta')+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))


head(fitLMPW$residuals)
residuales <- test$SalePrice-predL
residuales

summary(fitLMPW)



plot(test$SalePrice, test$LotArea)
points(predL, test$LotArea, col="red",pch=15)

par(mfrow = c(2,2))
plot(fitLMPW)



replace_outliers <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  caps <- quantile(x, probs = c(.05, .95), na.rm = removeNA)
  iqr <- qrts[2]-qrts[1]
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}

capped_pressure_height <- replace_outliers(fitLMPW$residuals)
par(mfrow = c(1,2))
boxplot(fitLMPW$residuals, main = "Presión con outliers"
        ,col=5)
boxplot(capped_pressure_height, main = "Presión sin outliers",col=6)

library(nortest)
lillie.test(capped_pressure_height)
hist(capped_pressure_height)

View(capped_pressure_height)

fitLMSpByPL<-lm(SalePrice~ ., data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
summary(fitLMSpByPL)

predLM<-predict(fitLMPW,newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])




predMSpByPL<-predict(fitLMSpByPL,newdata = test)
resultados1<-data.frame(test$SalePrice,round(predMSpByPL,0))
names(resultados1)<-c("real","prediccion")


confusionMatrix(resultados1$real,resultados1$prediccion)

fitLM<-lm(SalePrice~.,data = train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])

summary(fitLM)



predicted<-predict(fitLM,newdata = test)

test$prediccion <- predicted

cfm<-confusionMatrix(test$SalePrice,test$prediccion)
cfm


