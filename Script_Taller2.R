rm(list=ls()) 
library(tidyverse)
library(mosaic)
library(recipes)
library(openxlsx)
require("fabricatr")
require("stargazer") 
require("boot")
require("fastDummies")
require("dplyr")
library(pacman)
library(lfe)


## Data Cleaning

train_hogares <- read.csv("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\ProblemSet2\\train_hogares.csv",header=TRUE, sep="," )
train_personas <- read.csv("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\ProblemSet2\\train_personas.csv",header=TRUE, sep="," )
test_hogares <- read.csv("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\ProblemSet2\\test_hogares.csv",header=TRUE, sep="," )
test_personas <- read.csv("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\ProblemSet2\\test_personas.csv",header=TRUE, sep="," )


#Punto 5
#Punto 5.a
library(ISLR2)
#Selección muestra de entrenamiento y prueba
set.seed(10101)
id_train <- sample(1:nrow(train_hogares),size = 0.7*nrow(train_hogares), replace = F)
BD_train<-train_hogares[id_train,]
BD_test<-train_hogares[-id_train,]
#Modelos entrenamiento
model_1<-lm(LnIng~1,data = BD_train)
model_2<-lm(LnIng~age,data = BD_train)
model_3<-lm(LnIng~exp,data = BD_train)
model_4<-lm(LnIng~age+exp,data = BD_train)
model_5<-lm(LnIng~age+agesqr,data = BD_train)
model_6<-lm(LnIng~exp+sex,data = BD_train)
model_7<-lm(LnIng~poly(exp, 2),data = BD_train)
model_8<-lm(LnIng~poly(exp, 2)+sex,data = BD_train)
model_9<-lm(LnIng~poly(exp, 2)+sex+age,data = BD_train)
#Modelos fuera de muestra
BD_test$model_1<-predict(model_1,newdata = BD_test)
BD_test$model_2<-predict(model_2,newdata = BD_test)
BD_test$model_3<-predict(model_3,newdata = BD_test)
BD_test$model_4<-predict(model_4,newdata = BD_test)
BD_test$model_5<-predict(model_5,newdata = BD_test)
BD_test$model_6<-predict(model_6,newdata = BD_test)
BD_test$model_7<-predict(model_7,newdata = BD_test)
BD_test$model_8<-predict(model_8,newdata = BD_test)
BD_test$model_9<-predict(model_9,newdata = BD_test)
#MSE
mse01<-with(BD_test,mean((LnIng-model_1)^2))
mse02<-with(BD_test,mean((LnIng-model_2)^2))
mse03<-with(BD_test,mean((LnIng-model_3)^2))
mse04<-with(BD_test,mean((LnIng-model_4)^2))
mse05<-with(BD_test,mean((LnIng-model_5)^2))
mse06<-with(BD_test,mean((LnIng-model_6)^2))
mse07<-with(BD_test,mean((LnIng-model_7)^2))
mse08<-with(BD_test,mean((LnIng-model_8)^2))
mse09<-with(BD_test,mean((LnIng-model_9)^2))
#Grafica MSE
vmse1<-c(mse01,mse02,mse03,mse04,mse05,mse06,mse07,mse08,mse09)
graf2<-ggplot(mapping = aes(x=1:9, y=vmse1))+
  geom_line(color="blue")+
  xlab("Modelos")+
  ylab("MSE")+
  ggtitle("Resumen MSE")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1,9,1))
graf2

#Numeral a.V:
ujs<-c()
hjs<-c()
alphas <- c()
for (j in 1:nrow(BD_test)) {
  uj <- model_9$residual[j]
  hj <- lm.influence(model_9)$hat[j]
  alpha <- uj/(1-hj)
  alphas <- c(alphas, alpha)
  ujs <- c(ujs, uj)
  hjs <- c(hjs, hj)
}
#BD para analizar leverage
BD_Leverage<-cbind(BD_test,alphas)
y_out_test<-predict(model_9,BD_Leverage)
y_real_test<-BD_Leverage$LnIng
ggplot(BD_Leverage, aes(x=1:4052))+
  xlab("Observaciones")+
  geom_point(aes(y=y_real_test, color="red"))+
  ylab("Log Ingreso")+
  geom_point(aes(y=y_out_test), color="blue")+
  theme_classic()+
  ggtitle("Ingreso real vs predicho")+
  theme(plot.title = element_text(hjust = 0.5))




alpha
alphas
ggplot(BD_test, aes(x=alphas, y=LnIng))+
  geom_point(color="red")+
  theme_classic()+
  ggtitle("Leverage Stadistic Modelo 9")+
  theme(plot.title = element_text(hjust = 0.5))

#Punto 5b. K-fold cross-validation.
install.packages("caret")
library(dplyr)
library(caret)
#modelos
model_1CV<-train(LnIng~.,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")
model_1CV<-train(LnIng~1,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")
model_2CV<-train(LnIng~age,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_3CV<-train(LnIng~exp,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_4CV<-train(LnIng~age+exp,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_5CV<-train(LnIng~age+agesqr+Escol,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_6CV<-train(LnIng~exp+sex,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_7CV<-train(LnIng~poly(exp, 2),
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_8CV<-train(LnIng~poly(exp, 2)+sex,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_9CV<-train(LnIng~poly(exp, 2)+sex+age,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_2CV
model_3CV
model_4CV
model_5CV
model_6CV
model_7CV
model_8CV
model_9CV

#Numeral c. LOOCV
data_clean_ocu$MSE_LOOCV <- 1
for (i in 1:nrow(data_clean_ocu)) {
  #Establecer BD
  BD_train_LOOCV<-data_clean_ocu[-c(i),]
  dim(BD_train_LOOCV)
  BD_test_LOOCV<-data_clean_ocu[c(i),]
  dim(BD_test_LOOCV)
  #Modelo entrenamiento
  model_9LOOCV<-lm(LnIng~poly(exp, 2)+sex+age,data = BD_train_LOOCV)
  #Modelo fuera de muestra
  BD_test_LOOCV$model_9LOOCV<-predict(model_9LOOCV,newdata = BD_test_LOOCV)
  #MSE
  data_clean_ocu$MSE_LOOCV[i]<-with(BD_test_LOOCV,mean((LnIng-model_9LOOCV)^2))
}
mean(data_clean_ocu$MSE_LOOCV)

















