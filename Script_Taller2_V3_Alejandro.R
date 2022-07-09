rm(list=ls()) ## Limpiar el entorno de trabajo

#Librerias-------------------------------------------------------------------------------
library(tidyverse)
require(pacman)
require(dplyr)

p_load(rio) # Librería para importar datos 
p_load(tidyverse) # Librería para limpiar datos
p_load(e1071) # Tiene la función para calcular skewness
p_load(EnvStats) # Transformación Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown

#Lectura bases de datos ----------------------------------------------------------------

getwd() #Establezco mi directorio
BD_Pru_Hog<-readRDS("test_hogares.Rds")
BD_Pru_Per<-readRDS("test_personas.Rds")
BD_Ent_Hog<-readRDS("train_hogares.Rds")
BD_Ent_Per<-readRDS("train_personas.Rds")

#Limpieza de datos ---------------------------------------------------------------------

#1. BD_Pru_Hog

#borrar variables q no se consideran útiles
BD_Pru_Hog_Lim<-BD_Pru_Hog %>% 
  select(-"Depto",-"P5100",-"P5130",-"P5140")

#Volver variables categoricas
BD_Pru_Hog_Lim <- BD_Pru_Hog_Lim %>%
  mutate_at(.vars = c("Clase","Dominio","P5090"),
            .funs = factor)

#Na´s

cantidad_na <- sapply(BD_Pru_Hog_Lim, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(BD_Pru_Hog_Lim)
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na)) # Ordenamos de mayor a menor
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")# Convertierte el nombre de la fila en columna

#2. BD_Ent_Hog

#borrar variables q no se consideran útiles
BD_Ent_Hog_Lim<-BD_Ent_Hog %>% 
  select(-"Depto",-"P5100",-"P5130",-"P5140")

#Volver variables categoricas
BD_Ent_Hog_Lim <- BD_Ent_Hog_Lim %>%
  mutate_at(.vars = c("Clase","Dominio","P5090","Pobre","Indigente"),
            .funs = factor)

#Na´s
cantidad_na <- sapply(BD_Ent_Hog_Lim, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(BD_Ent_Hog_Lim)
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na)) # Ordenamos de mayor a menor
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")# Convertierte el nombre de la fila en columna

#3. BD_Pru_Per

#borrar variables q no se consideran útiles
BD_Pru_Per_Lim<-BD_Pru_Per %>% 
  select(-"P6426",-"P6510",-"P6545",-"P6580",-"P6585s1",-"P6585s2",-"P6585s3",
         -"P6585s4",-"P6600",-"P6610",-"P6630s1",-"P6630s2",-"P6630s3",-"P6630s4",
         -"P6630s6",-"P6870",-"P7045",-"P7050",-"P7110",-"P7120",-"P7150",
         -"P7160",-"P7350",-"P7500s2",-"P7500s3",-"P7510s1",-"P7510s2",
         -"P7510s3",-"P7510s5",-"P7510s6",-"P7510s7",-"Fex_c",-"Depto",-"Fex_dpto",
         -"P7310",-"P6590",-"P6620",-"P7472",-"P7422",-"P6920",-"Oficio",-"P6430",
         -"P6800",-"P7040",-"P7090",-"P6100"
         )

#convierto na en ceros de Pet, Oc, Des, Ina
BD_Pru_Per_Lim <- mutate_at(BD_Pru_Per_Lim, c("Pet","Oc","Des","Ina"), ~replace(., is.na(.), 0))

##Asignar NA´s a cat 9
BD_Pru_Per_Lim$P6090 <-ifelse(is.na(BD_Pru_Per_Lim$P6090)==T,  9,BD_Pru_Per_Lim$P6090)

#borrar variable de grado - P6210s1, dado que ya tenemos escolaridad en P6210
BD_Pru_Per_Lim<-BD_Pru_Per_Lim %>% select(-"P6210s1")

#borrar variable de en que ocupo tiempo semana antr - P6240
BD_Pru_Per_Lim<-BD_Pru_Per_Lim %>% select(-"P6240")

#Variable ¿recibió pagos por concepto de arriendos y/o pensiones? (P7495), 

BD_Pru_Per_Lim<-BD_Pru_Per_Lim%>%subset(is.na(P7495)==F)   ##Borrar NA´s

#Durante los últimos doce meses, ¿recibió dinero de otros hogares,
#personas o instituciones no gubernamentales; dinero por intereses,
#dividendos, utilidades o por cesantias? (P7505)                          

BD_Pru_Per_Lim<-BD_Pru_Per_Lim%>%subset(is.na(P7505)==F)   ##Borrar NA´s

#P6210 asigno Na´s a cat 9
BD_Pru_Per_Lim$P6210 <-ifelse(is.na(BD_Pru_Per_Lim$P6210)==T,  9,BD_Pru_Per_Lim$P6210)

#Volver variables categoricas
BD_Pru_Per_Lim <- BD_Pru_Per_Lim %>%
  mutate_at(.vars = c("Clase","Dominio","Pet","Oc","Des","Ina","P6020","P6040","P6050",
                      "P6090","P6210"
                      ,"P7495","P7505"),
            .funs = factor)

#Na´s
cantidad_na <- sapply(BD_Pru_Per_Lim, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(BD_Pru_Per_Lim)
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na)) # Ordenamos de mayor a menor
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")# Convertierte el nombre de la fila en columna
write.csv(porcentaje_na, file="CantidadNa_BD_Pru_Per_LimV2.csv", row.names = FALSE)

#4. BD_Ent_Per

#borrar variables q no se consideran útiles
BD_Ent_Per_Lim<-BD_Ent_Per %>% 
  select(-"P6426",-"P6430",-"P6510",-"P6545",-"P6580",-"P6585s1",-"P6585s2",-"P6585s3",
         -"P6585s4",-"P6600",-"P6610",-"P6620",-"P6630s1",-"P6630s2",-"P6630s3",-"P6630s4",
         -"P6630s6",-"P6870",-"P7040",-"P7045",-"P7050",-"P7090",-"P7110",-"P7120",-"P7150",
         -"P7160",-"P7310",-"P7350",-"P7422",-"P7500s2",-"P7500s3",-"P7510s1",-"P7510s2",
         -"P7510s3",-"P7510s5",-"P7510s6",-"P7510s7",-"Fex_c",-"Depto",-"Fex_dpto",
         -"Estrato1",-"P6500",-"P6510s1",-"P6510s2",-"P6545s1",-"P6545s2",-"P6580s1",
         -"P6580s2",-"P6585s1a1",-"P6585s1a2",-"P6585s2a1",-"P6585s2a2",-"P6585s3a1"
         ,-"P6585s3a2",-"P6585s4a1",-"P6585s4a2",-"P6590s1",-"P6600s1",-"P6610s1",
         -"P6620s1",-"P6630s1a1",-"P6630s2a1",-"P6630s3a1",-"P6630s4a1",-"P6630s6a1"
         ,-"P6750",-"P6760",-"P550",-"P7070",-"P7140s1",-"P7140s2",-"P7422s1",
         -"P7472s1",-"P7500s1",-"P7500s1a1",-"P7500s2a1",-"P7500s3a1",-"P7510s1a1"
         ,-"P7510s2a1",-"P7510s3a1",-"P7510s5a1",-"P7510s6a1",-"P7510s7a1",
         -"Impa",-"Isa",-"Ie",-"Imdi",-"Iof1",-"Iof2",-"Iof3h",-"Iof3i",-"Iof6",-"Cclasnr2",
         -"Cclasnr3",-"Cclasnr4",-"Cclasnr5",-"Cclasnr6",-"Cclasnr7",-"Cclasnr8",-"Cclasnr11",
         -"Impaes",-"Isaes",-"Iees",-"Imdies",-"Iof1es",-"Iof2es",-"Iof3hes",-"Iof3ies",
         -"Iof6es",-"Ingtotob",-"Ingtotes",
         -"P6590",-"P7472",-"P6920",-"Oficio",-"P6800",-"P6210s1",-"P6240",-"P6100")

#convierto na en ceros de Pet, Oc, Des, Ina
BD_Ent_Per_Lim <- mutate_at(BD_Ent_Per_Lim, c("Pet","Oc","Des","Ina"), ~replace(., is.na(.), 0))

#Variable ¿recibió pagos por concepto de arriendos y/o pensiones? (P7495)

BD_Ent_Per_Lim<-BD_Ent_Per_Lim%>%subset(is.na(P7495)==F)   ##Borrar NA´s

#Durante los últimos doce meses, ¿recibió dinero de otros hogares,
#personas o instituciones no gubernamentales; dinero por intereses,
#dividendos, utilidades o por cesantias? (P7505)                    

BD_Ent_Per_Lim<-BD_Ent_Per_Lim%>%subset(is.na(P7505)==F)   ##Borrar NA´s

##Asignar NA´s a cat 9
BD_Ent_Per_Lim$P6090 <-ifelse(is.na(BD_Ent_Per_Lim$P6090)==T,  9,BD_Ent_Per_Lim$P6090)

#Volver variables categoricas
BD_Ent_Per_Lim <- BD_Ent_Per_Lim %>%
  mutate_at(.vars = c("Clase","Dominio","P6020","P6050","P6090",
                      "P7495","P6210","P7505","Pet","Oc","Des","Ina"),
            .funs = factor)

#Na´s
cantidad_na <- sapply(BD_Ent_Per_Lim, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(BD_Ent_Per_Lim)
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na)) # Ordenamos de mayor a menor
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")# Convertierte el nombre de la fila en columna
write.csv(porcentaje_na, file="CantidadNa_BD_Ent_Per_Lim.csv", row.names = FALSE)

#Unir bases de datos individuales a hogares ---------------------------------------------------------

# 1. Pretratamiento BD_Ent_Per_Lim

#p6020, sexo, no relevante
#p6040, edad, no relevante
#p6050, parentesco con jefe hogar, 1 Jefe Hogar

VectorAuxIngreso<- BD_Ent_Per_Lim %>% group_by(id) %>% 
  summarize(sum_Ingtot=sum(Ingtot))

dim(VectorAuxIngreso)

#p6090, cotizante-beneficiario seg social, 1 si, 2 no, 9 no sabe
table(BD_Ent_Per_Lim$P6090) # sirve
#p6210, Max educ. 1.Ninguno 2.Prescolar 3.Primaria 4.Secundaria 5.Media 6.Superior 9. No sabe
table(BD_Ent_Per_Lim$P6210) # sirve
#p7495, Recibio pago por arriendo y/o pension? 1. Si 2. No
table(BD_Ent_Per_Lim$P7495) # sirve
#p7505, Recibio dinero de otros hogares, instituciones, intereses, dividendos o uti censantias. 1.Si 2.No
table(BD_Ent_Per_Lim$P7505) # sirve

#con lo siguiente queda solo info del jefe hogar
BD_Ent_Per_Lim<-BD_Ent_Per_Lim%>%subset(P6050=="1")
dim(BD_Ent_Per_Lim)

#unir BD_Ent_Per_Lim con VectorAuxIngreso

BD_Ent_Per_Lim<-left_join(BD_Ent_Per_Lim,VectorAuxIngreso, by="id")
dim(BD_Ent_Per_Lim)

#borrar variables q ya no sirve
BD_Ent_Per_Lim<-BD_Ent_Per_Lim %>% 
  select(-"Ingtot",-"Clase",-"Dominio")

#2. Unir bases de datos de entrenamiento

dim(BD_Ent_Hog_Lim)
dim(BD_Ent_Per_Lim)
BD_Ent_Hog_Lim<-left_join(BD_Ent_Hog_Lim,BD_Ent_Per_Lim, by="id")
dim(BD_Ent_Hog_Lim)

# 3. Pretratamiento BD_Pru_Per_Lim

#p6020, sexo, no relevante
#p6040, edad, no relevante
#p6050, parentesco con jefe hogar, 1 Jefe Hogar

#p6090, cotizante-beneficiario seg social, 1 si, 2 no, 9 no sabe
table(BD_Pru_Per_Lim$P6090) # sirve
#p6210, Max educ. 1.Ninguno 2.Prescolar 3.Primaria 4.Secundaria 5.Media 6.Superior 9. No sabe
table(BD_Pru_Per_Lim$P6210) # sirve
#p7495, Recibio pago por arriendo y/o pension? 1. Si 2. No
table(BD_Pru_Per_Lim$P7495) # sirve
#p7505, Recibio dinero de otros hogares, instituciones, intereses, dividendos o uti censantias. 1.Si 2.No
table(BD_Pru_Per_Lim$P7505) # sirve

#con lo siguiente queda solo info del jefe hogar
BD_Pru_Per_Lim<-BD_Pru_Per_Lim%>%subset(P6050=="1")
dim(BD_Pru_Per_Lim)

#borrar variables q ya no sirve
BD_Pru_Per_Lim<-BD_Pru_Per_Lim %>% 
  select(-"Clase",-"Dominio")

#2. Unir bases de datos de entrenamiento y prueba 

dim(BD_Pru_Hog_Lim)
dim(BD_Pru_Per_Lim)




BD_Pru_Hog_Lim <- left_join(BD_Pru_Hog_Lim,BD_Pru_Per_Lim, by="id")


BD_Hog_Train_Lim_Final <-  left_join(BD_Pru_Hog_Lim,
                                   BD_Ent_Hog_Lim %>% dplyr::select(Ingtotug,Ingtotugarr,Ingpcug, Pobre,Indigente,Npobres,Nindigentes,id),
                                   by = "id")


BD_Hog_Test_Lim_Final <- select(BD_Ent_Hog_Lim, - sum_Ingtot)


BD_Hog_Train_Lim_Final$Ingtotug <- as.numeric(BD_Hog_Train_Lim_Final$Ingtotug) 
BD_Hog_Train_Lim_Final$Ingtotugarr <- as.numeric(BD_Hog_Train_Lim_Final$Ingtotugarr) 
BD_Hog_Train_Lim_Final$Ingpcug <- as.numeric(BD_Hog_Train_Lim_Final$Ingpcug) 


BD_Hog_Test_Lim_Final$Ingtotug <- as.numeric(BD_Hog_Test_Lim_Final$Ingtotug) 
BD_Hog_Test_Lim_Final$Ingtotugarr <- as.numeric(BD_Hog_Test_Lim_Final$Ingtotugarr) 
BD_Hog_Test_Lim_Final$Ingpcug <- as.numeric(BD_Hog_Test_Lim_Final$Ingpcug) 


dim(BD_Hog_Train_Lim_Final)
dim(BD_Hog_Test_Lim_Final)

summary(BD_Hog_Train_Lim_Final)
summary(BD_Hog_Test_Lim_Final)


#-------------------------------------------------------------------------------------------------

#Punto 5
#Punto 5.a
library(ISLR2)
#Selección muestra de entrenamiento y prueba
set.seed(10101)
#Modelos entrenamiento
model_1<-lm(Ingtotugarr~1,data = BD_Hog_Train_Lim_Final)
model_2<-lm(Ingtotugarr~Lp,data = BD_Hog_Train_Lim_Final)
model_3<-lm(Ingtotugarr~ Li,data = BD_Hog_Train_Lim_Final)
model_4<-lm(Ingtotugarr~Lp+Li,data = BD_Hog_Train_Lim_Final)
model_5<-lm(Ingtotugarr~Lp+P5000,data = BD_Hog_Train_Lim_Final)
model_6<-lm(Ingtotugarr~Lp+P5090,data = BD_Hog_Train_Lim_Final)
model_7<-lm(Ingtotugarr~poly(Lp, 2),data = BD_Hog_Train_Lim_Final)
model_8<-lm(Ingtotugarr~poly(Lp, 2)+P5000,data = BD_Hog_Train_Lim_Final)
model_9<-lm(Ingtotugarr~poly(Lp, 2)+P5000+P5090,data = BD_Hog_Train_Lim_Final)
#Modelos fuera de muestra
BD_test$model_1<-predict(model_1,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_2<-predict(model_2,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_3<-predict(model_3,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_4<-predict(model_4,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_5<-predict(model_5,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_6<-predict(model_6,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_7<-predict(model_7,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_8<-predict(model_8,newdata = BD_Hog_Test_Lim_Final)
BD_test$model_9<-predict(model_9,newdata = BD_Hog_Test_Lim_Final)
#MSE
mse01<-with(BD_test,mean((Ingtotugarr-model_1)^2))
mse02<-with(BD_test,mean((Ingtotugarr-model_2)^2))
mse03<-with(BD_test,mean((Ingtotugarr-model_3)^2))
mse04<-with(BD_test,mean((Ingtotugarr-model_4)^2))
mse05<-with(BD_test,mean((Ingtotugarr-model_5)^2))
mse06<-with(BD_test,mean((Ingtotugarr-model_6)^2))
mse07<-with(BD_test,mean((Ingtotugarr-model_7)^2))
mse08<-with(BD_test,mean((Ingtotugarr-model_8)^2))
mse09<-with(BD_test,mean((Ingtotugarr-model_9)^2))
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
  ylab("Ingreso con arriendo imputado")+
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
model_1CV<-train(Ingtotugarr~.,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")
model_1CV<-train(Ingtotugarr~1,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="null")
model_2CV<-train(Ingtotugarr~Lp,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_3CV<-train(Ingtotugarr~ Li,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_4CV<-train(Ingtotugarr~Lp+Li,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_5CV<-train(Ingtotugarr~Lp+P5000,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_6CV<-train(Ingtotugarr~Lp+P5090,
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_7CV<-train(Ingtotugarr~poly(Lp, 2),
                 data =  BD_Hog_Train_Lim_Final,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_8CV<-train(Ingtotugarr~poly(Lp, 2)+P5000,
                 data =  data_clean_ocu,
                 trControl=trainControl(method = "cv",number = 5),
                 method="lm")
model_9CV<-train(Ingtotugarr~poly(Lp, 2)+P5000+P5090,
                 data =  BD_Hog_Train_Lim_Final,
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
BD_Hog_Train_Lim_Final$MSE_LOOCV <- 1
for (i in 1:nrow(BD_Hog_Train_Lim_Final)) {
  #Establecer BD
  BD_train_LOOCV<-BD_Hog_Train_Lim_Final[-c(i),]
  dim(BD_train_LOOCV)
  BD_test_LOOCV<-BD_Hog_Train_Lim_Final[c(i),]
  dim(BD_test_LOOCV)
  #Modelo entrenamiento
  model_9LOOCV<-lm(LnIng~poly(exp, 2)+sex+age,data = BD_train_LOOCV)
  #Modelo fuera de muestra
  BD_test_LOOCV$model_9LOOCV<-predict(model_9LOOCV,newdata = BD_test_LOOCV)
  #MSE
  BD_Hog_Train_Lim_Final$MSE_LOOCV[i]<-with(BD_test_LOOCV,mean((LnIng-model_9LOOCV)^2))
}
mean(BD_Hog_Train_Lim_Final$MSE_LOOCV)





