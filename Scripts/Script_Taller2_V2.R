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
         -"P6800",-"P7040",-"P7090"
         )

#convierto na en ceros de Pet, Oc, Des, Ina
BD_Pru_Per_Lim <- mutate_at(BD_Pru_Per_Lim, c("Pet","Oc","Des","Ina"), ~replace(., is.na(.), 0))

##Asignar NA´s a cat 9
BD_Pru_Per_Lim$P6100 <-ifelse(is.na(BD_Pru_Per_Lim$P6100)==T,  9,BD_Pru_Per_Lim$P6100)
BD_Pru_Per_Lim$P6090 <-ifelse(is.na(BD_Pru_Per_Lim$P6090)==T,  9,BD_Pru_Per_Lim$P6090)

#borrar variable de grado - P6210s1, dado que ya tenemos escolaridad en P6210
BD_Pru_Per_Lim<-BD_Pru_Per_Lim %>% select(-"P6210s1")

#borrar variable de en que ocupo tiempo semana antr - P6240
BD_Pru_Per_Lim<-BD_Pru_Per_Lim %>% select(-"P6240")

#Variable ¿recibió pagos por concepto de arriendos y/o pensiones? (P7495), No solucionado @@@@

BD_Pru_Per_Lim<-BD_Pru_Per_Lim%>%subset(is.na(P7495)==F)   ##Borrar NA´s

#Durante los últimos doce meses, ¿recibió dinero de otros hogares,
#personas o instituciones no gubernamentales; dinero por intereses,
#dividendos, utilidades o por cesantias? (P7505)                           No solucionado @@@@

BD_Pru_Per_Lim<-BD_Pru_Per_Lim%>%subset(is.na(P7505)==F)   ##Borrar NA´s

#P6210 asigno Na´s a cat 9
BD_Pru_Per_Lim$P6210 <-ifelse(is.na(BD_Pru_Per_Lim$P6210)==T,  9,BD_Pru_Per_Lim$P6210)

#Volver variables categoricas
BD_Pru_Per_Lim <- BD_Pru_Per_Lim %>%
  mutate_at(.vars = c("Clase","Dominio","Pet","Oc","Des","Ina","P6020","P6040","P6050",
                      "P6090","P6100","P6210"
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
         -"P6590",-"P7472",-"P6920",-"Oficio",-"P6800",-"P6210s1",-"P6240")

#convierto na en ceros de Pet, Oc, Des, Ina
BD_Ent_Per_Lim <- mutate_at(BD_Ent_Per_Lim, c("Pet","Oc","Des","Ina"), ~replace(., is.na(.), 0))

#Variable ¿recibió pagos por concepto de arriendos y/o pensiones? (P7495), No solucionado @@@@

BD_Ent_Per_Lim<-BD_Ent_Per_Lim%>%subset(is.na(P7495)==F)   ##Borrar NA´s

#Durante los últimos doce meses, ¿recibió dinero de otros hogares,
#personas o instituciones no gubernamentales; dinero por intereses,
#dividendos, utilidades o por cesantias? (P7505)                           No solucionado @@@@

BD_Ent_Per_Lim<-BD_Ent_Per_Lim%>%subset(is.na(P7505)==F)   ##Borrar NA´s

##Asignar NA´s a cat 9
BD_Ent_Per_Lim$P6100 <-ifelse(is.na(BD_Ent_Per_Lim$P6100)==T,  9,BD_Ent_Per_Lim$P6100)
BD_Ent_Per_Lim$P6090 <-ifelse(is.na(BD_Ent_Per_Lim$P6090)==T,  9,BD_Ent_Per_Lim$P6090)

#Volver variables categoricas
BD_Ent_Per_Lim <- BD_Ent_Per_Lim %>%
  mutate_at(.vars = c("Clase","Dominio","P6020","P6050","P6090","P6100",
                      "P7495","P6210","P7505","Pet","Oc","Des","Ina"),
            .funs = factor)

#Na´s
cantidad_na <- sapply(BD_Ent_Per_Lim, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(BD_Ent_Per_Lim)
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na)) # Ordenamos de mayor a menor
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")# Convertierte el nombre de la fila en columna
write.csv(porcentaje_na, file="CantidadNa_BD_Ent_Per_Lim.csv", row.names = FALSE)

#Tratamiento bases de datos individuales a hogares ---------------------------------------------------------

BD_Ent_Hog_Lim
BD_Ent_Per_Lim
colnames(BD_Ent_Hog_Lim)
colnames(BD_Ent_Per_Lim)

#p6020, sexo, no relevante
#p6040, edad, no relevante
#p6050, parentesco con jefe hogar, 1 Jefe Hogar
#p6090, cotizante-beneficiario seg social, 1 si, 2 no, 9 no sabe
table(BD_Ent_Per_Lim$P6090) # sirve


BD_Ent_Per_Lim_Prueba<- BD_Ent_Per_Lim %>% group_by(id) %>% 
  summarize(sum(Ingtot))
dim(BD_Ent_Per_Lim_Prueba)


#-------------------------------------------------------------------------------------------------

## recategorizar variable-
db = train_hogares %>% 
  mutate(Pobre=ifelse(Pobre==1,"pobre (1)","no pobre (0)") %>% as.factor())

str(db)

## fijar semilla
set.seed(210422)

## generar observaciones aleatorias
test <- sample(x=1:32, size=10)

## reescalar variables (para calcular distancias)
x <- scale(db[,-9]) 
apply(x,2,sd) ## verificar

## k-vecinos
k1 = knn(train=x[-test,], ## base de entrenamiento
         test=x[test,],   ## base de testeo
         cl=db$am[-test], ## outcome
         k=1)        ## vecinos 

tibble(db$am[test],k1)

## matriz de confusión
confusionMatrix(data=k1 , 
                reference=db$am[test] , 
                mode="sens_spec" , 
                positive="manual (1)")

cm = confusionMatrix(data=k1 , reference=db$am[test], positive="pobre (1)")$table
cm

## obtener los valores manualmente 
(cm[1,1]+cm[2,2])/sum(cm) ## Accuracy
cm[2,2]/sum(cm[,2]) ## Sensitivity
cm[1,1]/sum(cm[,1]) ## Specificity
cm[2,1]/sum(cm[2,]) ## Ratio Falsos Positivos
cm[1,2]/sum(cm[1,]) ## Ratio Falsos Negativos

##=== 2. Regresión: Logit y Probit ===##

## obtener datos
geih <- import("https://eduard-martinez.github.io/teaching/meca-4107/geih.rds")
head(geih)



## modelo a ajustar
model <- as.formula("ocu ~ age + sex + factor(maxEducLevel)")

## estimación logit
logit <- glm(model , family=binomial(link="logit") , data=geih)
tidy(logit)

## estimación probit
probit <- glm(model , family=binomial(link="probit") , data=geih)
tidy(probit)

## ratio de los coeficientes
logit$coefficients / probit$coefficients

## preddicción
geih$ocu_log = predict(logit , newdata=geih , type="response")
geih$ocu_prob = predict(probit , newdata=geih , type="response")
head(geih)

## definir la regla
rule=0.7
geih$ocu_prob = ifelse(geih$ocu_prob>rule,1,0)
geih$ocu_log = ifelse(geih$ocu_log>rule,1,0)
head(geih)





