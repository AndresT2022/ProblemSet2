ProblemSet2 - Grupo 2

Introducción

El propósito de este trabajo es construir un modelo predictivo de la pobreza en los hogares colombianos en base a la GEIH de 2018, para ello, se emplearon dos formas de predecir la pobreza, el primero a nivel de clasificación, pobre o no pobre mediante un modelo logit, y el segundo utilizando la predicción de los ingresos de los hogares y el umbral de pobreza, se determinó la clasificación de pobre y no pobre.

Datos

Respecto a las bases de datos, inicialmente fueron suministradas 4 bases de datos, una de entrenamiento a nivel hogar y una a nivel individual, y una de pruebas a nivel hogar y una a nivel individual.Luego de limpiar las bases de datos (ver apéndice), se obtuvieron la mayor cantidad de variables posibles sin missing values, se unió la información individual a las bases de datos hogares. Es importante mencionar que, en la base de datos de individuos, se tomó como referencia las observaciones del jefe de hogar como representación del hogar, y así utilizar esta información para unirla con la base de datos de hogar.

Desarrollo

Se utilizan las distintas metodologías de clasificación siguiendo los pasos que permitan evaluar estas muestras en base a la categorización de las distintas variables para observar efectos de control sobre variables que no son lineales o numéricas. Los resultados nos muestran que el mejor modelo está dado por la metodología de Logit.

Conclusión

Con la modelación se puede concluir que las mediciones del DANE cumplen su propósito de dotar de una base para logar. Una predicción de la situación de pobreza, al analizar por ingreso se puede comprobar que las variables que lo explican en nuestro caso, permiten también una clasificación acorde a lo que indica la proporción inicial de 20% de personas clasificadas como pobres. Sin embargo, también es preciso señalar que se encontró un riesgo de error tipo II, es decir clasificar personas no pobres como pobres. Inferimos que probablemente falta alguna variable más robusta de control que se encuentran dentro de los errores estocásticos.
