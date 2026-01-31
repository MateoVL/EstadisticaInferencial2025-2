library(EnvStats)
library(ggpubr)
library(tidyr)



datos <- datos_felicidad
n <- 70
set.seed(29)
datos_muestra <- datos[sample(nrow(datos), n, replace = FALSE),]

felicidad <- datos_muestra$Felicidad
n_amigos <- datos_muestra$N_amigos
n_mascotas <- datos_muestra$N_mascotas
edad <- datos_muestra$Edad
sexo <- datos_muestra$Sexo

# Como predictores utilizamos la cant de amigos y la cant de mascotas, ya que no dependen de los demas datos
# La variable de respuesta puede ser la felicidad, es la variable que podriamos estudiar y ver si depende de otros factores.

# Miremos histogramas
df1 <- data.frame(n_amigos, n_mascotas, edad, felicidad)
df1_l <- pivot_longer(df1, cols = everything(),
                      names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable))
ph1 <- gghistogram(df1_l, x = "valor", bins = 9, fill = "variable")
ph1 <- ph1 + facet_grid(~ variable, scales = "free_x")
ph1

# ver correlaciones
print(cor(df1))


# Hay mas correlacion entre la cantidad de mascotas que la cant de amigos para la felicidad, por lo que
# Estudiaremos si la cantidad de mascotas influye en el nivel de felicidad de las personas.
# La cant de mascotas logra predecir la felicidad?

# Veamos si hay una relación lineal entre estas variables.
ps1 <- ggscatter(df1, x = "n_mascotas", y = "felicidad",
                 add = "reg.line",
                 conf.int = TRUE)
ps1

# Obtengamos la regresión lineal y analicemos los resultados
modelo1 <- lm(felicidad ~ n_mascotas, df1)
cat("\n\n")
cat("Modelo directo 'horas de estudio' --> 'nota'\n")
cat("--------------------------------------------\n")
summary(modelo1)


# Para esto creamos dos modelos "jerárquicos"
modelo2a <- lm(felicidad ~ n_mascotas, df1)
modelo2b <- lm(felicidad ~ n_mascotas + edad, df1)
modelo2c <- lm(felicidad ~ n_mascotas + edad + n_amigos, df1)
modelo_nulo <- lm(felicidad ~ 1, df1)

# Veamos el primer modelo
cat("\n\n")
cat("Modelo inicial 'edad' --> 'nota'\n")
cat("--------------------------------\n")
summary(modelo2a)

# Vemos que, como es esperado, detectamos una fuerte relación
# Veamos el segundo modelo
cat("\n\n")
cat("Modelo inicial + efecto de las 'horas de estudio'\n")
cat("-------------------------------------------------\n")
summary(modelo2b)


# Veamos el segundo modelo
cat("\n\n")
cat("Modelo inicial + efecto de las 'horas de estudio'\n")
cat("-------------------------------------------------\n")
summary(modelo2c)

cat("\n\n")
cat("Modelo inicial + efecto de las 'horas de estudio'\n")
cat("-------------------------------------------------\n")
summary(modelo_nulo)



# vemos que las mascotas son un buen predictor.






# Inventemos datos
set.seed(29)
edad <- rtri(n, min = 18, max = 30, mode = 22)
edad <- round(edad) # edad de estudiantes de cálculo 3

# Las horas de estudio invertidas y la nota en la primera prueba
# dependen de la edad del o de la estudiante.
h_est <- round(1 + 0.25 * edad + rnorm(n, mean = 2/3, sd = 1), 1)
nota <- round(0.25 + 0.17 * edad  + rnorm(n, mean = 0, sd = 0.5), 1)


# Miremos histogramas
df1 <- data.frame(edad, h_est, nota)
df1_l <- pivot_longer(df1, cols = everything(),
                      names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable))
ph1 <- gghistogram(df1_l, x = "valor", bins = 9, fill = "variable")
ph1 <- ph1 + facet_grid(~ variable, scales = "free_x")
ph1

# Miremos correlaciones
print(cor(df1))

# Obviamente, nuestro interés es saber si las horas de estudio permiten
# predecir la nota en la prueba.

# Veamos si hay una relación lineal entre estas variables.
ps1 <- ggscatter(df1, x = "edad", y = "nota",
                 add = "reg.line",
                 conf.int = TRUE)
ps1

# Obtengamos la regresión lineal y analicemos los resultados
modelo1 <- lm(nota ~ h_est, df1)
cat("\n\n")
cat("Modelo directo 'horas de estudio' --> 'nota'\n")
cat("--------------------------------------------\n")
summary(modelo1)


# Pero podríamos sospechar que la edad puede tener también cierta
# influencia en la nota que obtiene un/a estudiante en la prueba.
# Así, en jerga de los modelos de regresión, queremos evaluar el
# predictor 'horas de estudio' en la 'nota' de la prueba 'controlando'
# (o descartando) el efecto de la 'edad'.

# Para esto creamos dos modelos "jerárquicos"
modelo2a <- lm(nota ~ n_mascotas, df1)
modelo2b <- lm(nota ~ n_mascotas + n_amigos, df1)

# Veamos el primer modelo
cat("\n\n")
cat("Modelo inicial 'edad' --> 'nota'\n")
cat("--------------------------------\n")
summary(modelo2a)

# Vemos que, como es esperado, detectamos una fuerte relación
# Veamos el segundo modelo
cat("\n\n")
cat("Modelo inicial + efecto de las 'horas de estudio'\n")
cat("-------------------------------------------------\n")
summary(modelo2b)

# Vemos que ahora sí detectamos que en realidad las horas de
# estudio no es tan buen predictor, sino que la edad del o de la
# estudiante predice mejor (pues es la 'causa' detrás de esa nota).


# Pero veamos un caso, probablemente, más parecido a la realidad:
# la nota en la primera prueba de cálculo 3 depende principalmente de
# las horas de estudio que invierte y de la madurez de sus métodos de
# estudio. Esto último depende de cuánto tiempo lleva estudiando
# "en serio", que pudo haber partido en el liceo o al entrar a la
# universidad (¡o al año siguiente!), pero que necesariamente se
# relaciona con la edad del o de la estudiante.
nota2 <- round(0.75 + 0.3 * h_est + 0.05 * edad  + rnorm(n, mean = 0, sd = 0.5), 1)

# Miremos histogramas
df2 <- data.frame(edad, h_est, nota2)
df2_l <- pivot_longer(df2, cols = everything(),
                      names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable))
ph2 <- gghistogram(df2_l, x = "valor", bins = 9, fill = "variable")
ph2 <- ph2 + facet_grid(~ variable, scales = "free_x")
ph2

# Revisemos los modelos "jerárquicos". Incluyamos ahora en el análisis
# al modelo nulo (solo intercepto = la media de las notas).
modelo30 <- lm(nota2 ~ 1, df2)
modelo3a <- lm(nota2 ~ edad, df2)
modelo3b <- lm(nota2 ~ edad + h_est, df2)

# Veamos el modelo nulo
cat("\n\n")
cat("Modelo nulo\n")
cat("-----------\n")
summary(modelo30)

# Observamos que usar la media (4,022) reporta un error estándar de 0,0662.
# Esto se traduce en una suma de cuadrados:
ss0a <- summary(modelo30)$sigma^2 * 99  # SE^2 * grados de libertad
ss0b <- deviance(modelo30)  # Devianza o desviación en español 
ss0c <- sum(resid(modelo30)^2)  # por definición: suma de los cuadrados de las desviaciones
cat("\n")
cat("SS modelo nulo:", ss0b, "\n")

# Veamos el modelo que predice con la edad
cat("\n\n")
cat("Modelo con 'edad' como predictor\n")
cat("--------------------------------\n")
summary(modelo3a)

# Vemos que el modelo permite predecir mejor la nota de la prueba,
# pues hay un mejor ajuste a los datos = una reducción de la desviación
# (y en consecuencia, del error estándar).
cat("\n")
cat("SS modelo con edad:", deviance(modelo3a), "\n")

# Y esta reducción es significativa:
cat("\n")
cat("Modelo nulo vs. modelo con 'edad' como predictor\n")
cat("------------------------------------------------\n")
anova(modelo30, modelo3a)

# Veamos que esta es la información que nos entrega el 'summary()'
# del modelo respecto a su bondad de ajuste (no tenemos que
# calcularlo a mano).

# Revisemos qué se nos dice de los predictores usados:
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.50143    0.62463   0.803    0.424
# edad         0.15585    0.02753   5.660  1.5e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’

# Primero, recordar que los coeficientes que se reportan (b_i) son
# estimaciones (de ahí el título 'Estimate') de los parámetros de la
# regresión lineal en la población (beta_i). Estos representan cuánto
# cambia la media de la variable dependiente (nota) cuando el predictor
# cambia en una unidad (edad = edad + 1 año), manteniendo sin cambio
# cualquier otra variable en el modelo.
# En este caso, por cada año de edad, la nota sube 0,156 puntos.

# Segundo, recordar que esta estimaciones son válidas (insesgadas) y los
# p valores confiables solo si se cumplen las condiciones expuestas en
# la lectura. No lo haremos aquí, ¡pero siempre debe hacerse en una
# respuesta de ustedes!

# Tercero, recordemos que los coeficientes obtenidos se usan para predecir
# la nota que obtendrá un/a estudiante. La fórmula sería:
#   nota_est = 0,5014 + 0,1559 * edad
# o, para evitar redondeos (si así se desea):
#   nota_est = coef(modelo3a)[1] + coef(modelo3a)[2] * edad

# Si tenemos dos estudiantes nuevos, de 21 y 31 años, respectivamente,
# podemos estimar sus notas:
df_nuevos <- data.frame(edad = c(21, 31))
pred1a <- coef(modelo3a)[1] + coef(modelo3a)[2] * df_nuevos[["edad"]]

# Pero no necesitamos hacerlo a mano, pues podemos usar la función
# 'predict()':
pred1b <- predict(modelo3a, df_nuevos)

# Comparemos:
cat("\n\n")
cat("Usando el modelo con 'edad' como predictor\n")
cat("------------------------------------------\n")
cbind(df_nuevos, manual.1 = pred1a, predict.1 = pred1b)

# Cuarto, el modelo encontró que la variable 'edad' es significativa para
# lograr la mejor bondad de ajuste del modelo (p < 0,001), mientras
# que el intercepto no ayuda tanto (p = 0,424).

# Veamos qué pasa con el modelo completo
cat("\n\n")
cat("Modelo con efecto de las 'horas de estudio'\n")
cat("-------------------------------------------\n")
print(summary(modelo3b))

# Vemos que este modelo también consigue una bondad de ajuste
# significativamente mejor que el modelo nulo: F(2, 97)=34,35
# y p < 0,001 (5,256 10^-12).
# Pero, ¿será mejor que el anterior? Aprovechemos de comparar
# los tres modelos:
cat("\n\n")
cat("Comparación de los tres modelos\n")
cat("-------------------------------\n")
print(anova(modelo30, modelo3a, modelo3b))

# Confirmamos que el modelo con dos predictores tiene mejor bondad
# de ajuste que el que usa un solo predictor.

# Revisemos qué se nos dice este modelo de los predictores:
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.37867    0.55383   0.684    0.496    
# edad         0.07149    0.02916   2.452    0.016 *  
# h_est        0.27562    0.05220   5.280 7.91e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’

# Primero, debemos notar que el coeficiente que teníamos para
# la variable 'edad' se ha reducido (0,07149 < 0,15585), al igual
# que su importancia (t = 2,45 < 5,66), aunque sigue siendo
# significativa para el modelo (p = 0,016).

# También, por cada hora de estudio que invierte un/a estudiante,
# en promedio la nota aumenta en 0,276 puntos, y es significativa
# para el modelo (t = 5,28; p < 0,001) y más importante que la
# 'edad' (t_edad = 2,45 < t_h_est = 5,28).

# Veamos cómo resultan las nuevas predicciones con este mejor modelo
df_nuevos <- data.frame(edad = c(21, 31), h_est = c(8, 7))
pred2 <- predict(modelo3b, df_nuevos)

cat("\n\n")
cat("Predicciones con el modelo con dos predictores\n")
cat("----------------------------------------------\n")
print(cbind(df_nuevos, predict.1 = pred1b, predict.2 = pred2))


# En resumen, no solo debemos considerar los predictores que creemos
# tienen relación con la variable dependiente. Debemos "controlar" otras
# variables que puedan tener algún efecto, para no atribuirlo erróneamente
# al conjunto de predictores considerados.

#
# Finalmente, miremos un gráfico de dispersión con los tres modelos.
#

# Primero obtenemos las predicciones hechas por cada modelo.
m30 <- predict(modelo30, df2)
m3a <- predict(modelo3a, df2)
m3b <- predict(modelo3b, df2)

# Para poder llevar todo a un solo gráfico, usamos una proyección
# del efecto que combina edad y horas de estudio.
efecto <- df2[["edad"]] * df2[["h_est"]]
n <- length(efecto)

# Juntamos todo esto en una matriz de datos
df3 <- data.frame(nota = df2[["nota2"]], m30, m3a, m3b)

# Ahora ordenamos estos datos según el efecto, y agregamos valores
# secuenciales para el eje X.
df3 <- cbind(x = 1:n, df3[order(efecto), ])

# Capturamos las posiciones de las menores y mayores predicciones,
# y las almacenamos en otra matriz de datos indicando el modelo al
# que pertenecen.
x_ini <- c(1, 1, 1)
y_ini <- c(df3[1, "m30"], df3[1, "m3a"], df3[1, "m3b"])
x_fin <- c(n, n, n)
y_fin <- c(df3[n, "m30"], df3[n, "m3a"], df3[n, "m3b"])

df4 <- data.frame(x_ini, y_ini, x_fin, y_fin, modelo = factor(c("0", "a", "b")))
# modelo = c("Nulo", "Edad", "Edad + horas de estudio"))

# Creamos el gráfico de dispersión.
ps3 <- ggscatter(df3, x = "x", y = "nota",
                 color = "steelblue", fill = "steelblue",
                 title = "Representación de los tres modelos obtenidos",
                 xlab = "Instancia (ordenada por efecto creciente)",
                 ylab = "Nota")

# Agregamos segmentos que representan las predicciones de cada modelo.
ps3 <- ps3 + geom_segment(aes(x = x_ini, y = y_ini, xend = x_fin, yend = y_fin,
                              colour = modelo, linetype = modelo), linewidth = 1,
                          data = df4)
ps3 <- ps3 + scale_color_manual(
  labels = c("0" = "Nulo", "a" = "Edad", "b" = "Edad + horas de estudio"),
  values = c("0" = "steelblue", "a" = "steelblue1", "b" = "steelblue4"))
ps3 <- ps3 + scale_linetype_manual(
  labels = c("0" = "Nulo", "a" = "Edad", "b" = "Edad + horas de estudio"),
  values = c("0" = "dotted", "a" = "solid", "b" = "solid"))

# Y lo mostramos.
print(ps3)