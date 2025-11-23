

#################################################################################
#################################################################################
# 0. SETUP
#################################################################################
#################################################################################


# -------------------------------------------------------------------------------
# 0.1 LIBRERÍAS
# -------------------------------------------------------------------------------

# install.packages("readxl")
# install.packages("caret")
# install.packages("stats")
# install.packages("ggplot2")

library(readxl)   # para leer archivos Excel
library(stats)    # para PCA y regresión lineal
library(caret)    # para crear particiones estratificadas

library(tidyverse)  # para manipulación y visualización de datos
library(dplyr)      # para manipulación de datos
library(ggplot2)    # para visualización de datos


# ------------------------------------------------------------------------------
# 0.2 CARGA DE DATOS
# ------------------------------------------------------------------------------

# Directorio de trabajo
setwd("C:/Users/velir/OneDrive - Universidad Alfonso X el Sabio/1. UAX UNIVERSIDAD/2. MASTER Inteligencia Artificial/01_Cuatri_MUIA/MUIA_1c Matematicas y Estadistica para la IA/ENTREGA_01")

# Cargamos el dataset
df <- read.csv("dataset.csv")

cat("Dimensiones del dataset:\n", nrow(df), "filas\n", ncol(df), "columnas\n\n")


# ------------------------------------------------------------------------------
# 0.3 VARIABLE OBJETIVO Y PREDICTORAS
# ------------------------------------------------------------------------------

# variable objetivo es SalePrice
y <- df$SalePrice

# el resto son variables predictoras (salvo Id y SalePrice)
X <- df %>%
  select(-Id, -SalePrice)  # eliminar columnas no predictoras

cat("Dimensiones del dataset:\n", nrow(df), "filas\n", "1 variable objetivo\n", ncol(X), "variables predictoras\n\n")



#################################################################################
#################################################################################
# 1. EDA (Análisis Exploratorio de Datos) con todos los datos
#################################################################################
#################################################################################

# hacer una copia del dataset original
df1 <- df

# eliminar columna Id ya que no es una variable que aporte información
df1 <- df1 %>%
  select(-Id)  

# ------------------------------------------------------------------------------
# 1.1 TIPOS VARIABLES
# ------------------------------------------------------------------------------

# ver estructura general del datase (variables, tipos de datos, primeros registros)
glimpse(df1)

# ver cuántos tipos de variables hay
type_tab <- as.data.frame(table(vapply(df1, typeof, character(1))))
colnames(type_tab) <- c("tipo_dato", "num_var_predict")
type_tab[order(-type_tab$num_var_predict), ]
     #     tipo_dato    num_var_predict
     # 1 character                 43
     # 2   integer                 37

# para posteriormente hacer un PCA habrá que gestionar las variables categóricas (lo veremos después en el apartado de PREPROCESAMIENTO)


# resumen estadístico de las variables
summary(df1)
  # variables numéricas <int> : media, mediana, min, max, cuartiles
  # variables categóricas <chr>: frecuencia de cada categoría


#-------------------------------------------------------------------------------
# 1.2 VISUALIZACIÓN DISTRIBUCIONES
# ------------------------------------------------------------------------------

# primero vamos a separar las variables numéricas y categóricas
vars_num <- df1 %>%
  select(where(is.numeric))  # seleccionar solo variables numéricas

vars_cat <- df1 %>%
  select(where(is.character))  # seleccionar solo variables categóricas

#--------------------
# VARIABLES NUMÉRICAS
#--------------------
# sacar la distribución de cada variable numérica en formato largo (long format) para ggplot
vars_num_gg <- vars_num %>%
  pivot_longer(
    cols = everything(),        # todas las columnas
    names_to = "variable",      # nueva columna con el nombre original de la variable
    values_to = "valor"         # nueva columna con el valor numérico
  )

# DIAGRAMA BARRAS POR VARIABLE
#------------------------------
ggplot(vars_num_gg, aes(x = as.factor(valor), fill = variable)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Frecuencia de cada valor", x = "Valor real", y = "Frecuencia") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1, size = 6),
    )

# ggplot(vars_num_gg, aes(x = as.factor(valor), fill = variable)) +
#   geom_bar() +
#   facet_wrap(~ variable, scales = "free_x", ncol = 2) +   # <- aquí: 2 columnas por fila
#   theme_minimal() +
#   labs(title = "Frecuencia de cada valor", x = "Valor real", y = "Frecuencia") +
#   theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1, size = 6),
#         strip.text = element_text(size = 10))


# NUBE DE VALORES POR VARIABLE
#------------------------------
ggplot(vars_num_gg, aes(x = variable, y = valor, color = variable)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Nube de valores por variable") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1),
        )

     # OBSERVACIONES:
     # parece que la variable LoftArea está muy dispersa


# BOXPLOTS POR VARIABLE
#------------------------------

# ggplot(vars_num_gg, aes(y = valor, fill = variable)) +
#   geom_boxplot(alpha = 0.5, outlier.color = "black") +
#   facet_wrap(~ variable, scales = "free") +
#   theme_minimal() +
#   labs(title = "Boxplots por variable", y = "Valor", x = "") +
#   theme( legend.position = "none")

# # con jitter (encima -no se ve muy bien, mejor lo ponemos al lado)
# ggplot(vars_num_gg, aes(x = variable, y = valor, fill = variable)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.6) +
#   geom_jitter(width = 0.2, alpha = 0.4, color = "lightgrey") +
#   facet_wrap(~ variable, scales = "free") + 
#   theme_minimal() +
#   labs(title = "Boxplots con observaciones individuales") +
#   theme(legend.position = "none")

# con jitter (al lado) para ver mejor la distribución
ggplot(vars_num_gg, aes(y = valor, fill = variable)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # ocultar outliers del boxplot
  geom_jitter(aes(x = factor(1)), width = 0.2, alpha = 0.6, size = 0.7, show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free") + 
  labs(title = "Boxplots individuales por variable (con jitter)", y = "Valor", x = "") +
  theme(
    legend.position = "none",
  )


#----------------------
# VARIABLES CATEGÓRICAS
#----------------------

# sacar las frecuencias de cada categoría por variable
vars_cat_gg <- vars_cat %>%
  pivot_longer(
    cols = everything(),        # todas las columnas
    names_to = "variable",      # nueva columna con el nombre original de la variable
    values_to = "valor"         # nueva columna con el valor numérico
  )

# DIAGRAMA BARRAS POR VARIABLE CATEGÓRICA
#-----------------------------------------
ggplot(vars_cat_gg, aes(x = valor, fill = variable)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Frecuencia de cada categoría", x = "Categoría", y = "Frecuencia") +
  theme( legend.position = "none",
         axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 1, size = 6),
         strip.text = element_text(size = 10)
         )



#################################################################################
#################################################################################
# 2. PREPROCESAMIENTO DE DATOS
#################################################################################
#################################################################################

df2 <- df1  # copiar dataset (en df1 se eliminó la variable Id)

#################################################################################
# 2.0 DIVIDIR LOS DATOS EN TRAIN/VALIDATION/TEST 
  # para evitar filtraciones de información durante el preprocesamiento
#################################################################################

# Primero hay que separar variable objetivo y predictoras
# variable objetivo
y <- df2$SalePrice

# variables predictoras
X <- df2 %>%
  select(-SalePrice)  # eliminar columna objetivo

# Establecemos la semilla para reproducibilidad
set.seed(42)

# Separamos 60% para entrenamiento
trainIndex <- createDataPartition(y, p = 0.6, list = FALSE)
                # createDataPartition mantiene la distribución de la variable objetivo
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]

# Del 40% restante, dividimos en 50%-50% (validación y test)
remainingIndex <- setdiff(seq_len(nrow(df2)), trainIndex)
valIndex <- createDataPartition(y[remainingIndex], p = 0.5, list = FALSE)

X_val <- X[remainingIndex[valIndex], ]
y_val <- y[remainingIndex[valIndex]]

X_test <- X[remainingIndex[-valIndex], ]
y_test <- y[remainingIndex[-valIndex]]

cat("Tamaños de conjuntos:\n")
cat("  - Entrenamiento:", nrow(X_train), "observaciones\n")
cat("  - Validación:", nrow(X_val), "observaciones\n")
cat("  - Test:", nrow(X_test), "observaciones\n\n")

# A PARTIR DE ESTE MOMENTO TODAS LAS TRAANSFORMACIONES DE LOS DATOS SE REALIZARÁN SOBRE EL CONJUNTO DE ENTRENAMIENTO
# Y POSTERIORMENTE SE REPLICARÁN EN LOS SUBCONJUNTOS DE VALIDACIÓN Y TEST
# ¡¡ MUY IMPORTANTE RESPETAR ESTO PARA EVIR DATA LEAKAGE!!

#hagamos una copia de seguridad de los datos originales de test
# (naming convention: añadir _num correspondiente a la sección del proceso en la que se encuentra)
X_train_2 <- X_train
X_val_2 <- X_val
X_test_2 <- X_test


#################################################################################
# 2.1 GESTIÓN DE NULOS
#################################################################################

# antes de nada comprobemos que no hay datos duplicados
sum(duplicated(X_train_2))  
  # es 0


#------------------------------------------------
# ver si hay vación o nulos (NULL) en los datos 
# (los NA los trataremos aparte porque hay variables <chr> que lo toman como categoría)
#------------------------------------------------

sum(sapply(X_train_2, is.null))  
  # es 0

# comprobamos si hay datos vacíos ("" o " ")
sum(sapply(X_train_2, function(x) sum(x == "" | x == " " , na.rm = TRUE)))
                                                          # ignorar NAs para esta comprobación
  # es 0


#------------------------------------------------
# NAs
#------------------------------------------------

# crear dataframe de nulos por variable
nulos_X_train <- as.data.frame(colSums(is.na(X_train_2)))
colnames(nulos_X_train) <- c("num_nulos")

nulos_X_train <- nulos_X_train %>%
  # seleccionar las variables que tienen nulos
  filter(num_nulos > 0) %>%
  # convertir los nombres de las filas en una columna
  tibble::rownames_to_column(var = "variable") %>%
  mutate(
    # añadir columna del porcentaje de nulos
    porcentaje_nulos = round((num_nulos / nrow(X_train_2)) * 100, 2),
    # añadir columna del tipo de variable
    tipo_variable = sapply(variable, function(v) class(X_train_2[[v]]))
  ) %>%
  # ordenar de mayor a menor número de nulos
  arrange(desc(tipo_variable), desc(porcentaje_nulos))

print(nulos_X_train)

# OJO QUE HAY VARIABLES CATEGÓRICAS QUE CONTEMPLAN "NA" COMO CATEGORÍA
  # Alley, FireplaceQu, PoolQC, Fence, MiscFeature
  # BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2
  # GarageType, GarageFinish, GarageQual, GarageCond

# CONSECUENCIAS:
#   * Las variables numéricas son tratables, mediante técnicas de imputación de datos
#   * Sin embargo, (casi todas) las variables categóricas contemplan el `NA` como categoría (ej: `PoolQC=NA` cuando no hay piscina), por lo que su tratamiento se solucionaría si sustituimos estos valores vacíos por la categoría ``No`` (no aplica)
#   * Las únicas variables que no contemplan `NA` como categoría son ``MasVnrType`` y ``Electrical``, esas habrá que tratarlas  ocn imputación también.


# NAs variables categóricas "no aplica"
#------------------------------------------------
# para estas variables categóricas, sustituimos los NA por "No"
vars_cat_na_no <- c("Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature",
                    "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
                    "GarageType", "GarageFinish", "GarageQual", "GarageCond")

X_train_2 <- X_train_2 %>%
  mutate(across(all_of(vars_cat_na_no), ~replace_na(.x, "No")))

# comprobamos que ya no hay NAs en esas variables
sapply(X_train_2[ , vars_cat_na_no], function(x) sum(is.na(x)))


# NAs variables numéricas
#------------------------------------------------

# LotFrontage
#-------------
# ver el dataframe volumen de entradas para cada uno de los valores unicos de la variable LotFrontage
LF_unique_freq <- as.data.frame(table(X_train_2$LotFrontage, useNA = "ifany"))
colnames(LF_unique_freq) <- c("LotFrontage", "Frequency")
LF_unique_freq <- LF_unique_freq %>%
  arrange(desc(Frequency))
print(LF_unique_freq)

# Imputación simple en LotFrontage: mediana global + indicador
# primero creamos la variable indicadora de NA
X_train_2$LotFrontage_na <- is.na(X_train_2$LotFrontage)
# luego calculamos la mediana
med <- median(X_train_2$LotFrontage, na.rm = TRUE)
# finalmente imputamos los NAs con la mediana
X_train_2$LotFrontage[is.na(X_train_2$LotFrontage)] <- med

# comprobamos que ya no hay NAs en LotFrontage
sum(is.na(X_train_2$LotFrontage))

# y eliminamos la variable indicadora por limpieza
X_train_2 <- X_train_2 %>%
  select(-LotFrontage_na)

unique(X_train_2$LotFrontage_na)

# veamos cuál es la media y la mediana para estas variables numéricas
vars_num_na <- c("LotFrontage", "MasVnrArea", "GarageYrBlt")

# estadísticas básicas de estas variables
summary(X_train_2[ , vars_num_na])


# MasVnrArea
#-------------
# si no hay revestimiento, el área es 0 por tanto imputamos 0 en los NAs
X_train_2$MasVnrArea[is.na(X_train_2$MasVnrArea)] <- 0

# comprobamos que ya no hay NAs en MasVnrArea
sum(is.na(X_train_2$MasVnrArea))


# GarageYrBlt
#-------------
# En `GarageYrBlt`: si no hay garaje cómo imputamos el año de construcción del garaje? (se podría imputar el año de construcción de la vivienda)
# (esta variable posteriormente desaparecerá porque está muy fuertemente correlacionada con `YearBuilt`, entonces este problema desaparece)





# ------------------------------------------------------------------------------
# 2.2 CORRELACIÓN
# ------------------------------------------------------------------------------

# calcular matriz de correlación para las variables numéricas
vars_num_corr <- vars_num %>%
  select(-SalePrice)  # eliminar variable objetivo para este análisis

corr_matrix <- cor(vars_num_corr, use = "pairwise.complete.obs")  # usar solo pares completos

# mostrar la matriz de correlación
print(round(corr_matrix, 2))

# ahora mostrar solo los pares con alta correlación (>0.7 o <-0.7)
high_corr <- which(abs(corr_matrix) > 0.7 & abs(corr_matrix) < 1, arr.ind = TRUE)
high_corr_pairs <- data.frame(
  Var1 = rownames(corr_matrix)[high_corr[, 1]],
  Var2 = colnames(corr_matrix)[high_corr[, 2]],
  Correlation = corr_matrix[high_corr]
)
high_corr_pairs <- high_corr_pairs %>%
  # eliminar duplicados (Var1, Var2) y (Var2, Var1)
  filter(Var1 < Var2) %>%
  
  # añadir columna con el tipo de var1 y otra con el tipo de var2
  mutate(
    Tipo_Var1 = sapply(Var1, function(v) class(X_train_2[[v]])),
    Tipo_Var2 = sapply(Var2, function(v) class(X_train_2[[v]]))
  ) %>%
  arrange(-abs(Correlation))
high_corr_pairs


# visualizar la matriz de correlación con colores
library(reshape2)
library(ggplot2)
melted_corr <- melt(corr_matrix)
ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=270, vjust=1, hjust=1, size=6),
    axis.text.y = element_text(size = 6)
    ) +
  coord_fixed() +
  labs(title = "Matriz de Correlación de Variables Numéricas")


# ------------------------------------------------------------------------------
# 2.2 CODIFICACIÓN VARIABLES CATEGÓRICAS
# ------------------------------------------------------------------------------







# ------------------------------------------------------------------------------
# --- 3. ESCALADO DE DATOS ---
# ------------------------------------------------------------------------------

# CRÍTICO: Solo se calculan parámetros (media y desviación) del conjunto de ENTRENAMIENTO
# Esto evita "data leakage" (filtración de información del test al train)

# Escalar conjunto de entrenamiento
X_train_scaled <- scale(X_train)

# Guardar los parámetros de escalado aprendidos del entrenamiento
center_params <- attr(X_train_scaled, "scaled:center")  # Medias
scale_params <- attr(X_train_scaled, "scaled:scale")    # Desviaciones estándar

# Aplicar los MISMOS parámetros a validación y test
# NUNCA recalcular medias/desviaciones de val/test
X_val_scaled <- scale(X_val, center = center_params, scale = scale_params)
X_test_scaled <- scale(X_test, center = center_params, scale = scale_params)

cat("Escalado completado usando parámetros de entrenamiento\n")
cat("Ejemplo - Media Intelligence (train):", round(center_params["Intelligence"], 2), "\n\n")



# ------------------------------------------------------------------------------
# --- 4. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA) ---
# ------------------------------------------------------------------------------

# PCA se ajusta SOLO con datos de entrenamiento
# center=FALSE y scale.=FALSE porque ya escalamos manualmente antes
pca <- prcomp(X_train_scaled, center = FALSE, scale. = FALSE)

# Calcular varianza explicada acumulada por cada componente
var_explained <- summary(pca)$importance[3, ]  # Tercera fila = proporción acumulada

# Seleccionar componentes que expliquen al menos 95% de la varianza
n_comp <- min(which(var_explained >= 0.95))

cat("--- RESULTADO PCA ---\n")
cat("Componentes que explican ≥95% varianza:", n_comp, "de", ncol(X), "\n")
cat("Varianza explicada por componente:\n")
print(round(summary(pca)$importance, 3))
cat("\n")



# ------------------------------------------------------------------------------
# --- 5. PROYECCIÓN A ESPACIO PCA ---
# ------------------------------------------------------------------------------

# Transformar datos usando la rotación (loading matrix) aprendida del train

# Train: Ya transformado por prcomp
X_train_pca <- pca$x[, 1:n_comp, drop = FALSE]

# Val y Test: Proyectar usando la matriz de rotación del PCA entrenado
X_val_pca <- as.matrix(X_val_scaled) %*% pca$rotation[, 1:n_comp, drop = FALSE]
X_test_pca <- as.matrix(X_test_scaled) %*% pca$rotation[, 1:n_comp, drop = FALSE]



# ------------------------------------------------------------------------------
# --- 6. ENTRENAMIENTO DEL MODELO ---
# ------------------------------------------------------------------------------

# Ajustar regresión lineal múltiple con los componentes principales
modelo_lm_pca <- lm(y_train ~ ., data = as.data.frame(X_train_pca))

cat("--- MODELO AJUSTADO ---\n")
print(summary(modelo_lm_pca))
cat("\n")
#salidas: Los errores están ligeramente sesgados hacia subestimar Combat (Mediana ≈ -5.2)
#Outliers: Hay algunos casos con errores muy grandes (±50 puntos), probablemente personajes con habilidades únicas
#Todos los PC son estadisticamente significativos
#R² = 0.67 es bueno para datos reales




# ------------------------------------------------------------------------------
# --- 7. PREDICCIÓN Y EVALUACIÓN ---
# ------------------------------------------------------------------------------

# Predecir en validación y test usando las proyecciones PCA
pred_train_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_train_pca))
pred_val_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_val_pca))
pred_test_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_test_pca))

# Función para calcular métricas de error
rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2))
mae <- function(y_true, y_pred) mean(abs(y_true - y_pred))
r2 <- function(y_true, y_pred) cor(y_true, y_pred)^2



# ------------------------------------------------------------------------------
# --- 8. RESULTADOS ---
# ------------------------------------------------------------------------------

cat("========================================\n")
cat("      MÉTRICAS DE RENDIMIENTO (PCA)\n")
cat("========================================\n\n")

cat("ENTRENAMIENTO:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_pca), 2), "\n") #promedio de los errores de predicción
cat("  MAE: ", round(mae(y_train, pred_train_pca), 2), "\n") #Error promedio sin penalizar outlaiers
cat("  R²:  ", round(r2(y_train, pred_train_pca), 4), "\n\n") #Coeficiente de determinación (varianza explicada)

cat("VALIDACIÓN:\n")
cat("  RMSE:", round(rmse(y_val, pred_val_pca), 2), "\n")
cat("  MAE: ", round(mae(y_val, pred_val_pca), 2), "\n")
cat("  R²:  ", round(r2(y_val, pred_val_pca), 4), "\n\n")

cat("TEST (conjunto de reserva):\n")
cat("  RMSE:", round(rmse(y_test, pred_test_pca), 2), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_pca), 2), "\n")
cat("  R²:  ", round(r2(y_test, pred_test_pca), 4), "\n\n")
#salidas: Train y Test son casi idénticos (RMSE: 19.33 vs 19.16) generaliza perfectamente a datos nuevos



# --- 9. VISUALIZACIONES OPCIONALES ---
# Descomenta si quieres ver gráficos

# # Varianza explicada por componente
par(mfrow = c(1, 2))
barplot(summary(pca)$importance[2, ], 
        main = "Varianza por Componente",
        xlab = "Componente Principal", 
        ylab = "Proporción de Varianza",
        col = "steelblue")

plot(var_explained, type = "b", 
     main = "Varianza Acumulada",
     xlab = "Número de Componentes", 
     ylab = "Proporción Acumulada",
     col = "darkred", lwd = 2)
abline(h = 0.95, col = "blue", lty = 2, lwd = 2)
legend("bottomright", legend = "Umbral 95%", 
       col = "blue", lty = 2, lwd = 2)

# # Predicciones vs Valores Reales
par(mfrow = c(1, 3))
plot(y_train, pred_train_pca, main = "Train", 
     xlab = "Real", ylab = "Predicho", pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

plot(y_val, pred_val_pca, main = "Validación", 
     xlab = "Real", ylab = "Predicho", pch = 19, col = "green")
abline(0, 1, col = "red", lwd = 2)

plot(y_test, pred_test_pca, main = "Test", 
     xlab = "Real", ylab = "Predicho", pch = 19, col = "orange")
abline(0, 1, col = "red", lwd = 2)

# ============================================================================
# NOTAS IMPORTANTES:
# ============================================================================
# 1. NUNCA usar estadísticas de val/test para preprocesamiento
# 2. PCA se ajusta solo en train, luego se proyectan val/test
# 3. El conjunto de test NO debe tocarse hasta la evaluación final
# 4. Si RMSE(val) << RMSE(test) → posible overfitting
# 5. Usar validación para ajustar hiperparámetros (ej: número de componentes)
# ============================================================================