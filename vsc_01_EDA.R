# INFO
# FLUJO DE TRABAJO CORRECTO PARA MODELADO PREDICTIVO EVITANDO DATA LEAKAGE
# 1. Cargar los datos completos
# 2. Análisis Exploratorio (EDA), con todo el dataset 
      # • Entender tipos de variables y su significado 
      # • Visualizar distribuciones, correlaciones, outliers 
      # • Identificar valores faltantes y su patrón 
# 3. Dividir en train/validation/test (60-20-20) 
# 4. Preprocesamiento: parámetros calculados solo con train 
# 5. Aplicar transformaciones a train, validation y test usando parámetros de train 
# 6. Ajustar modelos con train 
# 7. Seleccionar hiperparámetros evaluando en validation 
# 8. Evaluación final: una única vez en test 

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

library(tidyverse)  # para manipulación y visualización de datos
library(dplyr)      # para manipulación de datos
library(ggplot2)    # para visualización de datos

library(tidymodels)  # para pipelines y workflows de preprocesamiento y modelados


# ------------------------------------------------------------------------------
# 0.2 CARGA DE DATOS
# ------------------------------------------------------------------------------

# Directorio de trabajo
setwd("C:/Users/velir/^LOCAL_GITHUB/2025_UAX_MUIA/1c_Estadistica/MUIA_MyEpIA-ENTREGA_1-Regresion_Inmobiliaria/")

# Cargamos el dataset
df <- read.csv("data/dataset.csv")

cat("Dimensiones del dataset:\n", nrow(df), "filas\n", ncol(df), "columnas\n\n")



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

#################################################################################
# 1.1 TIPOS VARIABLES
#################################################################################

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


################################################################################
# 1.2 VISUALIZACIÓN DISTRIBUCIONES
################################################################################

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





################################################################################
# 1.3 ANÁLISIS DE CORRELACIÓN
################################################################################
# vamos ahora a estudiar la correlación entre variables numéricas, 
# para poder hacer un análisis de correlación clásico es necesario que las variables sean numéricas

# qué sería lo recomendable hacer con la variable objeetivo? eliminarla del análisis de correalación o dejarla?
  # mejor eliminarla para no sesgar el análisis de correlación entre variables predictoras

# calcular matriz de correlación para las variables numéricas
vars_num_corr <- vars_num %>%
  select(-SalePrice)  # eliminar variable objetivo para este análisis


#------------------------------------------------------------------------------------
# MATRIZ DE CORRELACIÓN

corr_matrix <- cor(vars_num_corr, use = "pairwise.complete.obs")  # usar solo pares completos

# mostrar la matriz de correlación completa
print(round(corr_matrix, 2))

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


#------------------------------------------------------------------------------------
# ANÁLISIS DE ALTAS CORRELACIONES
#------------------------------------------------------------------------------------

# ahora mostrar solo los pares con alta correlación
high_corr <- which(abs(corr_matrix) > 0.5 & abs(corr_matrix) < 1, arr.ind = TRUE)
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


# Destacan los pares de variables con mucha correlación:
# Var1  Var2  Correlation  Tipo_Var1  Tipo_Var2
# GarageArea	GarageCars	0.8824754	integer	integer
# GarageYrBlt	YearBuilt	0.8256675	integer	integer
# GrLivArea	TotRmsAbvGrd	0.8254894	integer	integer
# TotalBsmtSF	X1stFlrSF	0.8195300	integer	integer

#  * La variable `GarageYrBlt` (año de construcción del garaje) está muy correlacionada con `YearBuilt` (año de construcción de la vivienda), lo que tiene sentido en la mayoría de los casos coincidirá con la contrucción del edificio, por lo que es probable que haya que eliminar una de las dos.
 
#  * La variable `TotalBsmtSF` (superficie total del sótano) está muy correlacionada con `X1stFlrSF` (superficie de la planta baja) y `GrLivArea` (superficie habitable sobre el suelo), lo que tiene sentido ya que ambas superficies contribuyen al área habitable total de la vivienda.
 
#  * La variable `GarageArea` (área del garaje) está muy correlacionada con `GarageCars` (número de coches que caben en el garaje), tendremos que ver con qué variable tiene más sentido quedarse.
 
#  * La variable `TotRmsAbvGrd` (número total de habitaciones sobre el suelo) está muy correlacionada con `GrLivArea` (superficie habitable sobre el suelo), lo que tiene sentido ya que más habitaciones suelen implicar una mayor superficie habitable.

# el resto son:
# rLivArea	X2ndFlrSF	0.6875011	integer	integer
# BedroomAbvGr	TotRmsAbvGrd	0.6766199	integer	integer
# BsmtFinSF1	BsmtFullBath	0.6492118	integer	integer
# GarageYrBlt	YearRemodAdd	0.6422768	integer	integer
# FullBath	GrLivArea	0.6300116	integer	integer
# TotRmsAbvGrd	X2ndFlrSF	0.6164226	integer	integer

# qué tendría más sentido en el feature engineering (hablando de las variables en los pares muy correlacionados), si a nivel de signifcado son lo mismo, qué es mejor quedarse ocn variables continuas o discretaas?




#################################################################################
# 1.4 ANÁLISIS DE OUTLIERS
#################################################################################

#...












#################################################################################
# 1.5 ANÁLISIS DE VALORES CORRUPTOS (duplicados, nulos, faltantes...)
#################################################################################

#----------------------------------------------------------------------------------
# DATOS DUPLICADOS
#----------------------------------------------------------------------------------

cat( "Número de entradas duplicadas:", sum(duplicated(train_data)) )

#----------------------------------------------------------------------------------
# DATOS NULL O VACÍOS
#----------------------------------------------------------------------------------

cat( "Número de NULL:", sum(sapply(train_data, is.null)) )

cat("\nNúmero de vacíos:",sum(sapply(train_data, function(x) sum(x == "" | x == " " , na.rm = TRUE)))  )


#################################################################################
# NAs
#################################################################################
# VEAMOS QUÉ VARIABLES TIENEN NAs Y CÓMO TRATARLOS

# crear dataframe de nulos por variable
nulos_train <- as.data.frame(colSums(is.na(train_data)))
colnames(nulos_train) <- c("num_nulos")

nulos_train <- nulos_train %>%
  # seleccionar las variables que tienen nulos
  filter(num_nulos > 0) %>%
  # convertir los nombres de las filas en una columna
  tibble::rownames_to_column(var = "variable") %>%
  mutate(
    # añadir columna del porcentaje de nulos
    porcentaje_nulos = round((num_nulos / nrow(train_data)) * 100, 2),
    # añadir columna del tipo de variable
    tipo_variable = sapply(variable, function(v) class(train_data[[v]]))
  ) %>%
  # ordenar de mayor a menor número de nulos
  arrange(desc(tipo_variable), desc(porcentaje_nulos))

print(nulos_train)

# OJO QUE HAY VARIABLES CATEGÓRICAS QUE CONTEMPLAN "NA" COMO CATEGORÍA
  # Alley, FireplaceQu, PoolQC, Fence, MiscFeature
  # BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2
  # GarageType, GarageFinish, GarageQual, GarageCond



# Entre las variables categóricas encontramos dos casuísticas de valores nulos:
  
#   * Tras estudiar las variables categóricas, obervamos que casi todas contemplan el valor `NA` como categoría (ej: `PoolQC=NA` cuando no hay piscina), esto puede dar problemas, por lo que sustituiremos estos valores por la categoría `None` (no aplica)
  
#   * Por otro lado lado ``MasVnrType`` y `Electrical`, son las dos únicas variables categóricas con nulos que no  contemplan `NA` como categoría, esto es, los valores nulos son valores faltantes que trataremos con imputación (al tratarse de variables categóricas, imputaremos la moda).



# Por su parte, entre las variable numéricas también observamos distintos casos:

#   * `LotFrontage` (la longitud de la fachada del lote): aquí, los valores nulos pueden indicar que no se midió o registró esta información. Podemos imputar estos valores con la mediana de la variable.
    
#   * `MasVnrArea` (área del revestimiento): cuando ``MasVnrType=None`` (ningún tipo de revestimiento aplicado) la variable `MasVnrArea` tiene valores faltantes, lo que tiene sentido ya que si no hay revestimiento, no hay área de revestimiento, por lo que para tratarlos les imputaremos área `=0`.

#   * `GarageYrBlt` (año de construcción del garaje): los valores nulos en esta variable coinciden con los de la variable `GarageType=NA` (no hay garaje). Como ya vimos anteriormente, esta variable está muy correlacionada con `YearBuilt`, por lo que podríamos eliminarla directamente más adelante. Si quisiéramos imputar estos valores, podríamos usar el año de construcción de la vivienda (`YearBuilt`) como referencia, pero dado que esta variable desaparecerá en el feature engineering, no es necesario hacer esta imputación ahora.










#################################################################################
#################################################################################
# 2. DIVISIÓN CONJUNTOS DATOS: TRAIN / VALIDATION / TEST
#################################################################################
#################################################################################

df2 <- df1  # (en df1 se eliminó la variable Id)

# Desde este momento vamos a usar pipelines y workflows de tidymodels para el preprocesamiento, ya que esto nos va a permitir aplicar transformaciones en el conjunto de entrenamiento y posteriormente replicarlas de forma simple en los conjuntos de validación y test sin riesgo de data leakage.


set.seed(42)

# Separar 60% para train
initial_split  <- rsample::initial_split(df2, prop = 0.6)
train_data     <- rsample::training(initial_split)
temp_data      <- rsample::testing(initial_split)

# del 40% restante, separar 50%-50% para validation y test
validation_split <- rsample::initial_split(temp_data, prop = 0.5)
validation_data  <- rsample::training(validation_split)
test_data        <- rsample::testing(validation_split)


cat("Tamaños de conjuntos:\n")
cat("  - Entrenamiento:", nrow(train_data), "observaciones\n")
cat("  - Validación:", nrow(validation_data), "observaciones\n")
cat("  - Test:", nrow(test_data), "observaciones\n\n")



# A PARTIR DE ESTE MOMENTO TODAS LAS TRANSFORMACIONES DE LOS DATOS SE REALIZARÁN SOBRE EL CONJUNTO DE ENTRENAMIENTO
# Y POSTERIORMENTE SE REPLICARÁN EN LOS SUBCONJUNTOS DE VALIDACIÓN Y TEST
# ¡¡ MUY IMPORTANTE RESPETAR ESTO PARA EVIR DATA LEAKAGE!!



#################################################################################
#################################################################################
# 3. PREPROCESAMIENTO DE DATOS
#################################################################################
#################################################################################

# cada tipo de variable va a requerir un tratamiento diferente (usando solo el conjunto de datos de train), por lo que, antes de nada, vamos a separar las variables en los distintos tipos




# variables categóricas
vars_cat <- train_data %>%
  select(where(is.character))



# variables numéricas
vars_num <- data %>% select(where(is.numeric)) %>% names()

# umbral (ajusta si quieres)
umbral <- 100

# contar valores únicos (ignorando NA)
unique_counts <- sapply(data[num_vars], function(x) length(unique(na.omit(x))))

# listas resultantes
vars_num_cont <- names(unique_counts[unique_counts > umbral])
vars_num_disc <- names(unique_counts[unique_counts <= umbral])

# variables categóricas (character o factor)
vars_cat <- data %>% select(where(~ is.character(.) || is.factor(.))) %>% names()

# mostrar resultados breves
cat("Num contínuas:", length(vars_num_cont), "\n")
cat("Num discretas:", length(vars_num_disc), "\n")
cat("Categoricas:", length(vars_cat), "\n")




#################################################################################
# 3.1 PROCESAMIENTO DE VARIABLES CATEGÓRICAS
#################################################################################


#-------------------------------------------------------------------------------
# TRATAMIENTO DE VALORES NULOS EN VARIABLES CATEGÓRICAS
#-------------------------------------------------------------------------------

#----------------------------------------
# empecemos con las variables categóricas que contemplan NA como categoría, y vamos a recategorizar esos NAs como "None"

vars_cat_na_no <- c("Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature",
                    "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
                    "GarageType", "GarageFinish", "GarageQual", "GarageCond")

# sustituimos los NAs por "None"
train_data <- train_data %>%
  mutate(across(all_of(vars_cat_na_no), ~replace_na(.x, "None")))

# comprobamos que ya no hay NAs en esas variables
sapply(train_data[ , vars_cat_na_no], function(x) sum(is.na(x)))


#----------------------------------------
# ahora tratemos las variables categóricas con NAs que no contemplan NA como categoría: MasVnrType y Electrical, que les imputaremos la moda respectivamente

vars_cat_na_yes <- c("MasVnrType", "Electrical")

# comprobamos el número de NAs en esas variables
sapply(train_data[ , vars_cat_na_yes], function(x) sum(is.na(x)))

# función para calcular la moda
moda_func <- function(x) {
  # cogemos la tabla de frecuencias y devolvemos el nombre de la categoría más frecuente
  names(sort(table(x), decreasing = TRUE))[1]
}

# ahora imputamos los NAs con la moda
train_data <- train_data %>%
  mutate(across(all_of(vars_cat_na_yes), ~replace_na(.x, moda_func(.x))))

# comprobamos que ya no hay NAs en esas variables
sapply(train_data[ , vars_cat_na_yes], function(x) sum(is.na(x)))


































#################################################################################
# 3.1 PROCESAMIENTO DE VARIABLES NUMÉRICAS
#################################################################################











































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