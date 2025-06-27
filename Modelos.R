library(xgboost)
library(caret)
library(readxl)
library(Metrics)

# Cargar y preparar los datos

base <- read_excel("data/base_agua_limpia.xlsx")
base <- base[!is.na(base$dqof), ]

# Crear partición de datos 

set.seed(123)
indice_entrenamiento <- createDataPartition(base$dqof, p = 0.65, list = FALSE)
datos_train <- base[indice_entrenamiento, ]
datos_prueba <- base[-indice_entrenamiento, ]

# Obtener etiquetas

train_y <- datos_train$dqof
test_y <- datos_prueba$dqof

#Elimina el target de los datos

datos_train <- datos_train[, !(names(datos_train) %in% c("dqof"))]
datos_prueba <- datos_prueba[, !(names(datos_prueba) %in% c("dqof"))]

# Crear transformación dummy usando los datos de entrenamiento

dummies <- dummyVars("~.", data = datos_train)

# Aplicar transformación a ambos conjuntos

train_x <- predict(dummies, newdata = datos_train)
test_x <- predict(dummies, newdata = datos_prueba)

# Verificar y alinear columnas

missing_cols <- setdiff(colnames(train_x), colnames(test_x))
if(length(missing_cols) > 0) {
  test_x <- cbind(test_x, matrix(0, nrow = nrow(test_x), ncol = length(missing_cols)))
  colnames(test_x)[(ncol(test_x)-length(missing_cols)+1):ncol(test_x)] <- missing_cols
  test_x <- test_x[, colnames(train_x)]  # Reordenar columnas
}

# Convertir a matrices numéricas

train_x <- as.matrix(train_x)
test_x <- as.matrix(test_x)

# Crear matrices DMatrix

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# Parámetros del modelo

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 10,
  lambda = 2,
  alpha = 0.1,
  subsample = 0.7,
  colsample_bytree = 0.7
)


params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,               # Tasa de aprendizaje más común
  max_depth = 6,           # Profundidad moderada
  min_child_weight = 3,    # Controla el sobreajuste
  gamma = 0,               # Regularización mínima para empezar
  subsample = 0.8,         # Muestra el 80% de datos por árbol
  colsample_bytree = 0.8,  # Usa 80% de features por árbol
  lambda = 1,              # Regularización L2 moderada
  alpha = 0,               # Regularización L1 inicialmente desactivada
  nthread = 4              # Usa 4 núcleos del CPU
)

modelo_xg <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 5000,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 50
)

# Evaluar el modelo
preds <- predict(modelo_xg, dtest)
rmse <- sqrt(mean((preds - test_y)^2))
print(paste("RMSE:", rmse))

# Importancia de variables
importance <- xgb.importance(model = modelo_xg)
print(importance)


# Ajuste de hiperparámetros

ctrl <- trainControl(method = "cv", number = 5)

grid_fino <- expand.grid(
  nrounds = c(800, 1000, 1200),
  eta = c(0.03, 0.05, 0.07),
  max_depth = c(5, 6, 7),
  gamma = c(0.05, 0.1, 0.15),
  min_child_weight = c(2, 3, 4),
  subsample = c(0.75, 0.8, 0.85),
  colsample_bytree = c(0.75, 0.8, 0.85),
  lambda = c(0.8, 1, 1.2),
  alpha = c(0, 0.05, 0.1)
)

modelo_ajustado <- train(
  x = train_x, 
  y = train_y,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid
)

train_preds <- predict(modelo_ajustado, train_x)
test_preds<- predict(modelo_ajustado, test_x)

# RMSE (Root Mean Squared Error)

rmse_train <- rmse(train_y, train_preds)
rmse_test <- rmse(test_y, test_preds)

# MAE (Mean Absolute Error)

mae_train <- mae(train_y, train_preds)
mae_test <- mae(test_y, test_preds)

# R² (Coeficiente de determinación)

r2_train <- cor(train_y, train_preds)^2
r2_test <- cor(test_y, test_preds)^2

# Imprimir resultados

cat("Train RMSE:", rmse_train, "\n",
    "Test RMSE:", rmse_test, "\n",
    "Train MAE:", mae_train, "\n",
    "Test MAE:", mae_test, "\n",
    "Train R²:", r2_train, "\n",
    "Test R²:", r2_test)

# Visualizar los resultados

resultados <- data.frame(
  real = test_y, prediccion = test_preds
)

ggplot(resultados, aes(x = real, y = prediccion)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Valores Reales vs. Predicciones (Test)",
    x = "Valor Real (dqof)",
    y = "Valor Predicho"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray90")
  )

resultados$Residuals <- test_y - test_preds

ggplot(resultados, aes(x = prediccion, y = Residuals)) +
  geom_point(color = "#D55E00", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Análisis de Residuales",
    x = "Valor Predicho",
    y = "Residual (Real - Predicho)"
  ) +
  theme_minimal()


ggplot(resultados) +
  geom_density(aes(x = real, fill = "real"), alpha = 0.5) +
  geom_density(aes(x = prediccion, fill = "prediccion"), alpha = 0.5) +
  scale_fill_manual(values = c("real" = "#009E73", "prediccion" = "#E69F00")) +
  labs(
    title = "Distribución: Valores Reales vs. Predichos",
    x = "dqof",
    y = "Densidad",
    fill = ""
  ) +
  theme_bw()

rmse_val <- round(rmse(test_y, test_preds), 2)

ggplot(resultados, aes(x = real, y = prediccion)) +
  geom_point(color = "#56B4E9") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  annotate(
    "text", x = min(resultados$real), y = max(resultados$prediccion),
    label = paste("RMSE =", rmse_val),
    hjust = 0, vjust = 1, size = 5, color = "darkred"
  ) +
  labs(title = "Ajuste del Modelo XGBoost") +
  theme_light()

importance <- xgb.importance(model = modelo_ajustado)
print(importance)



