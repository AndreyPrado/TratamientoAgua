library(tidyverse)
library(xgboost)
library(caret)
library(readxl)
library(Metrics)
library(moments)
library(tseries)

# Función para cargar los datos

xg_cargar_datos <- function(ruta, var_obj){
  base <- read_excel(ruta)
  
  if (var_obj %in% names(base)) {
    base <- base[!is.na(base[[var_obj]]), ]
  } 
  return(base)
}

# Función para realizar la partición de los datos

xg_particion_datos <- function(datos, var_obj, tam_split = 0.65, semilla = 123){
  set.seed(semilla)
  indice <- createDataPartition(datos[[var_obj]], p = tam_split, list = FALSE)
  
  train_data <- datos[indice, ]
  test_data <- datos[-indice, ]
  
  train_y <- train_data[[var_obj]]
  test_y <- test_data[[var_obj]]
  
  train_x <- train_data[, !(names(train_data)) %in% var_obj]
  test_x <- test_data[, !(names(test_data)) %in% var_obj]
  
  dummies <- dummyVars("~.", data = train_x)
  train_x <- predict(dummies, newdata = train_x)
  test_x <- predict(dummies, newdata = test_x)
  
  missing_cols <- setdiff(colnames(train_x), colnames(test_x))
  if(length(missing_cols) > 0) {
    test_x <- cbind(test_x, matrix(0, nrow = nrow(test_x), ncol = length(missing_cols)))
    colnames(test_x)[(ncol(test_x)-length(missing_cols)+1):ncol(test_x)] <- missing_cols
    test_x <- test_x[, colnames(train_x)]
  }
  
  train_x <- as.matrix(train_x)
  test_x <- as.matrix(test_x)
  dtrain <- xgb.DMatrix(data = train_x, label = train_y)
  dtest <- xgb.DMatrix(data = test_x, label = test_y)
  
  return(list(
    dtrain = dtrain,
    dtest = dtest,
    train_x = train_x,
    test_x = test_x,
    train_y = train_y,
    test_y = test_y
  ))
}

# Función para crear el modelo básico

xg_modelo_basico <- function(lista_datos, parametros, nrounds = 5000){
  modelo <- xgb.train(
    params = parametros,
    data = lista_datos$dtrain,
    nrounds = nrounds,
    watchlist = list(train = lista_datos$dtrain, test = lista_datos$dtest),
    print_every_n = 50
  )
  
  test_x <- lista_datos$test_x
  test_y <- lista_datos$test_y
  train_x <- lista_datos$train_x
  train_y <- lista_datos$train_y
  
  train_preds <- predict(modelo, train_x)
  test_preds<- predict(modelo, test_x)
  
  # RMSE (Root Mean Squared Error)
  
  rmse_train <- rmse(train_y, train_preds)
  rmse_test <- rmse(test_y, test_preds)
  
  # MAE (Mean Absolute Error)
  
  mae_train <- mae(train_y, train_preds)
  mae_test <- mae(test_y, test_preds)
  
  # R² (Coeficiente de determinación)
  
  r2_train <- cor(train_y, train_preds)^2
  r2_test <- cor(test_y, test_preds)^2
  
  # MAPE (Mean Absolute Porcentual Error)
  
  mape_train <- mape(train_y, train_preds)
  mape_test <- mape(test_y, test_preds)
  
  # RAE (Relative Absolute Error)
  
  rae_train <- rae(train_y, train_preds)
  rae_test <- rae(test_y, test_preds)
  
  ## Métricas de errores
  
  errores <- test_y - test_preds
  
  # Skewness
  
  skewness <- skewness(errores)
  
  # Kurtosis

  kurtosis <- moments::kurtosis(errores)
  
  # Imprimir resultados
  
  cat("Train RMSE:", rmse_train, "\n",
      "Test RMSE:", rmse_test, "\n",
      "Train MAE:", mae_train, "\n",
      "Test MAE:", mae_test, "\n",
      "Train MAPE", mape_train, "\n",
      "Test MAPE", mape_test, "\n",
      "Train RAE", rae_train, "\n",
      "Test RAE", rae_test, "\n",
      "Train R²:", r2_train, "\n",
      "Test R²:", r2_test, "\n",
      "MÉTRICAS DE ERRORES", "\n",
      "Simetría", skewness, "\n",
      "Kurtosis", kurtosis, "\n"
      )
  return(modelo)
}

# Función para entrenar y evaluar el modelo ajustado

xg_optimizar_modelo <- function(lista_datos, grid, cv = 5, nrounds = 5000){
  control <- trainControl(method = "cv", number = cv)
  
  modelo <- train(
    x = lista_datos$train_x,
    y = lista_datos$train_y,
    method = "xgbTree",
    trControl = ctrl,
    tuneGrid = grid
  )
  
  test_x <- lista_datos$test_x
  test_y <- lista_datos$test_y
  train_x <- lista_datos$train_x
  train_y <- lista_datos$train_y
  
  train_preds <- predict(modelo, lista_datos$dtrain)
  test_preds<- predict(modelo, lista_datos$dtest)
  
  # RMSE (Root Mean Squared Error)
  
  rmse_train <- rmse(train_y, train_preds)
  rmse_test <- rmse(test_y, test_preds)
  
  # MAE (Mean Absolute Error)
  
  mae_train <- mae(train_y, train_preds)
  mae_test <- mae(test_y, test_preds)
  
  # R² (Coeficiente de determinación)
  
  r2_train <- cor(train_y, train_preds)^2
  r2_test <- cor(test_y, test_preds)^2
  
  # MAPE (Mean Absolute Porcentual Error)
  
  mape_train <- mape(train_y, train_preds)
  mape_test <- mape(test_y, test_preds)
  
  # RAE (Relative Absolute Error)
  
  rae_train <- rae(train_y, train_preds)
  rae_test <- rae(test_y, test_preds)
  
  ## Métricas de errores
  
  errores <- lista_datos$dtest - test_preds
  
  # Skewness
  
  skewness <- skewness(errores)
  
  # Kurtosis
  
  kurtosis <- moments::kurtosis(errores)
  
  # Imprimir resultados
  
  cat("Train RMSE:", rmse_train, "\n",
      "Test RMSE:", rmse_test, "\n",
      "Train MAE:", mae_train, "\n",
      "Test MAE:", mae_test, "\n",
      "Train MAPE", mape_train, "\n",
      "Test MAPE", mape_test, "\n",
      "Train RAE", rae_train, "\n",
      "Test RAE", rae_test, "\n",
      "Train R²:", r2_train, "\n",
      "Test R²:", r2_test, "\n",
      "MÉTRICAS DE ERRORES", "\n",
      "Simetría:", skewness, "\n",
      "Kurtosis:", kurtosis, "\n"
  )
  return(modelo)
}

# Función para visualizar resultados

xg_grafico_resultados <- function(modelo, lista_datos, title = "Valores reales vs Predicciones"){
  preds <- predict(modelo, lista_datos$dtest)
  test_y <- lista_datos$test_y
  
  resultados <- data.frame(real = test_y, prediccion = preds)
  rmse_val <- round(rmse(test_y, preds), 2)
  
  ggplot(resultados, aes(x = real, y = prediccion)) +
    geom_point(color = "steelblue", alpha = 0.6, size = 3) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = title,
      x = "Valor Real",
      y = "Predicción"
    ) +
    annotate(
      "text", x = min(resultados$real), y = max(resultados$prediccion),
      label = paste("RMSE =", rmse_val),
      hjust = 0, vjust = 1, size = 5, color = "darkred"
    ) +
    theme_minimal()
}

# Función para visualizar residuos

xg_grafico_residuos <- function(modelo, lista_datos){
  preds <- predict(modelo, lista_datos$dtest)
  test_y <- lista_datos$test_y
  
  resultados <- data.frame(prediccion = preds, residual = test_y - preds)
  
  ggplot(resultados, aes(x = prediccion, y = residual)) +
    geom_point(color = "#D55E00", alpha = 0.7) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Análisis de Residuales",
      x = "Predicción",
      y = "Residuos (Real - Predicción)"
    ) +
    theme_minimal()
}

# Función para visualizar importancia de las variables

xg_grafico_importancia <- function(modelo, top = 10){
  importancia <- xgb.importance(model = modelo)
  
  ggplot(importancia[1:top,], aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Importancia de Variables",
      x = "Variable",
      y = "Importancia"
    ) +
    theme_minimal()
}

### ----------------------------------- EJEMPLO ------------------------------------- ###

# Predicción del DQO 

## JULIO

base_jul <- xg_cargar_datos("data/base_agua_limpia_julio.xlsx", var_obj = "dqoj")
lista <- xg_particion_datos(base_jul, "dqoj", 0.65, 123)

parametros <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 10,
  lambda = 2,
  alpha = 0.1,
  subsample = 0.7,
  colsample_bytree = 0.7
)

modelo <- xg_modelo_basico(lista, parametros = parametros, nrounds = 5000)

xg_grafico_importancia(modelo, 15)

xg_grafico_resultados(modelo, lista)

xg_grafico_residuos(modelo, lista)

# FEBRERO

base_feb <- xg_cargar_datos("data/base_agua_limpia_febrero.xlsx", var_obj = "dqof")
lista <- xg_particion_datos(base_feb, "dqof", 0.65, 123)

parametros <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 10,
  lambda = 2,
  alpha = 0.1,
  subsample = 0.7,
  colsample_bytree = 0.7
)

modelo <- xg_modelo_basico(lista, parametros = parametros, nrounds = 5000)

xg_grafico_importancia(modelo, 15)

xg_grafico_resultados(modelo, lista)

xg_grafico_residuos(modelo, lista)

# Predicción del DBO

## JULIO

base_jul <- xg_cargar_datos("data/base_agua_limpia_julio.xlsx", var_obj = "dboj")
lista <- xg_particion_datos(base_jul, "dboj", 0.65, 123)

parametros <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 10,
  lambda = 2,
  alpha = 0.1,
  subsample = 0.7,
  colsample_bytree = 0.7
)

modelo <- xg_modelo_basico(lista, parametros = parametros, nrounds = 5000)

xg_grafico_importancia(modelo, 15)

xg_grafico_resultados(modelo, lista)

xg_grafico_residuos(modelo, lista)

## FEBRERO

base_feb <- xg_cargar_datos("data/base_agua_limpia_febrero.xlsx", var_obj = "dbof")
lista <- xg_particion_datos(base_feb, "dbof", 0.65, 123)

parametros <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 10,
  lambda = 2,
  alpha = 0.1,
  subsample = 0.7,
  colsample_bytree = 0.7
)

modelo <- xg_modelo_basico(lista, parametros = parametros, nrounds = 5000)

xg_grafico_importancia(modelo, 15)

xg_grafico_resultados(modelo, lista)

xg_grafico_residuos(modelo, lista)
