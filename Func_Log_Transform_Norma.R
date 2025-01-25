# Función para ejecutar una transformación logarítmica y tratar de acercar data a distribución normal

# Paquetes necesarios
library(nortest) # Para el test de Lilliefors (Kolmogorov-Smirnov corregido)
library(knitr)   # Para la salida tabular

# Función para transformar logarítmicamente y evaluar normalidad
log_transform_norma <- function(data) {
  # Verificar que las columnas sean numéricas
  data <- data[, sapply(data, is.numeric)]
  
  # Aplicar la transformación logarítmica (con manejo de valores <= 0)
  log_transformada <- as.data.frame(lapply(data, function(x) {
    if (any(x <= 0)) return(rep(NA, length(x))) else return(log(x))
  }))
  
  # Evaluar normalidad con Shapiro-Wilk y Lilliefors
  evaluar_normalidad <- function(variable) {
    if (length(variable[!is.na(variable)]) > 2) {
      shapiro_p <- tryCatch(shapiro.test(variable)$p.value, error = function(e) NA)
      ks_p <- tryCatch(nortest::lillie.test(variable)$p.value, error = function(e) NA)
    } else {
      shapiro_p <- ks_p <- NA
    }
    list(Shapiro_Wilk = shapiro_p, Lilliefors_KS = ks_p)
  }
  
  # Crear un dataframe para almacenar los resultados
  resultados <- data.frame(
    Escala = character(),
    Índice = character(),
    `p-valor` = character(),
    Interpretación = character(),
    stringsAsFactors = FALSE
  )
  
  interpretar_resultado <- function(p) {
    if (is.na(p)) return(c("NA", "Valor insuficiente"))
    nivel_significancia <- ifelse(p <= 0.001, "***",
                                  ifelse(p <= 0.01, "**",
                                         ifelse(p <= 0.05, "*", "NS")))
    interpretacion <- ifelse(p > 0.05, "Normalidad", "No Normalidad")
    c(sprintf("%.2f, %s", p, nivel_significancia), interpretacion)
  }
  
  # Iterar sobre cada columna
  for (var in colnames(log_transformada)) {
    variable <- log_transformada[[var]]
    resultados_test <- evaluar_normalidad(variable)
    
    # Agregar resultados de Shapiro-Wilk
    sw_resultado <- interpretar_resultado(resultados_test$Shapiro_Wilk)
    resultados <- rbind(resultados, data.frame(
      Escala = var,
      Índice = "S-W",
      `p-valor` = sw_resultado[1],
      Interpretación = sw_resultado[2],
      stringsAsFactors = FALSE
    ))
    
    # Agregar resultados de Lilliefors
    ks_resultado <- interpretar_resultado(resultados_test$Lilliefors_KS)
    resultados <- rbind(resultados, data.frame(
      Escala = var,
      Índice = "K-S, L",
      `p-valor` = ks_resultado[1],
      Interpretación = ks_resultado[2],
      stringsAsFactors = FALSE
    ))
  }
  
  # Mostrar resultados como tabla
  kable(resultados, format = "markdown", caption = "Transformación Logarítmica de las Escalas")
}


# Ejecutar la función
log_transform_norma(PULCRO)

# El resultado final es una tabla como esta:

"Table: Transformación Logarítmica de las Escalas

|Escala  |Índice |p.valor   |Interpretación |
|:-------|:------|:---------|:--------------|
|SEXTSUM |S-W    |0.00, *** |No Normalidad  |
|SEXTSUM |K-S, L |0.00, *** |No Normalidad  |
|CSUM    |S-W    |0.00, *** |No Normalidad  |
|CSUM    |K-S, L |0.00, *** |No Normalidad  |
|MSUM    |S-W    |0.00, *** |No Normalidad  |
|MSUM    |K-S, L |0.00, *** |No Normalidad  |"