# Función para ejecutar transformación de data mediante BoxCox
# Cargar librerías necesarias
library(MASS)       # Para la transformación Box-Cox
library(nortest)    # Para el test de Lilliefors (K-S corregido)
library(knitr)      # Para mostrar tablas en formato markdown

# Función para aplicar la transformación Box-Cox y evaluar normalidad
box_norma <- function(data) {
  # Cargar la librería MASS si no está ya cargada
  if (!requireNamespace("MASS", quietly = TRUE)) {
    install.packages("MASS")
  }
  library(MASS)
  
  # Verificar que las columnas sean numéricas
  data <- data[, sapply(data, is.numeric)]
  
  # Aplicar la transformación Box-Cox
  box_transformada <- as.data.frame(lapply(data, function(x) {
    if (any(x <= 0)) return(rep(NA, length(x))) 
    else {
      bc <- MASS::boxcox(x ~ 1, plotit = FALSE)
      lambda <- bc$x[which.max(bc$y)]
      if (lambda == 0) return(log(x))
      else return((x^lambda - 1) / lambda)
    }
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
  for (var in colnames(box_transformada)) {
    variable <- box_transformada[[var]]
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
  kable(resultados, format = "markdown", caption = "Transformación Box-Cox de las Escalas")
}

# Aplicando la función:

box_norma(PULCRO)

# El resultado es algo parecido a esto:
"Table: Transformación Box-Cox de las Escalas

|Escala  |Índice |p.valor   |Interpretación |
|:-------|:------|:---------|:--------------|
|SEXTSUM |S-W    |0.00, *** |No Normalidad  |
|SEXTSUM |K-S, L |0.00, *** |No Normalidad  |
|CSUM    |S-W    |0.00, *** |No Normalidad  |
|CSUM    |K-S, L |0.00, *** |No Normalidad  |
|MSUM    |S-W    |0.00, *** |No Normalidad  |
|MSUM    |K-S, L |0.00, *** |No Normalidad  |"