# Como soy un capo, genero función para obtener los casos outliers, y mostrar las frecuencias resumidas por categorías:

items_intensos_catego <- function(data, rango_columnas, columna_contexto, multiplicador_IQR = 1.5) {
  library(dplyr)
  library(knitr)
  
  # Verificar las columnas numéricas dentro del rango
  columnas_seleccionadas <- names(data)[rango_columnas]
  columnas_no_numericas <- columnas_seleccionadas[!sapply(data[, rango_columnas, drop = FALSE], is.numeric)]
  columnas_a_evaluar <- setdiff(columnas_seleccionadas, columnas_no_numericas)
  
  if (length(columnas_no_numericas) > 0) {
    cat("Las siguientes columnas no son numéricas y serán excluidas del análisis:\n")
    print(columnas_no_numericas)
  }
  
  # Crear un data frame vacío para registrar los outliers
  registros_outliers <- data.frame(
    Columna = character(),
    Valor_Outlier = numeric(),
    Contexto = character()
  )
  
  # Identificar los outliers en cada columna numérica
  for (columna in columnas_a_evaluar) {
    valores <- data[[columna]]
    Q1 <- quantile(valores, 0.25, na.rm = TRUE)
    Q3 <- quantile(valores, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    limite_inferior <- Q1 - multiplicador_IQR * IQR
    limite_superior <- Q3 + multiplicador_IQR * IQR
    filas_outliers <- which(valores < limite_inferior | valores > limite_superior)
    
    if (length(filas_outliers) > 0) {
      registros <- data.frame(
        Columna = columna,
        Valor_Outlier = valores[filas_outliers],
        Contexto = data[[columna_contexto]][filas_outliers]
      )
      registros_outliers <- bind_rows(registros_outliers, registros)
    }
  }
  
  if (nrow(registros_outliers) > 0) {
    # Tabla de frecuencias descendentes del contexto
    tabla_frecuencias <- registros_outliers %>%
      count(Contexto, sort = TRUE, name = "Frecuencia")
    
    # Imprimir tabla usando kable
    cat("\n### Tabla de Outliers según Categoría\n")
    print(kable(tabla_frecuencias, format = "markdown", row.names = FALSE))
  } else {
    cat("No se detectaron valores atípicos en las columnas evaluadas.\n")
  }
}

# Ahora la aplico, muy frescamente

items_intensos_catego (
  data = LIMPIO,           # Data frame con los datos
  rango_columnas = 2:53,     # Rango de columnas numéricas a evaluar (columnas 2 a 4)
  columna_contexto = "Reg",  # Columna adicional para incluir en la salida (contexto, o sea categoría)
  multiplicador_IQR = 3   # Multiplicador del IQR (por defecto 1.5)
)

# Ejemplo de la salida:

"### Tabla de Outliers según Categoría


|Contexto | Frecuencia|
|:--------|----------:|
|Costa    |         34|
|Selva    |         19|
|Sierra   |         19|"