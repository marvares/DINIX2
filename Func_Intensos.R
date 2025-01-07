# Función para identificar casos con valores atípicos (outliers) en columnas específicas
items_intensos <- function(data, rango_columnas, columna_contexto, multiplicador_IQR = 1.5) {
  # Verificar que las columnas estén en el rango
  columnas_seleccionadas <- names(data)[rango_columnas]
  
  # Identificar columnas no numéricas
  columnas_no_numericas <- columnas_seleccionadas[!sapply(data[, rango_columnas, drop = FALSE], is.numeric)]
  
  # Excluir las columnas no numéricas y avisar
  columnas_a_evaluar <- setdiff(columnas_seleccionadas, columnas_no_numericas)
  if (length(columnas_no_numericas) > 0) {
    cat("Las siguientes columnas no son numéricas y serán excluidas del análisis:\n")
    print(columnas_no_numericas)
  }
  
  # Inicializar una tabla consolidada
  resultados_outliers <- data.frame(
    Columna = character(),
    Fila = integer(),
    Valor_Outlier = numeric(),
    Contexto = character()
  )
  
  # Aplicar la detección de outliers para cada columna numérica
  for (columna in columnas_a_evaluar) {
    # Obtener los datos de la columna
    valores <- data[[columna]]
    
    # Calcular los cuartiles y los límites del IQR
    Q1 <- quantile(valores, 0.25, na.rm = TRUE)
    Q3 <- quantile(valores, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    limite_inferior <- Q1 - multiplicador_IQR * IQR
    limite_superior <- Q3 + multiplicador_IQR * IQR
    
    # Identificar las filas con outliers
    filas_outliers <- which(valores < limite_inferior | valores > limite_superior)
    
    # Crear un data frame con los resultados para esta columna
    if (length(filas_outliers) > 0) {
      resultados <- data.frame(
        Columna = columna,
        Fila = filas_outliers,
        Valor_Outlier = valores[filas_outliers],
        Contexto = data[[columna_contexto]][filas_outliers]
      )
      
      # Consolidar los resultados
      resultados_outliers <- rbind(resultados_outliers, resultados)
    }
  }
  
  # Generar una tabla en Markdown
  if (nrow(resultados_outliers) > 0) {
    cat("### Tabla consolidada de casos con valores atípicos\n")
    kable(resultados_outliers, format = "markdown", row.names = FALSE)
  } else {
    cat("No se detectaron valores atípicos en las columnas evaluadas.\n")
  }
}

# EJEMPLO
# Ejecutar la función para encontrar casos con valores atípicos
items_intensos(
  data = LIMPIO,           # Data frame con los datos
  rango_columnas = 34:47,     # Rango de columnas numéricas a evaluar (columnas 2 a 4)
  columna_contexto = "Regnat",  # Columna adicional para incluir en la salida (contexto)
  multiplicador_IQR = 3   # Multiplicador del IQR (por defecto 1.5)
)
