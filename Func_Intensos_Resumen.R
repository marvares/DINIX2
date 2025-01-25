# Función para resumir outliers, es decir mostrar cuántos casos outliers existen agrupando los casos por ítem
library(dplyr)
library(knitr)

items_intensos_resumen <- function(data, rango_columnas, columna_contexto, multiplicador_IQR = 1.5) {
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
    # Matriz de doble entrada: sumar frecuencia por ítem y valor atípico
    matriz_doble_entrada <- registros_outliers %>%
      group_by(Columna, Valor_Outlier) %>%
      summarise(Frecuencia = n(), .groups = "drop") %>%
      arrange(desc(Frecuencia))  # Ordenar por frecuencia descendente
    
    # Tabla de frecuencias descendentes del contexto
    tabla_frecuencias <- registros_outliers %>%
      count(Contexto, sort = TRUE, name = "Frecuencia")
    
    # Imprimir tablas usando kable
    cat("\n### Resumen de Outliers por ítem\n")
    print(kable(matriz_doble_entrada, format = "markdown", row.names = FALSE))
    
    cat("\n### Tabla de Outliers según Categoría\n")
    print(kable(tabla_frecuencias, format = "markdown", row.names = FALSE))
  } else {
    cat("No se detectaron valores atípicos en las columnas evaluadas.\n")
  }
}


# Llamando a la función:

items_intensos_resumen(
  data = LIMPIO,           # Data frame con los datos
  rango_columnas = 2:53,     # Rango de columnas numéricas a evaluar (columnas 2 a 53)
  columna_contexto = "Regnat",  # Columna adicional para incluir en la salida (contexto)
  multiplicador_IQR = 3   # Multiplicador del IQR (por defecto 1.5)
)

# Ejemplo de resultado


"### Resumen de Outliers por ítem


  |Columna | Valor_Outlier| Frecuencia|
  |:-------|-------------:|----------:|
  |S6      |             6|         30|
  |S12     |             6|         22|
  |S14     |             6|         20|
  "
