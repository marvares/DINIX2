# Definir la función para calcular indicadores estadísticos descriptivos
# Cargar la librería necesaria
library(knitr)

# Definir la función para calcular indicadores estadísticos descriptivos
TED <- function(df) {
  # Crear un data frame para almacenar los resultados
  resultados <- data.frame(
    Variable = character(),
    Mediana = numeric(),
    Media = numeric(),
    `Desviación Estándar` = numeric(),
    `Número de Casos` = integer(),
    stringsAsFactors = FALSE
  )
  
  # Iterar sobre cada columna del dataframe
  for (nombre_columna in colnames(df)) {
    # Calcular la mediana, media, desviación estándar y número de casos
    mediana <- round(median(df[[nombre_columna]]), 2)
    media <- round(mean(df[[nombre_columna]]), 2)
    desviacion_estandar <- round(sd(df[[nombre_columna]]), 2)
    numero_casos <- length(df[[nombre_columna]])
    
    # Agregar los resultados al data frame
    resultados <- rbind(resultados, data.frame(
      Variable = nombre_columna,
      Mediana = mediana,
      Media = media,
      `Desviación Estándar` = desviacion_estandar,
      `Número de Casos` = numero_casos
    ))
  }
  
  return(kable(resultados, format = "markdown"))
}



# Llamar a la función y mostrar los resultados
TED_C3 <- TED(INDIGO_C3)
print(TED_C3)