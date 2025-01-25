# Función para extraer la data numérica sin NA's de un df

ayudin <- function(dataframe, columnas) {
  # Verificar que las columnas seleccionadas sean numéricas
  if (!all(sapply(columnas, function(col) is.numeric(dataframe[[col]])))) {
    stop("Todas las columnas seleccionadas deben ser numéricas.")
  }
  
  # Filtrar el dataframe eliminando filas con NA en las columnas seleccionadas
  dataframe_limpio <- dataframe[complete.cases(dataframe[, columnas]), columnas]
  
  # Devolver el dataframe limpio
  return(dataframe_limpio)
}

# Not necessarily, but oftentimes one should transform all relevant columns to numeric in order for the function to work properly. So this is how to do just that:

# Convertir columnas de la 3 a la 6 en un dataframe a numéricas (perform this *before* running the function)
INDI45[ , 43:104] <- lapply(INDI45[ , 43:104], as.numeric)

# Llamar a la función y mostrar los resultados
LIMPIO <- ayudin(dataframe = INDI45, columnas = c(43:105))