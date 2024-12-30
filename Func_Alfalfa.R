# Función para Calcular el índice Alpha de Cronbach

library(psych)
library(knitr)

Alfalfa <- function(dataframe, nombre_escala) {
  # Calcular el índice alpha estándar
  alpha_res <- alpha(dataframe)
  std_alpha <- round(alpha_res$total[1, "std.alpha"], 2)  # Redondear std.alpha a 2 decimales
  
  # Inicializar variables para los resultados
  items_a_excluir <- c()
  
  # Calcular alpha al excluir cada ítem
  for (item in colnames(dataframe)) {
    # Calcular alpha sin el ítem actual
    alpha_sin_item <- alpha(dataframe[, !(colnames(dataframe) %in% item)])
    std_alpha_sin_item <- round(alpha_sin_item$total[1, "std.alpha"], 2)  # Redondear std.alpha a 2 decimales
    alpha_diff <- std_alpha_sin_item - std_alpha
    
    # Evaluar si el alpha mejora más de 0.05
    if (alpha_diff >= 0.05) {
      items_a_excluir <- c(items_a_excluir, item)
    }
  }
  
  # Preparar los valores finales para la tabla
  cantidad_items <- length(items_a_excluir)
  if (cantidad_items == 0) {
    items_a_excluir <- "no hay ítems cuya exclusión incremente alpha en más de 0.05 puntos"
  }
  
  # Crear la tabla de resultados
  tabla_resultados <- data.frame(
    `Std.Alpha` = std_alpha,
    `Cantidad Items Prescindibles` = ifelse(cantidad_items == 0, 0, cantidad_items),
    `Items` = paste(items_a_excluir, collapse = ", ")
  )
  
  # Mostrar la tabla con título usando kable
  kable(tabla_resultados, 
        caption = paste("Confiabilidad de la Escala", nombre_escala))
}

# Ejemplo de uso
# dataframe <- data.frame(...)  # Reemplazar con tu dataframe real
# Alfalfa(dataframe, "Nombre de la Escala")
