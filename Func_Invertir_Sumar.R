library(tidyverse)

invertir_y_sumar <- function(df, indices_inversos, nombre_variable_sumatoria, posicion_variable) {
  
  # 1. Convertir columnas 75:88 a numéricas
  df <- df %>%
    mutate(across(all_of(names(df)[75:88]), as.numeric))
  
  # 2. Invertir los ítems indicados
  df <- df %>%
    mutate(across(all_of(names(df)[indices_inversos]), ~ 7 - .x))
  
  # 3. Crear la variable sumatoria
  df <- df %>%
    mutate(!!nombre_variable_sumatoria := rowSums(across(all_of(names(df)[75:88]))))
  
  # 4. Reubicar la variable sumatoria en la posición deseada
  col_order <- names(df)  # Obtener nombres de columnas
  
  if (posicion_variable <= length(col_order)) {
    nueva_posicion <- append(col_order, nombre_variable_sumatoria, after = posicion_variable - 1)
    df <- df %>% select(all_of(nueva_posicion))
  }
  
  return(df)
}

# Ejemplo
# Aplicar la función con los valores indicados
INDI45_i <- invertir_y_sumar(
  df = INDI45,
  indices_inversos = c(77:80, 84:88),  # Ítems inversos
  nombre_variable_sumatoria = "SSUM",  # Nombre de la variable sumatoria
  posicion_variable = 104  # Ubicación deseada
)