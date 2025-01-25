# Función para generar un análisis porcentual de resumen de frecuencia de respuesta "0" ó "1" en una serie de ítems

# Caso: Tengo una serie de ítems cuya respuesta es Sí/No, 0/1, M/F, etc., donde me interesa solo uno de los casos (1 pero no 0, Sí, mas no No, etc) y necesito generar una tabla y un gráfico que agregue y resuma los casos

# Salida: Es algo así como (tb bota un gráfico porcentual de barras descendentes, no se incluye aquí porque el script no lo soporta):

"Table: Resumen de Tratamientos

|Variable   | Porcentaje (%)|
|:----------|--------------:|
|RTTasipsi  |           3.24|
|RTTasifon  |           1.24|
|RTTasiped  |           1.12|
|RTTdifdiag |           0.79|
|RTTasipsim |           0.75|
|RTTdisc    |           0.46|
|RTTasipsiq |           0.04|"

# 0. Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(RColorBrewer)

# Función 
anal_di <- function(data, 
                    rango_indices, 
                    titulo_tabla = "Tabla de porcentajes de 'Sí' (1)", 
                    titulo_grafico = "Porcentajes de 'Sí' (1) por variable", 
                    color_contorno = "black", 
                    paleta_colores = "Dark2") {
  # Seleccionar las variables según el rango
  sub_data <- data[, rango_indices]
  
  # Verificar que las columnas sean dicotómicas
  if (!all(apply(sub_data, 2, function(x) all(x %in% c(0, 1))))) {
    stop("Todas las columnas seleccionadas deben ser dicotómicas (valores 0 y 1).")
  }
  
  # Calcular los porcentajes de "1" para cada variable
  tabla_porcentajes <- sub_data %>%
    summarise(across(everything(), ~ mean(.x) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Porcentaje") %>%
    arrange(desc(Porcentaje)) %>%
    mutate(Porcentaje = round(Porcentaje, 2)) # Redondear a 2 decimales
  
  # Mostrar la tabla en formato Markdown usando kable
  tabla_md <- tabla_porcentajes %>%
    kable("markdown", col.names = c("Variable", "Porcentaje (%)"), caption = titulo_tabla) 
  # %>%
  #   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
  
  print(tabla_md)
  
  # Crear el gráfico de barras
  grafico <- ggplot(tabla_porcentajes, aes(x = reorder(Variable, -Porcentaje), y = Porcentaje, fill = Variable)) +
    geom_col(width = 0.75, color = color_contorno) +  # Agregar línea de contorno
    geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
    scale_fill_brewer(palette = paleta_colores) +  # Paleta definida por el usuario
    labs(
      title = titulo_grafico,
      x = "Variables",
      y = "Porcentaje",
      caption = "Fuente: Data Niveles 4-5"
    ) +
    theme_gray() +  # Cambiar tema a theme_gray
    theme(legend.position = "none")
  
  print(grafico)
}


# Ejemplo de uso



anal_di(data = INDI45, # df de donde sale la data
        rango_indices = c(22:30), # columnas que contienen las variables dicotómicas
        titulo_tabla = "Resumen de Incidencias", 
        titulo_grafico = "Porcentajes de Incidencia", 
        color_contorno = "black", # Especialmente útil si son muchas columnas y la paleta incluye colores muy claros
        paleta_colores = "Greens")