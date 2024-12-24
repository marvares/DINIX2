library(ggplot2)
library(dplyr)
library(RColorBrewer)

graficar_porcentajes <- function(variable, data, 
                                 titulo = "Distribución porcentual", 
                                 etiqueta_x = "Categorías", 
                                 etiqueta_y = "Porcentaje", 
                                 pie_de_pagina = "Fuente: Datos internos. Elaboración propia", 
                                 paleta_colores = "Dark2") {
  # Validar que la variable es un factor
  if (!is.factor(data[[deparse(substitute(variable))]])) {
    stop("La variable debe ser de tipo factor. Por favor, conviértela antes de usar esta función.")
  }
  
  # Calcular los porcentajes
  datos_porcentajes <- data %>%
    count({{ variable }}) %>%
    mutate(porcentaje = n / sum(n) * 100)
  
  # Redondear los porcentajes
  datos_porcentajes$porcentaje <- round(datos_porcentajes$porcentaje, 2)
  
  # Crear el gráfico de barras con porcentajes
  ggplot(datos_porcentajes, aes(x = {{ variable }}, y = porcentaje, fill = {{ variable }})) +
    geom_col(width = 0.75) +
    geom_text(aes(label = paste0(porcentaje, " %")), vjust = 1.2) +
    scale_fill_brewer(palette = paleta_colores) +
    labs(
      title = titulo,
      x = etiqueta_x,
      y = etiqueta_y,
      caption = pie_de_pagina
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Ejemplo de aplicación básica:
# Supongamos que tu dataframe se llama INDI45 y tu variable es Fechin
graficar_porcentajes(
  variable = Fechin, 
  data = INDI45
)
# Ejemplo aplicación personalizada
graficar_porcentajes(
  variable = Fechin, 
  data = INDI45, 
  titulo = "Participantes según Fecha de Evaluación", 
  etiqueta_x = "Fecha de Evaluación", 
  etiqueta_y = "Porcentaje de Participantes", 
  pie_de_pagina = "Fuente: Análisis propio", 
  paleta_colores = "Set3"
)
