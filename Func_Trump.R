# Función para generar el índice de Discriminación de una serie de ítems. Esto sirve para determinar qué tanto los ítems individuales discriminan entre los evaluados. El análisis discriminante en el contexto de la evaluación psicométrica de ítems de una escala se refiere a la capacidad de cada ítem para diferenciar adecuadamente entre los individuos que tienen puntuaciones altas y bajas en la variable o constructo que la escala intenta medir. Un ítem tiene alta capacidad discriminativa cuando las personas que obtienen puntuaciones globales altas en la escala tienden a responder al ítem de manera consistente con el constructo medido (por ejemplo, seleccionando respuestas más extremas o acordes a la medición) y, al mismo tiempo, las personas con puntuaciones bajas responden de manera opuesta.

Trump <- function(nombre_escala, df_final, df_inicial, prefijo, rango_columnas) {
  library(knitr)
  library(dplyr)
  
  # Ordenar los datos según el puntaje total
  df_final <- df_inicial[order(df_inicial$CSUM, decreasing = TRUE), ]
  
  # Definir el tamaño de los grupos (27% superior e inferior)
  n <- nrow(df_final)
  grupo <- round(0.27 * n)
  
  # Identificar las columnas de los ítems (según el rango especificado)
  items <- names(df_final)[rango_columnas]
  
  # Transformar las respuestas en dicotómicas (1 = alta, 0 = baja)
  df_final[items] <- lapply(df_final[items], function(x) ifelse(x >= 4, 1, 0))
  
  # Dividir los grupos después de la transformación
  grupo_alto <- df_final[1:grupo, ]
  grupo_bajo <- df_final[(n - grupo + 1):n, ]
  
  # Calcular el índice de discriminación para cada ítem
  indices_discriminacion <- sapply(items, function(item) {
    # Proporción de respuestas altas en el grupo alto
    p_alto <- mean(grupo_alto[[item]])
    # Proporción de respuestas altas en el grupo bajo
    p_bajo <- mean(grupo_bajo[[item]])
    # Índice de discriminación
    p_alto - p_bajo
  })
  
  # Redondear los índices a 2 dígitos
  indices_discriminacion <- round(indices_discriminacion, 2)
  
  # Crear una tabla con la interpretación
  interpretacion <- sapply(indices_discriminacion, function(d) {
    if (d >= 0.40) {
      "Excelente discriminación"
    } else if (d >= 0.30) {
      "Buena discriminación"
    } else if (d >= 0.20) {
      "Discriminación aceptable"
    } else if (d >= 0) {
      "Mala discriminación"
    } else {
      "Discriminación inversa"
    }
  })
  
  # Crear el data frame final
  tabla_discriminacion <- data.frame(
    Ítem = items,
    Índice.D = indices_discriminacion,
    Interpretación = interpretacion
  )
  
  # Generar la tabla en Markdown
  cat("Table 1: Índices de Discriminación ", nombre_escala, "\n\n", sep = "")
  print(
    kable(tabla_discriminacion, format = "markdown", align = c("l", "r", "l"), row.names = FALSE)
  )
  
  # Retornar la tabla (opcional, para que el usuario pueda manipularla)
  return(tabla_discriminacion)
}

# Llamar a la función
tabla <- Trump(
  nombre_escala = "Escala C",
  df_final = DISCRIC, 
  df_inicial = DISCRI, 
  prefijo = "C",
  rango_columnas = 2:27  # Columnas de los ítems
)

