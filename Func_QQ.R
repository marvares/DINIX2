# Función para generar los gráficos QQ

dont_mess_with_my_qq <- function(data, columnas, nombres_escalas) {
  # Validaciones iniciales
  if (length(columnas) != length(nombres_escalas)) {
    stop("El número de columnas debe coincidir con el número de nombres de escalas.")
  }
  
  # Cargar ggplot2 si no está disponible
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  # Crear gráficos para cada escala
  for (i in seq_along(columnas)) {
    col_index <- columnas[i]
    escala_nombre <- nombres_escalas[i]
    
    # Validar si la columna existe
    if (col_index > ncol(data) || col_index < 1) {
      stop(paste("La columna", col_index, "no existe en el dataframe."))
    }
    
    # Extraer la columna
    col_data <- data[[col_index]]
    
    # Generar el gráfico Q-Q y almacenarlo en una variable
    plot <- ggplot(data.frame(valores = col_data), aes(sample = valores)) +
      stat_qq() +
      stat_qq_line(color = "blue", linetype = "dashed") +
      labs(
        title = paste("Gráfico Q-Q de la ", escala_nombre),
        x = "Cuantiles teóricos",
        y = "Cuantiles muestrales"
      ) +
      theme_minimal()
    
    # Mostrar el gráfico
    print(plot)
  }
}




# Usamos la función

dont_mess_with_my_qq (
  subes,
  columnas = 8:10,
  nombres_escalas= c("Escala Cognitiva", "Escala Socioemocional", "Escala Disposicional")
)