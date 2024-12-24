library(ggplot2)
library(dplyr)
library(RColorBrewer)


chiste <- function(data) {
  # Lista de paletas para colorear los histogramas
  paletas <- c(
    "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", 
    "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", 
    "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
  )
  
  # Convertir paletas en colores hexadecimales
  colores_paletas <- lapply(paletas, function(p) brewer.pal(9, p))
  
  # Iterar sobre cada columna del dataframe
  for (i in seq_along(data)) {
    variable <- data[[i]]
    nombre_variable <- colnames(data)[i]
    
    # Seleccionar la paleta correspondiente (cíclico)
    colores <- colores_paletas[[(i - 1) %% length(colores_paletas) + 1]]
    
    # Calcular media y desviación estándar
    media_var <- mean(variable)
    sd_var <- sd(variable)
    
    # Crear el histograma con ggplot2
    histograma <- ggplot(data = data.frame(variable), aes(x = variable)) +
      geom_histogram(aes(y = ..density..), 
                     bins = 30, 
                     fill = colores[5],  # Color central de la paleta
                     color = "black") +
      stat_function(fun = dnorm, 
                    args = list(mean = media_var, sd = sd_var), 
                    color = "red", size = 1) +
      geom_vline(aes(xintercept = media_var), 
                 color = "blue", linetype = "dashed", size = 1) +
      ggtitle(paste("Histograma de", nombre_variable)) +
      theme_minimal() +
      labs(x = nombre_variable, y = "Densidad")
    
    # Mostrar el gráfico
    print(histograma)
  }
}

# Aplicando la función

chiste(df)