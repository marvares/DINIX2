library(dplyr)
library(kableExtra)

generar_kruskal <- function(data, rango_var_num, col_factor) {
  # Verificar que la variable factor sea de tipo factor
  if (!is.factor(data[[col_factor]])) {
    stop("La columna especificada como factor debe ser de tipo factor.")
  }
  
  # Extraer nombres de las columnas numéricas
  nombres_var <- colnames(data)[rango_var_num]
  
  # Inicializar lista de resultados
  resultados <- data.frame(Variable = character(),
                           P_Valor = numeric(),
                           Significancia = character(),
                           stringsAsFactors = FALSE)
  
  # Iterar sobre las columnas numéricas
  for (col in nombres_var) {
    tryCatch({
      # Realizar la prueba Kruskal-Wallis
      test <- kruskal.test(data[[col]] ~ data[[col_factor]])
      p_valor <- test$p.value
      
      # Determinar la significancia
      significancia <- ifelse(p_valor < 0.001, "***",
                              ifelse(p_valor < 0.01, "**",
                                     ifelse(p_valor < 0.05, "*", "ns")))
      
      # Agregar resultados al dataframe
      resultados <- rbind(resultados, data.frame(Variable = col,
                                                 P_Valor = round(p_valor, 2),
                                                 Significancia = significancia))
    }, error = function(e) {
      warning(paste("Hubo un problema con la variable:", col, "-", e$message))
    })
  }
  
  # Crear tabla con kable
  tabla <- resultados %>%
    kable("html", col.names = c("Variable", "P-Valor", "Significancia")) %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
  
  return(tabla)
}

# Llamar a la función
tabla_resultado <- generar_kruskal(data = datos, rango_var_num = 2:6, col_factor = 1)

# Mostrar la tabla en RStudio Viewer o un navegador
tabla_resultado
