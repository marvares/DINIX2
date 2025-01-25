prueba_mann_whitney <- function(data, var_numerica, var_factor) {
  # Extraer los nombres de las variables
  var_numerica_name <- deparse(substitute(var_numerica))
  var_factor_name <- deparse(substitute(var_factor))
  
  # Verificar que la variable categórica tenga exactamente 2 niveles
  niveles <- unique(data[[var_factor]])
  if (length(niveles) != 2) {
    stop("La variable factor debe tener exactamente dos niveles para realizar la prueba.")
  }
  
  # Dividir los datos en los dos grupos
  grupo1 <- data[[var_numerica]][data[[var_factor]] == niveles[1]]
  grupo2 <- data[[var_numerica]][data[[var_factor]] == niveles[2]]
  
  # Ejecutar la prueba de Mann-Whitney-Wilcoxon
  wilcox_result <- wilcox.test(grupo1, grupo2, exact = FALSE, correct = TRUE)
  
  # Calcular la diferencia de promedios
  diff_promedio <- mean(grupo1) - mean(grupo2)
  
  # Determinar la significancia del p-valor
  p_value <- wilcox_result$p.value
  p_label <- if (p_value <= 0.001) {
    paste0(round(p_value, 3), " ***")
  } else if (p_value <= 0.01) {
    paste0(round(p_value, 3), " **")
  } else if (p_value <= 0.05) {
    paste0(round(p_value, 3), " *")
  } else {
    paste0(round(p_value, 3), " NS")
  }
  
  # Interpretación
  interpretacion <- if (p_value <= 0.05) {
    "Diferencia estadísticamente significativa"
  } else {
    "No se encontró una diferencia estadísticamente significativa"
  }
  
  # Crear la tabla de resultados
  resultados <- tibble(
    `Grupos Analizados` = paste(niveles[1], "-", niveles[2]),
    `p-valor` = p_label,
    `Dif. Prom.` = round(diff_promedio, 2),
    Interpretación = interpretacion
  )
  
  # Retornar la tabla en formato Markdown
  resultados_kable <- kable(resultados, format = "markdown", align = "c")
  return(resultados_kable)
}


# Ejecutar la función
prueba_mann_whitney(PULCRO, "Escala_Cog.", "Area")
