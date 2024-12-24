# Función para ejecutar la prueba Dunn Test
# Usarlo cuando:
  # Querramos comparar las diferencias en una variable dependiente numérica no normal en función de una variable factor de más de dos categorías

library(dunn.test)

dundun <- function(dataframe, columna_datos, columna_grupo) {
  # Extraer las variables de datos y grupo
  datos <- dataframe[[columna_datos]]
  grupo <- dataframe[[columna_grupo]]
  
  # Nombre de la variable numérica
  nombre_variable <- colnames(dataframe)[columna_datos]
  
  # Verificar que las columnas seleccionadas sean válidas
  if (!is.numeric(datos)) {
    stop("La columna de datos debe ser numérica.")
  }
  if (!is.factor(grupo) && !is.character(grupo)) {
    stop("La columna de grupo debe ser categórica (factor o texto).")
  }
  
  # Convertir la columna de grupo a factor si no lo es
  grupo <- as.factor(grupo)
  
  # Ejecutar el test de Dunn con ajuste Bonferroni
  resultado_dunn <- dunn.test(x = datos, g = grupo, method = "bonferroni", kw = FALSE, table = FALSE)
  
  # Extraer comparaciones y p-valores ajustados
  comparaciones <- resultado_dunn$comparisons
  p_valores_ajustados <- resultado_dunn$P.adjusted
  
  # Clasificar el nivel de significancia
  significacion <- ifelse(p_valores_ajustados < 0.001, "***",
                          ifelse(p_valores_ajustados < 0.01, "**",
                                 ifelse(p_valores_ajustados < 0.05, "*", "NS")))
  
  # Determinar si hay diferencias reales probables
  diferencia_real <- ifelse(p_valores_ajustados < 0.05, "Sí", "No")
  
  # Crear el data frame final (sin el p-valor ajustado)
  resultado_final <- data.frame(
    Variable = nombre_variable,
    Comparacion = comparaciones,
    Significacion = significacion,
    Diferencia_Real_Probable = diferencia_real,
    stringsAsFactors = FALSE
  )
  
  # Generar el gráfico de boxplot
  cat("\n### Boxplots de la distribución de datos por grupo ###\n")
  boxplot(datos ~ grupo,
          main = paste("Distribución de", nombre_variable, "por grupo"),
          xlab = "Grupo",
          ylab = nombre_variable,
          col = rainbow(length(levels(grupo))),
          border = "black")
  
  # Mostrar la tabla con formato Markdown
  cat("\n### Análisis Dunn de las diferencias estadísticamente significativas ###\n\n")
  print(knitr::kable(resultado_final, format = "markdown"))
}

# Ejemplo de uso

dundun (dataframe = INDI45, columna_datos = 95, columna_grupo = 2)