# Función para calcular e interpretar correlaciones Spearman
# Instalar paquetes necesarios si no están instalados
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")

# Función para calcular y analizar las correlaciones de Spearman
corre_anal <- function(data, variables_X, variable_Y) {
  resultados <- data.frame(Variable_X = character(), Rho = numeric(), Magnitud = character(), 
                           Significacion = character(), stringsAsFactors = FALSE)
  
  # Iterar sobre las variables X
  for (var in variables_X) {
    test <- cor.test(data[[var]], data[[variable_Y]], method = "spearman")
    rho <- test$estimate
    p_value <- test$p.value
    
    # Determinar la magnitud de rho
    magnitud <- ifelse(abs(rho) > 0.7, "Muy fuerte",
                       ifelse(abs(rho) > 0.5, "Fuerte",
                              ifelse(abs(rho) > 0.3, "Moderada",
                                     ifelse(abs(rho) > 0.2, "Débil",
                                            ifelse(abs(rho) > 0.1, "Muy débil", "Nula")))))
    
    # Determinar la significación estadística
    significacion <- ifelse(p_value < 0.001, "***",
                            ifelse(p_value < 0.01, "**",
                                   ifelse(p_value < 0.05, "*", "NS")))
    
    # Agregar los resultados a la tabla
    resultados <- rbind(resultados, data.frame(
      Variable_X = colnames(data)[var],
      Rho = round(rho, 2),  # Redondear a dos decimales
      Magnitud = magnitud,
      Significacion = significacion
    ))
  }
  
  # Crear la tabla en formato Markdown
  markdown_tabla <- paste0(
    "| Variable X        | Índice Rho | Magnitud     | Significación |\n",
    "|-------------------|------------|--------------|---------------|\n"
  )
  for (i in 1:nrow(resultados)) {
    fila <- resultados[i, ]
    # Alinear Rho a la derecha añadiendo espacios si es necesario
    rho_formateado <- sprintf("%6.2f", fila$Rho)
    markdown_tabla <- paste0(markdown_tabla,
                             "| ", fila$Variable_X, 
                             " | ", rho_formateado, 
                             " | ", fila$Magnitud, 
                             " | ", fila$Significacion, "         |\n")
  }
  
  # Generar interpretaciones textuales
  no_significativas <- resultados[resultados$Significacion == "NS", ]
  significativas <- resultados[resultados$Significacion != "NS" & resultados$Magnitud != "Nula", ]
  significativas_nulas <- resultados[resultados$Significacion != "NS" & resultados$Magnitud == "Nula", ]
  
  texto <- ""
  
  # Correlaciones no significativas
  if (nrow(no_significativas) > 0) {
    texto <- paste(texto, "La(s) siguiente(s) correlación(es) no fueron estadísticamente significativas:")
    texto <- paste(texto, paste(no_significativas$Variable_X, collapse = ", "), ".\n")
  }
  
  # Correlaciones significativas pero con magnitud nula
  if (nrow(significativas_nulas) > 0) {
    texto <- paste(texto, "Por otro lado, la(s) siguiente(s) correlación(es), si bien técnicamente significativa(s), son de magnitud tan pequeña que no indican ninguna asociación en la práctica:")
    texto <- paste(texto, paste(significativas_nulas$Variable_X, collapse = ", "), ".\n")
  }
  
  # Correlaciones significativas con interpretación
  if (nrow(significativas) > 0) {
    texto <- paste(texto, "En contraste, las siguientes correlaciones sí resultaron estadísticamente significativas:")
    texto <- paste(texto, paste(significativas$Variable_X, collapse = ", "), ".\n\n")
    
    texto <- paste(texto, "A continuación, pasaremos a analizar el detalle de las correlaciones estadísticamente significativas.\n")
    for (i in 1:nrow(significativas)) {
      detalle <- significativas[i, ]
      signo <- ifelse(detalle$Rho > 0, "directa", "inversa")
      interpretacion <- paste0(
        "- La asociación entre ", detalle$Variable_X, " y ", colnames(data)[variable_Y],
        " es ", detalle$Magnitud, " y ", signo, 
        ". Esto implica que a mayor puntaje en ", detalle$Variable_X, 
        ", la persona suele tener ", ifelse(signo == "directa", "mayores", "menores"),
        " puntajes en ", colnames(data)[variable_Y], ".\n"
      )
      texto <- paste(texto, interpretacion, "\n")
    }
  }
  
  # Imprimir la tabla en formato Markdown y las interpretaciones
  cat(markdown_tabla, "\n")
  cat(texto, "\n")
}

# Probar la función con el conjunto de datos ficticio
# OJO, EL DF DEBE ESTAR LIMPIO, CON LAS VARIABLES NUMÉRICAS DEL CASO Y SIN NA'S.
corre_anal(data_ficticia, variables_X = 2:5, variable_Y = 6)