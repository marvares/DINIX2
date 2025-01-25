# Función para ejecutar un Análisis Factorial Confirmatorio, obtener una tabla con los resultados de los principales indicadores de bondad de ajuste de frente (la tabla final excluye chi cuadrado, pues casi siempre ese indicador sale sobrando en muestras mínimanete amplias)

AFC <- function(data, factors, indicators, result_object_name, scale_name) {
  # Verificar que la longitud de `factors` coincida con la longitud de `indicators`
  if (length(factors) != length(indicators)) {
    stop("El número de factores debe coincidir con el número de listas de indicadores.")
  }
  
  # Generar la sintaxis del modelo automáticamente
  modelo <- ""
  for (i in seq_along(factors)) {
    factor_name <- factors[i]
    column_indices <- indicators[[i]]
    
    # Convertir los índices a nombres de las columnas
    variable_names <- colnames(data)[column_indices]
    variables <- paste(variable_names, collapse = " + ")
    modelo <- paste0(modelo, factor_name, " =~ ", variables, "\n")
  }
  
  # Ajustar el modelo usando lavaan::cfa()
  ajuste <- cfa(modelo, data = data)
  
  # Obtener índices de ajuste relevantes
  fit_indices <- fitmeasures(ajuste, c("rmsea", "cfi", "tli", "srmr", "gfi"))
  
  # Función auxiliar para evaluar el nivel obtenido
  evaluar_indice <- function(indice, valor) {
    case_when(
      indice == "rmsea" & valor <= 0.05 ~ "Excelente",
      indice == "rmsea" & valor <= 0.08 ~ "Aceptable",
      indice == "cfi" & valor >= 0.95 ~ "Excelente",
      indice == "cfi" & valor >= 0.90 ~ "Aceptable",
      indice == "tli" & valor >= 0.95 ~ "Excelente",
      indice == "tli" & valor >= 0.90 ~ "Aceptable",
      indice == "srmr" & valor <= 0.08 ~ "Excelente",
      indice == "srmr" & valor <= 0.10 ~ "Aceptable",
      indice == "gfi" & valor >= 0.95 ~ "Excelente",
      indice == "gfi" & valor >= 0.90 ~ "Aceptable",
      TRUE ~ "Deficiente"
    )
  }
  
  # Crear tabla con resultados (sin la columna con nombres de índices)
  resultados_tabla <- data.frame(
    "Índice Obtenido" = c(fit_indices["rmsea"], 
                          fit_indices["cfi"], 
                          fit_indices["tli"], 
                          fit_indices["srmr"], 
                          fit_indices["gfi"]),
    "Nivel Obtenido" = c(
      evaluar_indice("rmsea", fit_indices["rmsea"]),
      evaluar_indice("cfi", fit_indices["cfi"]),
      evaluar_indice("tli", fit_indices["tli"]),
      evaluar_indice("srmr", fit_indices["srmr"]),
      evaluar_indice("gfi", fit_indices["gfi"])
    )
  )
  
  # Cambiar nombres de las filas para usar como identificadores
  rownames(resultados_tabla) <- c("RMSEA", "CFI", "TLI/NNFI", "SRMR", "GFI")
  
  # Guardar el objeto con los resultados en el entorno global
  assign(result_object_name, ajuste, envir = .GlobalEnv)
  
  # Mostrar título y tabla formateada
  cat("\n### Resumen de Índices de Bondad de Ajuste para el AFC de la", scale_name, "\n\n")
  print(kable(resultados_tabla, align = "c", 
              caption = paste("Resumen de Índices de Bondad de Ajuste para el AFC de la", scale_name)))
}


# Para ejecutar función

# 1. Definición de argumentos


factores <- c("Cognitivo", "Motor", "Socioemocional", "Disposicional") # Nombre de los factores previstos en el modelo
items <- list(c(1:26), c(27:32), c(33:46), c(47:52)) # Declaro qué items forman qué factores
result_object_name <- "afc30_01" # Le pongo nombre al objeto que contendrá el resultado del AFC
escala <- "Escala INDI" # Declaro el nombre de la Escala a Confirmar

# 2. Le especifico a la función cuáles son los argumentos

resultado <- AFC (
  data = LIMPIO, 
  factors = factores, 
  indicators = items, 
  result_object_name = "afc30_01",
  scale_name = escala
)

# 3. Adicionalmente, para generar tabla con las cargas factoriales (esto no es parte de la función, pero...)

# Extraer las cargas factoriales estandarizadas del modelo
cargas <- inspect(afc30_01, what = "std")$lambda

# Convertir la matriz a un data.frame
cargas_df <- as.data.frame(cargas)
cargas_df <- cbind(Item = rownames(cargas_df), cargas_df)

# Identificar columnas con al menos una carga distinta de NA o 0
col_validas <- colSums(!is.na(cargas)) > 0  # Identifica factores con cargas
col_validas <- which(col_validas)           # Obtiene los índices de las columnas válidas

# Filtrar solo las columnas relevantes (ítems y factores con cargas válidas)
cargas_df <- cargas_df[, c(1, col_validas + 1)]  # "+1" porque "Item" está en la columna 1

# Renombrar columnas para mayor claridad
colnames(cargas_df)[-1] <- paste0("Factor", seq_len(ncol(cargas_df) - 1))

# Generar la tabla en formato Markdown con kable sin la columna extra
kable(cargas_df, caption = "Cargas Factoriales Estandarizadas", digits = 3, row.names = FALSE)


# 4. Lo mismo que el paso 3, pero con el texto de cada ítem

library(readxl)
infovar <- read_excel("infovar.xlsx") # Extraigo o tipeo el contenido de c/ítem y lo guardo en un Excel, como una tripa en una sola columna. He notado que funciona bastante bien incluir el número del ítem dentro del contendio, algo así como "C1 Me considero un idiota, ...." etc. La tripa no debe contener el título, puro contenido de ítems y nada más. Le agrego el título después.

colnames(infovar) <- c("Contenido")

# Extraer las cargas factoriales estandarizadas del modelo
cargas <- inspect(afc30_01, what = "std")$lambda

# Convertir la matriz a un data.frame
cargas_df <- as.data.frame(cargas)
cargas_df <- cbind(Item = rownames(cargas_df), cargas_df)

# Identificar columnas con al menos una carga distinta de NA o 0
col_validas <- colSums(!is.na(cargas)) > 0  # Identifica factores con cargas
col_validas <- which(col_validas)           # Obtiene los índices de las columnas válidas

# Filtrar solo las columnas relevantes (ítems y factores con cargas válidas)
cargas_df <- cargas_df[, c(1, col_validas + 1)]  # "+1" porque "Item" está en la columna 1

# Renombrar columnas para mayor claridad
colnames(cargas_df)[-1] <- paste0("Factor", seq_len(ncol(cargas_df) - 1))

# Validar que el número de filas de infovar y cargas_df coincidan
if (nrow(cargas_df) != nrow(infovar)) {
  stop("Error: El número de filas en cargas_df no coincide con el número de filas en infovar.")
}

# Reemplazar la columna "Item" con los valores de "infovar$Contenido"
cargas_df$Item <- infovar$Contenido

# Generar la tabla en formato Markdown con kable
library(knitr)
kable(cargas_df, caption = "Cargas Factoriales Estandarizadas con Contenido", digits = 3, row.names = FALSE)