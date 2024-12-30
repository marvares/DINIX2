# Añado y uso función para generar diagramas de caja a pa

library(RColorBrewer)


encaja <- function(dataframe, columnas, titulo = "Boxplots de las Variables", paleta = "Dark2") {
  # Validar si la paleta existe en RColorBrewer
  if (!paleta %in% rownames(brewer.pal.info)) {
    stop("La paleta especificada no existe en RColorBrewer. Por favor, elige una paleta válida.")
  }
  
  # Seleccionar las columnas numéricas especificadas
  datos <- dataframe[, columnas]
  
  # Verificar si todas las columnas seleccionadas son numéricas
  if (!all(sapply(datos, is.numeric))) {
    stop("Todas las columnas seleccionadas deben ser numéricas.")
  }
  
  # Obtener los colores de la paleta
  colores <- brewer.pal(min(length(columnas), brewer.pal.info[paleta, "maxcolors"]), paleta)
  
  # Calcular el espacio necesario para las etiquetas largas
  max_length <- max(nchar(colnames(datos)))
  margen_izquierdo <- max(5, max_length * 0.8) # Ajuste dinámico del margen izquierdo
  
  # Ajustar el margen de la gráfica
  par(mar = c(5, margen_izquierdo, 4, 2)) # c(bottom, left, top, right)
  
  # Crear el gráfico de boxplots
  boxplot(
    datos,
    horizontal = TRUE,       # Boxplots horizontales
    col = colores,           # Colores asignados desde la paleta
    main = titulo,           # Título del gráfico
    las = 1,                 # Rotar las etiquetas para legibilidad
    xlab = "Valores",        # Etiqueta del eje X
    names = colnames(datos)  # Nombres de las variables como etiquetas
  )
}


# Ejemplo de uso:
encaja(dataframe = pre_TED_C3, columnas = 01:07, titulo = "Distribución de las Subescalas DINI", paleta = "Set3")