# Esta función genera un campo factor de cuatrimestre que reclasifica los casos según el cuatrimestre de nacimiento del participante.
# Los parámetros son: el df y el nombre del campo con la fecha de nacimiento. La fecha de nacimiento debe estar en character y en el formato MM/DD/AAAA.

# Definición de la función
library(dplyr)
en_cuatro <- function(dataframe, columna_fechas) {
  # Convertir la columna de fechas de character a Date
  dataframe[[columna_fechas]] <- as.Date(dataframe[[columna_fechas]], format = "%m/%d/%Y")
  
  # Crear la nueva variable QDOB inmediatamente después de la columna de fechas
  columna_pos <- which(names(dataframe) == columna_fechas) # Obtener la posición de la columna de fechas
  dataframe <- dataframe %>%
    mutate(QDOB = cut(
      as.numeric(format(.data[[columna_fechas]], "%m")),  # Extraer el mes
      breaks = c(0, 3, 6, 9, 12),                        # Definir los límites de los cuatrimestres
      labels = c("1 (Ene-Mar)", "2 (Abr-Jun)", "3 (Jul-Sep)", "4 (Oct-Dic)"),   # Etiquetas para los cuatrimestres
      include.lowest = TRUE                              # Incluir el límite inferior (enero)
    )) %>%
    relocate(QDOB, .after = all_of(columna_fechas))      # Mover QDOB justo después de la columna de fechas
  
  # Convertir la nueva variable a factor (opcional, ya hecho en cut)
  dataframe$QDOB <- as.factor(dataframe$QDOB)
  
  return(dataframe)
}

# Ejemplo de Uso de la función
INDI30 <- en_cuatro(INDI30, "Fecnac")

# Verificar resultados
head(INDI30)