# Librerías necesarias
library(dplyr)
library(nortest)  # Para K-S Lilliefors
library(knitr)    # Para kable

Norma <- function(data) {
  # Función para calcular el nivel de significancia y formatear el resultado
  interpretar_resultado <- function(p) {
    if (is.na(p)) return("NA")
    nivel_significancia <- ifelse(p <= 0.001, "***",
                                  ifelse(p <= 0.01, "**",
                                         ifelse(p <= 0.05, "*", "NS")))
    interpretacion <- ifelse(p > 0.05, "Se puede asumir normalidad", "No se puede asumir normalidad")
    paste0("p = ", sprintf("%.2f", p), " ", nivel_significancia, " (", interpretacion, ")")
  }
  
  # Lista para almacenar resultados
  resultados <- list()
  
  # Iterar sobre cada columna del dataframe
  for (var in colnames(data)) {
    variable <- data[[var]]
    
    # Calcular los p-valores
    shapiro_p <- ifelse(length(variable) > 2, shapiro.test(variable)$p.value, NA)
    # ks_p <- ifelse(length(variable) > 1, ks.test(variable, "pnorm", mean(variable), sd(variable))$p.value, NA)
    ks_p <- ifelse(length(variable) > 1, nortest::lillie.test(variable)$p.value, NA)
    
    # ad_p <- ifelse(length(variable) > 1, ad.test(variable)$p.value, NA)
    
    # Interpretar y formatear los resultados
    shapiro_resultado <- interpretar_resultado(shapiro_p)
    ks_resultado <- interpretar_resultado(ks_p)
    # ad_resultado <- interpretar_resultado(ad_p)
    
    # Agregar resultados formateados a la lista
    resultados[[var]] <- c(
      Shapiro_Wilk = shapiro_resultado,
      Lilliefors_KS = ks_resultado
      # Anderson_Darling = ad_resultado
    )
  }
  
  # Convertir la lista en un dataframe
  resultados_df <- as.data.frame(do.call(rbind, resultados), stringsAsFactors = FALSE)
  rownames(resultados_df) <- colnames(data)
  
  # Personalizar la salida con kable
  resultados_df %>%
    kable(format = "markdown", align = "rll", caption = "Resultados de los tests de normalidad (S-W, Lilliefors K-S)")
}

# Ejemplo de uso con un dataframe "df"
Norma(AGEMO)