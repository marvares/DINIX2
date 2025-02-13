moderation_analysis <- function(data, VI, VM, VD, color_palette) {
  
  # Cargar paquetes necesarios
  if (!requireNamespace("interactions", quietly = TRUE)) install.packages("interactions")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
  if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
  
  library(interactions)
  library(ggplot2)
  library(RColorBrewer)
  library(knitr)
  
  # Verificar que las variables sean numéricas
  if (!is.numeric(data[[VI]]) || !is.numeric(data[[VM]]) || !is.numeric(data[[VD]])) {
    stop("Todas las variables deben ser numéricas.")
  }
  
  # Crear el término de interacción
  data$Interaccion <- data[[VI]] * data[[VM]]
  
  # Ajustar el modelo de regresión
  model <- lm(as.formula(paste(VD, "~", VI, "*", VM)), data = data)
  
  # Obtener los coeficientes y p-valores
  summary_model <- summary(model)
  coef_table <- as.data.frame(summary_model$coefficients)
  colnames(coef_table) <- c("Estimado", "Error Std.", "t-valor", "p-valor")
  coef_table$Término <- rownames(coef_table)
  
  # Formatear p-valores con asteriscos
  coef_table$Significancia <- ifelse(coef_table$`p-valor` < 0.001, "***",
                                     ifelse(coef_table$`p-valor` < 0.01, "**",
                                            ifelse(coef_table$`p-valor` < 0.05, "*", "NS")))
  
  # Seleccionar columnas relevantes
  coef_table <- coef_table[, c("Término", "Estimado", "Significancia")]
  
  # Generar la tabla en Markdown para PDF/HTML
  tabla_md <- kable(coef_table, format = "markdown", digits = 3,
                    caption = paste("Resultados de Moderación entre VI", VI, ", VM", VM, "y VD", VD))
  
  # Retornar tabla en Markdown (se imprimirá automáticamente en RMarkdown)
  print(tabla_md)
  
  # Gráfico de interacción
  plot_title <- paste("Visualización de Efecto de Moderación entre VI", VI, ", VM", VM, "y VD", VD)
  
  interact_plot(model, pred = !!sym(VI), modx = !!sym(VM),
                plot.points = TRUE, colors = brewer.pal(3, color_palette)) +
    theme_gray() +
    ggtitle(plot_title)
}


# Llamar a la función
moderation_analysis(PULCRO, VI = "Edad_Mes", VM = "Quintil_N", VD = "Escala_Cog.", color_palette = "Set1")