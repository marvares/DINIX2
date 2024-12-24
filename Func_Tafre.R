tf <- function(df, variable) {
  # Cálculo de frecuencias
  resultado <- df %>%
    group_by({{ variable }}) %>%
    summarize(
      n = n(),
      porcentaje = round(n / nrow(df) * 100, 2)
    ) %>%
    arrange({{ variable }}) %>%
    mutate(
      n_acum = cumsum(n),
      porcentaje_acum = cumsum(porcentaje)
    ) 
  
  # Cambiamos el nombre de las columnas de la tabla de frecuencias
  colnames(resultado) <- c(colnames(resultado)[1], "N", "%", "N Acum.", "% Acum.")
  
  # Personalizamos el caption con el nombre de la variable
  caption_personalizado <- paste("Frecuencias de", quo_name(enquo(variable)))
  
  # Embellecemos la tabla
  library(knitr)
  print(
    kable(
      resultado, 
      caption = caption_personalizado, 
      align = 'lrrrr', 
      booktabs = TRUE
    )
  )
}