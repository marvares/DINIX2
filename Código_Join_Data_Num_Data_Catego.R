# Para añadir data categorial a un df que solo contiene ítems u otra data meramente numérica. 

# 1. Extraigo los campos categóricos que me interesan del df original
library(dplyr)
INDI45_CATEGO <- INDI45 %>% 
  dplyr::select(Codigo, Regnat, Reg)

# 2. Si es necesario, transformo los valores correspondientes a factor
# 2.a. Ejemplo de cómo pasar a factor reetiquetando las categorías
INDI45_CATEGO$Regnat <- factor(INDI45_CATEGO$Regnat, 
                               levels = c("1", "2", "3"),  # Valores originales
                               labels = c("Costa", "Sierra", "Selva"))  # Nuevas etiquetas
# 2.b. Ejemplo de cómo pasar a factor de frente, sin cambiar los valores ya presentes
# Cambiar la variable "Regnat" a factor y reetiquetar sus valores
INDI45_CATEGO$Reg <- factor(INDI45_CATEGO$Reg) 

# 3. Extraigo la data numérica de interés, conservando el ID, para poder luego hacer el join (acá uso la función "ayudín", que extrae solo data numérica y sin NA's de un df, documentada en otro script)

PRELIMPIO <- ayudin(dataframe = INDI45, columnas = c(1, 43:105)) # Extraigo solo valores no NA y numéricos del df original, incluyendo el ID (primera columna)

# 4. Hago el join entre la data numérica con ID y la data categorial

# Realizar el left join
LIMPIO <- left_join(PRELIMPIO, INDI45_CATEGO, by = "Codigo")

