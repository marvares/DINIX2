
library(haven)

# Carga el archivo .sav
INDI45 <- read_sav("INDI45.sav")
View(INDI45)

install.packages("kableExtra")

glimpse(INDI45)
is.factor(INDI45$Fechin)
