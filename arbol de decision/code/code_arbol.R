library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)

casen_mod <- read_csv("arbol de decision/data/casen_mod.csv") %>%
  filter(region == 9) %>%
  mutate(ing_nivel = as.factor(ing_nivel),
         sexo = as.factor(sexo),
         ing_baj = as.factor(if_else(ingreso <= 4*10^5, 1, 0)),
         ing_alt = as.factor(if_else(ingreso >= 10^6, 1, 0)),
         nac = as.factor(nac),
         prof = as.factor(prof))

# casen_mod$ing_nivel=as.factor(casen_mod$ing_nivel)
# casen_mod$sexo=as.factor(casen_mod$sexo)
# 
# casen_mod <- casen_mod %>%
#   mutate(ing_baj = as.factor(if_else(ingreso <= 4*10^5, 0, 1)),
#          ing_alt = as.factor(if_else(ingreso >= 10^6, 0, 1)))


arbol_casen=rpart(ing_nivel~esc,casen_mod,parms=list(split = "information"))

rpart.plot(arbol_casen,type=5, extra=106)

arbol_casen=rpart(ing_baj~esc+sexo,casen_mod,parms=list(split = "information"))

rpart.plot(arbol_casen,type=5, extra=106)

arbol_casen=rpart(ing_alt~esc+sexo+edad,casen_mod,parms=list(split = "information"))

rpart.plot(arbol_casen,type=5, extra=106)

#################################################################
arbol <- function(eq, data) {
  library(rpart)
  library(rpart.plot)
  arbol <- rpart(eq, data, parms = list(split = "information"))
  plot <- rpart.plot(arbol, type=5, extra=106)
  return(plot)
}

arbol(ing_alt~esc, casen_mod)


#######################################
indice = c(1:nrow(casen_mod))
index_cal = sample(indice, (2/3)*length(indice))
muestra_cal = casen_mod[index_cal,] 
muestra_val = casen_mod[-index_cal,] 

summary(casen_mod)

arbol_casen=rpart(ing_alt~nac+sexo+edad+edu, muestra_cal,parms=list(split = "information"))
arbol_casen=rpart(ing_baj~nac+sexo+edad+edu, muestra_cal,parms=list(split = "information"))
arbol_casen=rpart(ing_alt~nac+sexo+edad+edu, muestra_val,parms=list(split = "information"))
arbol_casen=rpart(ing_baj~nac+sexo+edad+edu, muestra_val,parms=list(split = "information"))

arbol_casen=rpart(ing_alt~nac+sexo+edad+edu, muestra_cal,parms=list(split = "gini"))
arbol_casen=rpart(ing_baj~nac+sexo+edad+edu, muestra_cal,parms=list(split = "gini"))
arbol_casen=rpart(ing_alt~nac+sexo+edad+edu, muestra_val,parms=list(split = "gini"))
arbol_casen=rpart(ing_baj~nac+sexo+edad+edu, muestra_val,parms=list(split = "gini"))

rpart.plot(arbol_casen,type=5, extra=106)
summary(arbol_casen)

predict(arbol_casen, muestra_cal, type = "class")







