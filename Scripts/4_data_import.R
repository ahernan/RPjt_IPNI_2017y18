# Script de importacion de datos crudos
# Datos amacenados en .data/ del proyecto

library(readxl)     # Lib para archivos xls

canola <- read_excel("data/canola_phoma.xlsx")
olivo <- read_excel("data/olivo_xylella.xls")
soja <- read_excel("data/soja_mancha.xls")

soja <- read_excel("data/soja.xls")

# Manipulacion: wide - long
library(tidyverse)

x <- c(1, 2, 3, 4)
sqrt(sum(x))

x %>%
  sum %>%
  sqrt 

canola %>%  
  gather(inc_15:inc_248, key = "tt", value = "inc") %>%     # apila registros de columnas
  separate(tt, c(NA, "tiempo"), sep = "_", convert = T) %>% # me quedo con la parte entera del nombre de una columna
  mutate_at(vars(tiempo), list(as.numeric)) -> can_long     # cambio el formato de dato de una columna

can_longa # veo que quedo OK

olivo %>% 
  gather(`1`:`30`, key = "tree", value = "sev") -> oli_longa # apila registros de columnas

oli_longa

soja %>% 
  gather(key = var, value = val, -fungic) %>% 
  separate(var, c('bk', 'x'), sep = '_', convert = TRUE) %>% 
  spread(x, val, convert = TRUE)  %>% 
  mutate_at(vars(fungic:bk), list(as.factor)) -> soja_longa

soja_longa

soja %>% 
  gather(bk_1:bk_4,
         key = bk, 
         value = yield) -> soja_long
  


# olivo <- readxl::read_excel("data/olivo_xylella.xls")
dim(olivo)

olivo %>%  # dataset wide (planilla de campo, con 30 columnas de sev por arbol individual)
  # le pedimos que apile las columnas conteniendo a las plantas 1 a 30
  gather(`1`:`30`, 
         # el nombre de las columnas las apile en una columna llamada "tree"
         key = "tree",
         # la observaciones de severidad las apile en una columna llamada sev
         value = "sev") -> oli_long # el producto de este re-arreglo se llamará "oli_long"

ftable(xtabs(~year+loc+farm, oli_long))

# canola <- readxl::read_excel("data/canola_maculas.xlsx")

canola %>%  
  gather(inc_15:inc_248, 
         key = "tt", 
         value = "inc") %>% 
  # ahora tengo deshacerme de la palabra "inc" de la columna tt para quedarme con los tiempos de evaluación 
  separate(tt, 
           c(NA, "tt"), 
           sep = '_', 
           convert = T) -> can_long

ftable(xtabs(~trt+bk, can_long))



# Compilar datos listos para la sig etapa
save(can_long, oli_long, soja_long, file = "data/datos_curso.RData")
load("data/datos_curso.RData")



# 5.2 dplyr::
# select
soja_long %>% select(trt, yield)
soja_long %>% select(-bk)
# filter
soja_long %>% 
  select(trt, yield) %>% 
  dplyr::filter(trt == 'check')

soja_long %>% 
  select(trt, yield) %>% 
  filter(trt %in% c("check","A"))

# mutate
soja_long %>% 
  mutate(yield_tn = yield/1000) %>% 
  select(-yield)

# Conversión de tipos de variables:
# Ahora que hemos re-organizado los datos, queremos chequear los tipos de variables que tiene el dataset:
datatype

str(soja_long)

soja_long %>% 
  mutate(yield_tn = yield/1000) %>% 
  select(-yield) %>% 
  mutate_at(vars(trt, bk), as.factor) %>% 
  mutate_at(vars(yield_tn), as.numeric) -> soja_long1

str(soja_long1) 

# summarise
soja_long %>% 
  group_by(trt) %>% 
  summarise(yield_mean =  mean(yield),
            yield_sd = sd(yield)) 

# Ordenar columnas y dejar el registro listo con orden de datos, columnas y datatypes
oli_long %>% 
  mutate_at(vars(tree), as.numeric) %>% 
  arrange(loc, year, farm, tree) %>% 
  mutate(tree_f = as.factor(tree)) %>% 
  mutate_at(vars(loc, farm), as.factor) %>% 
  select(loc, year, everything()) -> oli_long

# Guardo como csv 
write.csv2(oli_long, file = "data/oli_long.csv", row.names = F)
