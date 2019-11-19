# Hernan Angelini. 4-10-2019
# descripcion: carga datos de CEa de EM38 y deja listo para proceso

# librerias
library(readxl)
library(tidyverse)

# lee excel EM38-CEa ####
em38 <- read_excel("data/CEaM38_IPNI_2017_18.xls", 
                                  col_types = c("date", "text", "numeric", "numeric", 
                                                "numeric", "numeric","numeric"))
summary(em38)

lec.2019.11 <- read.csv("data/IPNI_Lec_2019_11.csv")

# Manipulacion: wide - long
em38 %>% mutate_at(vars(fecha:sitio), list(as.factor)) -> em38

ftable(xtabs(~fecha+sitio, em38))

save(em38, file = "data/em38.RData")


# lee excel SNeutrones ####
sonda <- read_excel("data/Sonda_2017a2018.xls", 
                                       sheet = "SONDAN", col_types = c("date", 
                                                                       "numeric", "text", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric"))

summary(sonda)

# Manipulacion: wide - long
sonda %>% gather(key = "sitio", value = "Lectura_std", S1:S12) %>% 
          mutate_at(vars(fecha:Prof), list(as.factor)) -> sonda
    
ftable(xtabs(~fecha+Prof+Sitio, sonda))

# elimino filas NA
sonda %>% dplyr::filter(!is.na(sonda$Lectura_std)) -> sonda


save(sonda, file = "data/sonda.RData")

# Puedo pasar a humedad = 0.3737 * (lec/std) + 0.0248
# Puedo pasar a lamina de agua en mm = humedad * prof en m * 1000
# promedio de profunfidades a 100 cm y a 40 cm
sonda %>% group_by(fecha, sitio) %>% 
          summarise(Lectura_std_Med = mean(Lectura_std),
                    Lectura_std_Sum = sum(Lectura_std),
                    Lectura_std_Max = max(Lectura_std)) -> sonda_gby

sonda_gby %>% mutate_at(vars(sitio), list(as.factor)) -> sonda_gby


save(sonda_gby, file = "data/sonda_gby.RData")

# Unir em38 y sonda por fecha + sitio ####
em38 %>% inner_join(sonda_gby) -> IPNI_SM
IPNI_SM %>% mutate_at(vars(sitio), list(as.factor)) -> IPNI_SM

# Hay un error de muestreo con el registro 37 (S1 2018-01-10). 
# Se quita con la siguiente linea
IPNI_SM <- IPNI_SM[-37,]

# Se agrega el valor de Humedad volumetrico por sugerencia de RR
IPNI_SM$HvMed <- (IPNI_SM$Lectura_std_Med * 0.3737) + 0.0248
summary(IPNI_SM$HvMed)

# Se agrega zona Roja o Verde de EM38-CEa1m
IPNI_SM$Zona <- "red"
IPNI_SM$Zona[(IPNI_SM$sitio %in% c("S4", "S5", "S6"))] <- "green"
IPNI_SM$Zona[(IPNI_SM$sitio %in% c("S12"))] <- "yellow"

# Guarda objeto
save(IPNI_SM, file = "data/IPNI_SM.RData")


# Export to .csv ####
write.csv2(em38, file = "data/em38.csv", row.names = F)
write.csv2(sonda, file = "data/sonda.csv", row.names = F)
write.csv2(sonda_gby, file = "data/sonda_gby.csv", row.names = F)
IPNI_SM %>% write.csv(.,file = "data/IPNI_SM.csv", row.names = F)
  
