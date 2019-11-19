# Hernan Angelini. 4-10-2019
# descripcion: carga tabla IPNI_SoilMoisture y genera graficos

# librerias
library(ggplot2)
library(cowplot)

# cargo datos
load("data/IPNI_SM.RData")
class(IPNI_SM)

ggplot(data = IPNI_SM, aes(CEa1m, Lectura_std_Med))
#
plot_grid(
  IPNI_SM %>% 
    ggplot(aes(CEa1m, fill = sitio))+
    geom_histogram()+
    guides(fill=FALSE) + 
    scale_fill_grey() # Escala de grises
  ,
  IPNI_SM %>% 
    ggplot(aes(CEa1m, fill = sitio)) +
    geom_density(alpha = 0.7)
  , 
  align="h", 
  labels=c("a","b")
)





# ejemplo
cowplot::plot_grid( # > install.packages("cowplot") no para 3.4.4
  iris %>%          
    ggplot(aes(Petal.Length, fill=Species)) +
    geom_histogram()+
    guides(fill=FALSE) + 
    scale_fill_grey() # Escala de grises
  ,
  iris %>% 
    ggplot(aes(Petal.Length, fill=Species)) +
    geom_density(alpha=0.7)
  , 
  align="h", 
  labels=c("a","b")
)

p <- IPNI_SM %>% 
  ggplot(aes(x = CEa1m, y = Lectura_std_Med)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~sitio) +
  theme_bw()
p

IPNI_SM %>% 
  ggplot(aes(x = CEa1m, y = Lectura_std_Med, colour = sitio)) + 
  geom_point()
