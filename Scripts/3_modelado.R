# Hernan Angelini. 17-11-2019
# descripcion: carga tabla IPNI_SoilMoisture y genera un modelo linal para predecir 
# el contenido de humedad volumetrio de suelo a partir de la lectura de CEa para cada 
# sitio de monitoreo

# librerias
library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)


# cargo datos
load("data/IPNI_SM.RData")

# Correlacion entre HvMed ~ CEa1m
IPNI_SM %>%
  group_by(sitio) %>% 
  get_correlation(formula = HvMed ~ CEa1m) -> SitioCor

# Grafico de dispersion entre variables
ggplot(IPNI_SM, aes(x = CEa1m, y = HvMed)) +
  geom_point() +
  labs(x = "CEa 1m", y = "Hv media 1m", title = "Scatterplot of HvMed ~ CEa1m") +
  geom_smooth(method = "lm", se = T)

##################
library(tidyverse)
library(skimr)
library(broom)
library(rcfss)

modverde <- lm(HvMed ~ CEa1m, subset = (sitio %in% c("S4", "S5", "S5")) , data = IPNI_SM)
modrojo <- lm(HvMed ~ CEa1m, subset = !(sitio %in% c("S4", "S5", "S6", "S12")) , data = IPNI_SM)

plot(mod)
tidy(modrojo)
glance(modverde)

# coefficient plot
d <- tidy(mod) %>% 
  mutate(
    low = estimate - std.error,
    high = estimate + std.error
  )

ggplot(d, aes(estimate, term, xmin = low, xmax = high, height = 0)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh()

augment(mod)
augment(mod, IPNI_SM)

##################


# Fit regression model:
Hv_model <- lm(HvMed ~ CEa1m, data = IPNI_SM)
# Get regression table:
get_regression_table(Hv_model)

# Corre el modelo para el set de datos 
regression_points <- get_regression_points(Hv_model)
regression_points

# grafica el histograma de los residuales
ggplot(regression_points, aes(x = residual)) +
  geom_histogram() +
  labs(x = "Residual")
