###Cargar libreria
library(dplyr)
library(lubridate)
library(ggplot2)

###importar libreria
datos <- read.csv("C:/Users/Aleja/Downloads/covid-vaccination-vs-death_ratio.csv/covid-vaccination-vs-death_ratio.csv",
                  header = TRUE, sep = ",")

###filtrar por paises
datos = datos %>%
  filter(country %in% c('Ecuador','Georgia'))

###convertir la columna fecha al tipo correspondiente
datos <- datos %>%
  mutate(date = as.Date(date))

###se deja el primer día del mes
datos_agru = datos %>%
  mutate(mes = floor_date(date, "month"))

###se agrupa por mes
datos_agru <- datos_agru %>%
  group_by(mes,country) %>%
  summarize(
    muertes = sum(New_deaths),
    vacunados = max(people_vaccinated),
    poblacion = mean(population)
  )

###crear nuevas columnas
datos_agru <- datos_agru %>%
  mutate(
    muertes_poblacion = muertes/poblacion,
    vacunados_poblacion = vacunados/poblacion)

###colocar como factor el pais
datos_agru$country <- factor(datos_agru$country)

###graficar serie de tiempo con la variable muertes_poblacion
g = ggplot(datos_agru, aes(x = mes, y = muertes_poblacion, color = country)) +
  geom_line() +
  labs(
    title = "Serie de tiempo por pais de muertes sobre población",
    x = "Fecha",
    y = "muertes sobre poblacion",
    color = "Pais"
  ) +
  theme_minimal()

print(g)

###graficar serie de tiempo con la variable vacunados_poblacion
h = ggplot(datos_agru, aes(x = mes, y = vacunados_poblacion, color = country)) +
  geom_line() +
  labs(
    title = "Serie de tiempo por pais de vacunados sobre poblacion",
    x = "Fecha",
    y = "vacunados sobre poblacion",
    color = "Pais"
  ) +
  theme_minimal()

print(h)

###crear columna que me dice si es de control o tratamiento
datos_agru$grupo = ifelse(datos_agru$country == 'Ecuador', "Tratamiento", "Control")

###decir si se hizo antes del tratamiendo o después
datos_agru$tiempo = ifelse(datos_agru$mes <= '2021-07-01', "antes", "despues")

###convertir las variables en factores
datos_agru$grupo <- factor(datos_agru$grupo)
datos_agru$tiempo <- factor(datos_agru$tiempo)

###revisar si se cumple el supuesto de modelo paralelo
df_pre_tratamiento <- datos_agru %>%
  filter(tiempo == "antes")

###revisar el modelo antes
print(wilcox.test(muertes_poblacion ~ grupo, data = df_pre_tratamiento))


# Modelo de Diferencias en Diferencias
modelo <- lm(muertes_poblacion ~ grupo * tiempo, data = datos_agru)

# Ver los resultados del modelo
print(summary(modelo))



