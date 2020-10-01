#  Librerías ####
library(tidyverse)
#install.packages("lubridate")
library(lubridate)
# install.packages("here")
# install.packages(c("knitr", "rmarkdown"))
library(here)
library(knitr)
library(rmarkdown)
# Lectura de datos ----
# seguridad_ <- read_csv("data/raw/Municipal-Delitos-2015-2020_jul2020/Municipal-Delitos-2015-2020_jul2020.csv",
                       # locale = locale(encoding = "latin1"))

seguridad <- data.table::fread("data/raw/Municipal-Delitos-2015-2020_jul2020/Municipal-Delitos-2015-2020_jul2020.csv",
                               encoding = 'Latin-1') %>%
  tibble()

# seguridad %>%
#   count(tipo_de_delito) %>%
#   write_csv()

# Limpieza ----
seguridad_clean <- seguridad %>%
  janitor::clean_names() %>%
  pivot_longer(enero:diciembre, names_to = "mes", values_to = "total") %>%
  unite(fecha, ano, mes, sep = "/") %>%
  mutate(fecha = parse_date(fecha, '%Y/%B', locale = locale("es")))


seguridad_valores <- seguridad_clean %>%
  filter(total != 0)

write_csv(seguridad_clean, 'data/interim/seguridad_clean.csv')
data.table::fwrite(seguridad_valores, 'data/interim/seguridad_valores.csv')

# Primera exploración de datos ####
# c_across()
# Conteo ----

# Número de casos reportados por estado

# vector de tipo de delito

delitos <- seguridad_valores %>%
  select(tipo_de_delito) %>%
  unique()

data.table::fwrite(delitos, 'data/interim/delitos.csv')

# write_csv(delitos, 'data/interim/delitos.csv')

seguridad_valores %>%
  count(year(fecha), entidad)

seguridad_valores %>%
  count(entidad) %>%
  print(n = 32)

# Número de casos por tipo durante el tiempo
seguridad_valores %>%
  count(year(fecha), tipo_de_delito) %>%
  print(n = 50)

# Homicidios
homicidios_nacional <- seguridad_valores %>%
  count(fecha, tipo_de_delito) %>%
  filter(tipo_de_delito == "Homicidio")

homicidios_estado <- seguridad_valores %>%
  # rename(anio = 'year(fecha)')
  count(year(fecha), entidad,tipo_de_delito) %>%
  filter(tipo_de_delito == "Homicidio") %>%
  rename(anio = 'year(fecha)')

# Visualizaciones ----
homicidios_nacional %>%
  # filter(month(fecha) %in% 1:6) %>%
  ggplot() +
    aes(fecha, n) +
    geom_line() +
    geom_smooth(aes(color = "loess"), method = "loess", se = FALSE) +
    geom_smooth(aes(color = "lm"), method = "lm", se = FALSE) +
    xlab("Tiempo") + ylab(NULL) +
    ggtitle("Número de homicidios en México", subtitle = "De enero 2015 a julio 2020") +
    theme_minimal()

homicidios_nacional %>%
  mutate(mes = month(fecha, label = T)) %>%
  ggplot() +
  facet_wrap(vars(mes)) +
  geom_vline(xintercept = ymd('2018-12-01')) +
  geom_line(aes(x = fecha, y = n))

  homicidios_nacional %>%
    filter(fecha >= "2017-07-01") %>%
    ggplot(aes(fecha, n)) +
    geom_line() +
    geom_smooth(aes(color = "loess"), method = "loess", se = FALSE) +
#    geom_smooth(aes(color = "lm"), method = "lm", se = FALSE) +
    xlab("Tiempo") + ylab(NULL) +
    ggtitle("Número de homicidios en México", subtitle = "De julio 2017 a julio 2020")

seguridad_valores %>%
  filter(month(fecha) %in% 1:6) %>%
  count(anio = year(fecha), entidad) %>%
  ggplot(aes(anio, n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  xlab("Tiempo (años)") + ylab(NULL) +
  ggtitle("Reporte de delitos por Estado", subtitle = "Estados con más de 10,000 casos reportados al año")

