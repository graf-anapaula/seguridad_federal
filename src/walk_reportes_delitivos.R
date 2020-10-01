library(tidyverse)

# Lectura
delitos <- read_csv(here::here("data/interim/delitos.csv"), locale = readr::locale(encoding = "latin1"))
delitos <- read_csv(here::here("data/interim/seguridad_clean.csv"), locale = readr::locale(encoding = "latin1"))
mod_mult_delito <- delitos %>%
  group_by(entidad) %>%
  nest() %>%
  mutate(modelo = map(data,
                      ~ lm(total ~ fecha, data = .x)),
         resumen = map(modelo,
                      ~ summary(modelo)))

mod_mult_delito %>%
  pull(modelo) %>%
  set_names(mod_mult_delito$entidad)

# delitos[7,]
# toString(delitos[8,1])
delitos_for <- unlist(delitos)
# seq(delitos)

purrr::map()
purrr::walk()

map(c(1, 2, 3, 4),
     ~ print(.x))

walk(delitos$tipo_de_delito,
     ~ rmarkdown::render('src/reports/reporte_delictivo.Rmd',
                           output_file = glue::glue("{.x}.html"),
                           params = list(delito = .x)))

# for(i in seq_along(delitos_for)){
#   rmarkdown::render('src/reports/reporte_delictivo.Rmd',
#                     output_file = str_c(toString(delitos_for[i]), ".html"),
#                     params = list(
#                       delito = toString(delitos_for[i])
#                     )
#   )
# }

library(purrr)

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")
