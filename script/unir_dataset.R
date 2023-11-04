library(readxl)
library(readr)
library(tidyverse)
library(janitor)
library(sf)

ismt = read_csv("data/censo/ISMT.csv")
censo17 = read_csv2("data/censo/censo2017_manzanas.csv") %>% 
  clean_names()

# ismt = ismt %>% 
#   filter(nom_cmn == "SANTIAGO")

glimpse(censo17)
glimpse(ismt)

# censo17 = censo17 %>% 
#   filter(comuna == 13101)

ismt %>% count(geocodigo)

censo17 %>% count(id_manzent)

# pegar información de ismt en tabla de censo
censo17_ismt = censo17 %>% 
  mutate(geocodigo = str_sub(id_manzent , 1,11),
         geocodigo = as.numeric(geocodigo)) %>% 
  left_join(ismt %>% 
              select(hogares,hog_40pct, prom_ismt, prom_rank, pct_hog40p, AVE_GSE, NSE, geocodigo),
            by = "geocodigo")

sum(is.na(censo17_ismt$NSE))

censo17_ismt %>% 
  filter(is.na(NSE)) %>% 
  View()

write_csv(censo17_ismt, file = "data/outputs/censo17_ismt.csv")


############
# UNIR TODO 
#############

# Información censal
censo <- read_csv("data/outputs/censo17_ismt.csv")

# Distancia a colegios
distancias <- read_excel("data/outputs/distancias_por_manzana.xlsx")
distancias_entidad <- read_excel("data/outputs/distancias_por_entidad.xlsx")

# Shapes: contiene solo 15 regiones
shapes_manzanas <-  sf::read_sf("data/shapes/Manzanas/pob_manzana_censo2017.shp")

# Para asegurar el orden de las regiones
shapes_manzanas <- shapes_manzanas %>% 
  mutate(REGION = as.numeric(REGION)) %>% 
  select(REGION, geometry, MANZENT_I)

# shapes de entidades (zonas rurales)
entidades = st_read('data/shapes/microdatos_entidad/Microdatos_Entidad.shp') %>% 
  janitor::clean_names() %>%
  select(matches('com|region'), total_pers) %>%
  mutate(comuna = nombre_com %>% str_to_title(),
         id_ent = paste0('e',row_number())) %>% 
  st_transform(crs = 3857) %>% 
  mutate(total_pers = as.numeric(total_pers),
         REGION = as.numeric(cod_region)
         ) %>% 
  left_join(distancias_entidad %>%
              select(-nn), by = "id_ent") %>% 
  select(-region, -comuna) %>% 
  filter(!nombre_com %in% c("ISLA DE PASCUA", "JUAN FERNÁNDEZ"))


# Unir todo
completa <- distancias %>% 
  inner_join(censo %>% mutate(id_manzent = as.character(as.numeric(id_manzent))),
             by =  c("manzent"= "id_manzent")) %>% 
  inner_join(shapes_manzanas, by = c("manzent" = "MANZENT_I") ) %>% 
  sf::st_as_sf() %>% 
  st_transform(crs = 3857) %>% 
  bind_rows(entidades) %>% 
  filter(REGION != 16) %>% 
  mutate(dist_ponderada = dist * total_pers)


completa$REGION %>% unique()
write_csv(completa, "data/outputs/tabla_final_app.csv")

###################################
# Guardar en el directorio de la app 
#################################
n_reg <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")

# Crear directorios
purrr::walk(n_reg,~dir.create(paste0("app/data/tabla_shiny/r", .x)))

# Separar por regiones
fe = split(completa,completa$REGION)

# # 2.1 separamos el dato por regiones y lo guardamos
purrr::walk2(fe,n_reg,~st_write(.x,paste0("app/data/tabla_shiny/r",.y,"/datos.shp")))
