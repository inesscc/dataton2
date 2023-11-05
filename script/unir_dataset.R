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

# información sobre distancia a colegios buenos
distancias_colegios_buenos <- read_excel("data/outputs/distancias_por_manzana_alto_rendimiento_quintiles.xlsx")
distancias_entidad_colegios_buenos <- read_excel("data/outputs/distancias_por_entidad_alto_rendimiento_quintil.xlsx")

# Distancia a colegios
distancias <- read_excel("data/outputs/distancias_por_manzana.xlsx")
distancias_entidad <- read_excel("data/outputs/distancias_por_entidad.xlsx")

# Shapes: contiene solo 15 regiones
#shapes_manzanas <-  sf::read_sf("data/shapes/Manzanas/pob_manzana_censo2017.shp")
shapes_manzanas <-  sf::read_sf("data/shapes/Manzanas/microdatos_manzana/Microdatos_Manzana.shp")

# Para asegurar el orden de las regiones
shapes_manzanas <- shapes_manzanas %>%
  mutate(caracteres = nchar(as.character(MANZENT))) %>% 
  mutate(REGION = if_else(caracteres == 14, str_sub(MANZENT, 1, 2), str_sub(MANZENT, 1, 1))) %>% 
  mutate(REGION = as.numeric(REGION)) %>% 
  select(REGION, geometry, MANZENT, nombre_comuna = COMUNA) 

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
  left_join(distancias_entidad_colegios_buenos %>%
              select(-nn, dist_bueno = dist, id_ent, rbd), by = "id_ent" ) %>% 
  select(-region, -comuna) %>% 
  filter(!nombre_com %in% c("ISLA DE PASCUA", "JUAN FERNÁNDEZ"))




# Tratar distancias a colegios buenos
distancias_colegios_buenos <- distancias_colegios_buenos %>% 
  select(manzent, dist_bueno = dist)


# Unir todo
completa <- distancias %>% 
  inner_join(distancias_colegios_buenos, by = "manzent") %>% 
  inner_join(censo %>% mutate(id_manzent = as.character(as.numeric(id_manzent))),
              by =  c("manzent"= "id_manzent")) %>% 
  inner_join(shapes_manzanas, by = c("manzent" = "MANZENT") ) %>% 
  sf::st_as_sf() %>% 
  st_transform(crs = 3857) %>% 
  bind_rows(entidades) %>% 
  filter(REGION != 16, !comuna %in% c(5201, 5104)) %>%
  mutate(total_personas = total_pers) %>% 
  mutate(total_personas = if_else(is.na(total_pers) & !is.na(personas), personas, total_personas)) %>% 
  mutate(dist_ponderada = dist * total_personas,
         dlog = log10(dist)
         ) %>% 
  mutate(dist_ponderada2 = scale(dist_ponderada),
         dist2 = scale(dist),
         dist_ponderada_norm = (dist_ponderada-min(dist_ponderada))/(max(dist_ponderada)-min(dist_ponderada)),
         dist_norm = (dist-min(dist))/(max(dist)-min(dist)),
         dist_bueno_10 = log10(dist_bueno),
         dpd = dist_bueno * total_personas
         ) %>% 
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(id_ent = if_else(is.na(id_ent), "999", id_ent)) %>% 
  filter( id_ent != "e16608")   # sacar una isla rara
  
censo %>% 
  filter( comuna == 8201 & area == 1) %>% 
  anti_join(completa %>% filter(nombre_comuna == "LEBU"),
            by = c("id_manzent" = "manzent")  ) %>% 
  nrow()

censo %>%
  filter( comuna == 8201 & area == 1) %>% 
  nrow()


for (com_shape in unique(shapes_manzanas$nombre_comuna)) {
        if (!com_shape %in% completa$nombre_comuna ) {
          print(com_shape)
        }
  }




# xy <- sf::st_coordinates(completa$centroid)
# 
# z <- completa %>% 
#   mutate(x = xy[, 1]) %>% 
#   mutate(x_abs = as.integer(abs(x))) 
# 
# 
# z %>% 
#   filter( x_abs == 8916497) %>% 
#   select(id_ent, manzent)
# 
# 
# completa$x %>% summary()
# z$x_abs %>% summary()
# completa$x %>% class()
# 
# x$centroid[1:10]
# x$REGION %>% unique()

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


full_data <-  st_write(completa,paste0("app/data/tabla_shiny/full",.y,"/datos.shp"))
