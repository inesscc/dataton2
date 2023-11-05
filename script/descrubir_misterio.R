library(readxl)
library(readr)
library(tidyverse)
library(janitor)
library(sf)

shapes_manzanas <-  sf::read_sf("data/shapes/Manzanas/microdatos_manzana/Microdatos_Manzana.shp")

shapes_manzanas_rm <- shapes_manzanas %>%
  mutate(caracteres = nchar(as.character(MANZENT))) %>% 
  mutate(REGION = if_else(caracteres == 14, str_sub(MANZENT, 1, 2), str_sub(MANZENT, 1, 1))) %>% 
  mutate(REGION = as.numeric(REGION)) %>% 
  select(REGION, geometry, MANZENT, nombre_comuna = COMUNA) %>% 
  filter(REGION == 13) %>% 
  select(geometry)


entidades = st_read('data/shapes/microdatos_entidad/Microdatos_Entidad.shp') %>%
  janitor::clean_names() %>% 
  #filter(region == "REGIÓN METROPOLITANA DE SANTIAGO") %>% 
  filter(cod_region == 13) %>% 
  select(geometry)

entidades$
final <- shapes_manzanas_rm %>% 
  bind_rows(entidades)


ggplot(final) +
   geom_sf()