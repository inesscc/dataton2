library(tidyverse); library(ggplot2)
library(feather);library(sf)
library(nngeo)
# Carga datos -------------------------------------------------------------

fs::dir_tree(recurse = 3)

pob_manz = read_csv('data/input/chile/Poblacion_por_manzana_segun_Censo_2017_2C_Chile.csv') %>% 
  janitor::clean_names()

sf_use_s2(FALSE)
manzanas_malo = st_read('data/input/chile/shapes/Manzanas/pob_censo/pob_manzana_censo2017.shp') %>% 
  janitor::clean_names() %>% 
  mutate(nom_comuna = nom_comuna %>% str_to_title()) %>% 
  st_transform(crs = 3857)


manzanas = st_read('data/input/chile/shapes/Manzanas/microdatos_manzana/Microdatos_Manzana.shp') %>% 
  janitor::clean_names() %>% 
  mutate(comuna = comuna %>% str_to_title()) %>% 
  st_transform(crs = 3857)

manzanas_centroide = manzanas %>% st_centroid

entidades = st_read('data/input/chile/shapes/Manzanas/microdatos_entidad/Microdatos_Entidad.shp') %>% 
  janitor::clean_names() %>% 
  mutate(nom_comuna = nom_comuna %>% str_to_title()) %>% 
  st_transform(crs = 3857)


manzanas$manzent_i
st_crs(manzanas_centroide)
stgo = manzanas %>% filter(nom_comuna == 'SANTIAGO')
stgo_centroide = stgo %>% st_centroid


regiones = st_read('data/input/chile/shapes/Regiones/Regional.shp') %>% 
  janitor::clean_names()
comunas = st_read('data/input/chile/shapes/Comunas/comunas.shp') %>% 
  janitor::clean_names()

oriente = comunas %>% 
  filter(comuna %in% c('Las Condes', 'La Reina', 'Providencia', 'Ñuñoa', 'Vitacura'))
oriente_centroide = oriente %>% st_centroid()

comunas_centroide = comunas %>% st_centroid



manzanas_centroide$geometry

ee = read_csv2('data/input/educacion/Directorio-oficial-EE-2023/20230912_Directorio_Oficial_EE_2023_20230430_WEB.csv') %>% 
  janitor::clean_names() %>% filter( !is.na(longitud)) %>% 
  mutate(nom_com_rbd = nom_com_rbd %>% str_to_title()) %>% 
  transform(latitud = ifelse(latitud <= -50 & cod_reg_rbd == 13, longitud, latitud),
            longitud  = ifelse(latitud <= -50 & cod_reg_rbd == 13, latitud, longitud)) %>%
  transform(latitud = ifelse(latitud <= -60, longitud, latitud),
            longitud  = ifelse(latitud <= -60, latitud, longitud)) %>%
  filter(nom_rbd != 'ESCUELA HOSPITALARIA PROVINCIA CORDILLERA PUENTE ALTO') %>% 
  mutate(latitud = if_else(nom_rbd == 'JARDIN INFANTIL MAHAY MONTESSORI',
                           -33.34623599469457, latitud)) %>% 
  st_as_sf(coords = c('longitud', 'latitud'), crs = "EPSG:4326") %>% 
  st_transform(crs = 3857) %>% 
  rename(comuna = nom_com_rbd) %>% rownames_to_column('id_fila') %>% 
  mutate(id_fila = as.numeric(id_fila))





# Filtros -----------------------------------------------------------------



# ee_rm_vi_cent = ee_rm_vi %>%
#   left_join(rm_vi_cent %>%
#               select(codregion, geometry),
#             by = c('cod_reg_rbd' = 'codregion'))


# distancias --------------------------------------------------------------

distancias = st_distance(manzanas_centroide %>% filter(nom_comuna == 'Santiago'),
                         ee %>% filter(comuna == 'Santiago')) %>% as.data.frame()
manzanas_centroide %>% names
mas_cercanos = st_nearest_feature(manzanas_centroide,
                           ee) %>% as.data.frame()
mas_cercanos2 = st_nn(manzanas_centroide, ee, k = 1, returnDist = T)
nn_df =  do.call(cbind, mas_cercanos2) %>% as.data.frame %>%
  bind_cols(manzent = manzanas_centroide$manzent) %>%
  mutate(nn = as.numeric(nn),
         dist = as.numeric(dist)) %>% 
  left_join(ee %>% as.data.frame %>% select(rbd, id_fila), by=c('nn' = 'id_fila'))

nn_df %>% writexl::write_xlsx('data/output/distancias_por_manzana.xlsx')
manzanas %>% group_by(manzent_i) %>% filter(n()>1)

names(distancias) = ee %>% filter(comuna == 'Santiago') %>% pull(rbd) 
rownames(distancias) = manzanas_centroide %>% filter(nom_comuna == 'Santiago') %>%
  pull(manzent_i)
manzanas %>% group_by(manzent_i) %>% filter(n()> 1) %>% select(manzent_i, fid)

Adistancias %>% rownames_to_column('id_manzana') %>% writexl::write_xlsx('data/output/ejemplo_distancias_manzana.xlsx')

distancias %>% filter(row.names(distancias) %>% str_detect('GIROUET')) %>% t %>%  view

ee_rm_vi %>% group_by(nom_rbd) %>% filter(n()>1) %>% view
sf::st_nearest_points()


distancias 


plot_territorio = function(territorio, ee, 
                           filtro = NULL, col_filtro_ee = NULL,
                           col_filtro_territorio = NULL,
                           plot_centroide = T) {
  
  
  if(!is.null(filtro)){
    territorio = territorio %>%
      filter({{col_filtro_territorio}} %in% filtro)
    
    ee = ee %>% filter({{col_filtro_ee}} %in% filtro)
    
    
  }
  
  territorio_centroide = territorio %>% st_centroid()

  territorio %>% ggplot +
    geom_sf()  +
    #stat_sf_coordinates()+
    {if(plot_centroide) geom_sf(data = territorio_centroide,
                                color = 'red', size = 1, alpha = .3)}   +
    geom_sf(data = ee, color = 'blue', size = 2, alpha = .9)
  
  
  distancias = st_distance(territorio_centroide, ee) %>% as.data.frame()
  names(distancias) = ee$comuna
  rownames(distancias) = territorio$nom_rbd %>% make.unique(sep = '_')
  
  return(distancias)
  
}
# 
# ee$nom_com_rbd
# manzanas$nom_comuna
# d = plot_territorio(manzanas, ee, filtro = c('Santiago'),
#                 col_filtro_territorio = nom_comuna,
#                 col_filtro_ee = nom_com_rbd, plot_centroide = T )
# 
# 
# min_dist = distancias %>% apply(1, FUN = min)
# ee$cod_reg_rbd
# stgo$geometry
# Plot ----
inac = ee %>% filter(nom_rbd == 'LICEO INSTITUTO NACIONAL') 

ee %>% ggplot +
  geom_sf()  +
  #stat_sf_coordinates()+
  geom_sf(data = oriente_centroide, color = 'red', size = 2, alpha = .3)   +
  geom_sf(data = ee_rm_vi, color = 'blue', size = 3, alpha = .4)

  
  
ee_rm_vi %>% ggplot + geom_sf()
ee_rm_vi$geometry
rm_vi$geometry

  ee_rm_vi$geometry
pob_manz %>% count(REGION)
ee_rm %>% arrange(latitud) %>% select(latitud, longitud, nom_rbd) %>% view
