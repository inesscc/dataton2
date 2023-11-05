library(tidyverse); library(ggplot2)
library(arrow);library(sf)
library(nngeo)



# funciones ---------------------------------------------------------------
filtrar_ee_por_rendimiento = function(indicadores, ee, filtro){
  rbds_filtrados = indicadores %>%
    filter(categoria_desempeno_2019_media %in% filtro |
             categoria_desempeno_2019_basica %in% filtro)
  
  
  ee_filtered = ee %>% inner_join(rbds_filtrados, by='rbd')
  
  
}

filtrar_ee_por_rendimiento2 = function(indicadores, ee){
  rbds_filtrados = indicadores %>%
  filter(quintil_mate4 == 1 |
             quintil_lect4 == 1)
  
  ee_filtered = ee %>% inner_join(rbds_filtrados, by='rbd')
  
  
}




obtener_mas_cercanos = function(territorio, ee, tipo_territorio = 'manzana'){
  mas_cercanos = st_nn(territorio, ee, k = 1, returnDist = T)
  
  nn_df =  do.call(cbind, mas_cercanos) %>% as.data.frame %>%
    {if(tipo_territorio == 'manzana') bind_cols(., manzent = manzanas_centroide$manzent) else
      bind_cols(., id_ent = entidades_centroide$id_ent)} %>%
    mutate(nn = as.numeric(nn),
           dist = as.numeric(dist)) %>% 
    left_join(ee %>% as.data.frame %>% select(rbd, id_fila), by=c('nn' = 'id_fila'))
}

plot_territorio = function(territorio, ee, 
                           filtro = NULL, col_filtro = NULL,
                           plot_centroide = T,
                           plot_estab = T,
                           size_ee = 2,
                           col_fill = NULL) {
  
  
  if(!is.null(filtro)){
    territorio = territorio %>%
      filter({{col_filtro}} %in% filtro)
    
    ee = ee %>% filter({{col_filtro}} %in% filtro)
    
    
  }
  
  territorio_centroide = territorio %>% st_centroid()
  
  p = territorio %>% ggplot +
    geom_sf(aes(fill = {{col_fill}}), lwd = 0)  +
    scale_fill_continuous(high = "red", low = "green") +
    #stat_sf_coordinates()+
    {if(plot_centroide) geom_sf(data = territorio_centroide,
                                color = 'red', size = 1, alpha = .3)}   +
    {if(plot_estab) geom_sf(data = ee, color = 'blue', size = size_ee, alpha = .8)}
  
  ggsave('data/output/viz/prueba.png', width = 20, height = 20)
  
  print(p)
  
}


# Carga datos -------------------------------------------------------------



## Territoriales -----------------------------------------------------------


fs::dir_tree(recurse = 3)

pob_manz = read_csv('data/input/chile/Poblacion_por_manzana_segun_Censo_2017_2C_Chile.csv') %>% 
  janitor::clean_names()

sf_use_s2(FALSE)
# manzanas_malo = st_read('data/input/chile/shapes/Manzanas/pob_censo/pob_manzana_censo2017.shp') %>% 
#   janitor::clean_names() %>% 
#   mutate(nom_comuna = nom_comuna %>% str_to_title()) %>% 
#   st_transform(crs = 3857)
com_gran_stgo = 'Cerrillos, Cerro Navia, Conchalí, El Bosque, Estación Central,
 Huechuraba, Independencia, La Cisterna, La Florida, La Granja, La Pintana, La Reina,
 Las Condes, Lo Barnechea, Lo Espejo, Lo Prado, Macul, Maipú, Ñuñoa, Pedro Aguirre Cerda,
 Peñalolén, Providencia, Pudahuel, Quilicura, Quinta Normal, Recoleta, Renca, San Joaquín,
 San Miguel, San Ramón, Santiago, Vitacura' %>% str_split(',') %>% map(~str_remove(.x, '\n') %>% str_squish()) %>% unlist

entidades = st_read('data/input/chile/shapes/Manzanas/microdatos_entidad/Microdatos_Entidad.shp') %>% 
  janitor::clean_names() %>%
  select(matches('com|region'), total_pers) %>%
  mutate(comuna = nombre_com %>% str_to_title(),
         id_ent = paste0('e',row_number())) %>% 
  st_transform(crs = 3857) %>% 
  mutate(total_pers = as.numeric(total_pers),
         total_pers =  if_else(total_pers == 0, NA_integer_, total_pers)) %>% 
  filter(!comuna %in% c('Isla De Pascua', 'Juan Fernández'),
         id_ent != 'e16608')
entidades_centroide = entidades %>% st_centroid
codreg = entidades %>% distinct(region, cod_region)





manzanas = st_read('data/input/chile/shapes/Manzanas/microdatos_manzana/Microdatos_Manzana.shp') %>% 
  janitor::clean_names() %>% 
  mutate(comuna = comuna %>% str_to_title()) %>% 
  st_transform(crs = 3857) %>%
  select(manzent, total_pers, matches('com|reg')) %>%
  left_join(codreg) %>% 
  filter(!comuna %in% c('Isla De Pascua', 'Juan Fernández'))

manzanas_centroide = manzanas %>% st_centroid

regiones = st_read('data/input/chile/shapes/Regiones/Regional.shp') %>% 
  janitor::clean_names()
comunas = st_read('data/input/chile/shapes/Comunas/comunas.shp') %>% 
  janitor::clean_names()
comunas_centroide = comunas %>% st_centroid


## Educación ---------------------------------------------------------------

indicadores = arrow::read_feather('data/input/educacion/indicadores_colegios.feather') %>% 
  filter(!is.na(categoria_desempeno_2019_basica) | !is.na(categoria_desempeno_2019_media))


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
  rename(comuna = nom_com_rbd,
         cod_region = cod_reg_rbd) %>% rownames_to_column('id_fila') %>% 
  mutate(id_fila = as.numeric(id_fila)) %>% 
  filter(!comuna %in% c('Isla De Pascua', 'Isla de Pascua', 'Juan Fernández'))


ee_ind = ee %>% inner_join(indicadores, by='rbd')



ee_alto_rendimiento = filtrar_ee_por_rendimiento(indicadores, ee, 'ALTO')
ee_alto_rendimiento2 = filtrar_ee_por_rendimiento2(indicadores, ee)

ee_bajo_rendimiento = filtrar_ee_por_rendimiento(indicadores, ee, 'INSUFICIENTE')

# distancias --------------------------------------------------------------

mas_cercanos_manz = obtener_mas_cercanos(manzanas_centroide, ee)
mas_cercanos_manz_alto_rend2 = obtener_mas_cercanos(manzanas_centroide, ee_alto_rendimiento2)
mas_cercanos_manz_alto_rend = obtener_mas_cercanos(manzanas_centroide, ee_alto_rendimiento)
mas_cercanos_manz_bajo_rend = obtener_mas_cercanos(manzanas_centroide, ee_bajo_rendimiento)


mas_cercanos_ent = obtener_mas_cercanos(entidades_centroide, ee, tipo_territorio = 'entidad')

mas_cercanos_manz %>% writexl::write_xlsx('data/output/distancias_por_manzana.xlsx')
mas_cercanos_manz_alto_rend %>% writexl::write_xlsx('data/output/distancias_por_manzana_alto_rendimiento.xlsx')
mas_cercanos_manz_alto_rend2 %>% writexl::write_xlsx('data/output/distancias_por_manzana_alto_rendimiento_quintiles.xlsx')

mas_cercanos_ent %>% writexl::write_xlsx('data/output/distancias_por_entidad.xlsx')


full = manzanas %>% bind_rows(entidades)

# 
# ee$nom_com_rbd
# manzanas$nom_comuna
# Plot ----
manzanas_norm = manzanas %>% left_join(mas_cercanos_manz)
manzanas_alto = manzanas %>% left_join(mas_cercanos_manz_alto_rend)
manzanas_bajo = manzanas %>% left_join(mas_cercanos_manz_bajo_rend)

entidades %>% names
plot_territorio(manzanas_alto, ee_alto_rendimiento2, 
                filtro = com_gran_stgo,
                col_filtro = comuna,
                plot_centroide = F,
                size_ee = .05,
                plot_estab = T,
                col_fill = dist)


entidades_centroide  %>%  filter(cod_region == 5) %>% distinct(comuna, .keep_all = T) %>% view
xy = entidades %>% filter(cod_region == 5) %>% st_coordinates()
(entidades_centroide %>% filter(cod_region == 5))[order(xy[,"X"]),] %>% view


# Extras ------------------------------------------------------------------


# 
# 
# min_dist = distancias %>% apply(1, FUN = min)
# ee$cod_reg_rbd
# stgo$geometry
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
