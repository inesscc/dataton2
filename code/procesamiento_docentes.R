library(tidyverse)
library(janitor)

############################
# Armar tabla de profesores
############################

# Se arma una tabla para hacerle seguimiento a los profesores que están en 2007

extrar_columnas_relevantes <- function(file) {
  docentes <- read_csv2(file) %>% 
    clean_names()
  
  columnas_relevantes <- docentes %>% 
    select(mrun, agno, rbd, cod_reg_rbd, nom_rbd, dgv_rbd, cod_depe, tip_insti_id_1,
           duracion_carrera_1, modalidad_estudio_1, tip_insti_id_2)
  
  return(columnas_relevantes)
  }

docentes <- read_csv2("data/docentes/docentes_csv//Docentes_2015.csv")



full_files <- list.files("data/docentes/docentes_csv/", full.names = T) 

anios <-  full_files %>% 
  str_extract(pattern = "[[:digit:]]{4}") %>% 
  as.numeric()

filtro_anios <- anios >= 2011

files <- full_files[filtro_anios]

data <-  map_df(files, extrar_columnas_relevantes)

total_anios <- length(files)

profe_anio <-  data %>% 
  group_by(mrun, agno) %>%
  slice(1) %>%
  ungroup()

n_anios <-  profe_anio %>% 
  group_by(mrun) %>% 
  summarise(contar = n())

filtro <- n_anios %>% 
  filter(contar == total_anios) %>% 
  pull(mrun)

total_docentes <-  length(unique(filtro))

full_years <-  profe_anio %>% 
  filter(mrun %in% filtro)

profes_por_anio_dependecia <-  full_years %>%
  mutate(cod_depe2 = case_when(
    cod_depe %in% c(1, 2) ~ "municipal",
    cod_depe == 3 ~ "subvencionado", 
    cod_depe == 4 ~ "particular pagado",
    cod_depe == 5 ~ "administración delegada",
    cod_depe == 6 ~ "servicio local"
  )) %>% 
  mutate(cod_depe3 = case_when(
    cod_depe %in% c(1, 2, 6) ~ "público",
    cod_depe == 3 ~ "subvencionado", 
    cod_depe == 4 ~ "particular pagado",
    cod_depe == 5 ~ "administración delegada"  )) %>% 
  group_by(agno, cod_depe3) %>% 
  summarise(contar = n()) %>% 
  ungroup() %>% 
  mutate(total_profesores = total_docentes,
         porcentaje = contar / total_docentes * 100
         )


profes_por_anio_dependencia <-  full_years %>%
  mutate(cod_depe3 = case_when(
    cod_depe %in% c(1, 2, 6) ~ "público",
    cod_depe == 3 ~ "subvencionado", 
    cod_depe == 4 ~ "particular pagado",
    cod_depe == 5 ~ "administración delegada"  )) %>%
  mutate(tip_insti_id_1 = if_else(is.na(tip_insti_id_1), 99, tip_insti_id_1)) %>% 
  group_by(agno, cod_depe3, tip_insti_id_1) %>% 
  summarise(frecuencia = n())

profes_por_anio_dependencia <- profes_por_anio_dependencia %>% 
  left_join(full_years %>% 
              mutate(tip_insti_id_1 = if_else(is.na(tip_insti_id_1), 99, tip_insti_id_1)) %>% 
              group_by(agno, tip_insti_id_1) %>% 
              summarise(total_docentes = n()),
            by = c("agno", "tip_insti_id_1")
  ) %>% 
  mutate(porcentaje = frecuencia / total_docentes * 100 ) %>% 
  group_by(agno, tip_insti_id_1) %>% 
  mutate(comprobar = sum(porcentaje))



# profes_por_anio_dependencia %>% 
#   ggplot(aes(x = agno, y = porcentaje, group = cod_depe2, color = as.factor(cod_depe2))) +
#   geom_line() +
#   facet_wrap(~tip_insti_id_1)

profes_por_anio_dependencia %>% 
  ggplot(aes(x = agno, y = porcentaje, group = cod_depe3, color = as.factor(cod_depe3))) +
  geom_line() +
  facet_wrap(~tip_insti_id_1)


############################3
############################
############################
############################

profes_por_anio_dependencia <-  full_years %>%
  mutate(cod_depe3 = case_when(
    cod_depe %in% c(1, 2, 6) ~ "público",
    cod_depe == 3 ~ "subvencionado", 
    cod_depe == 4 ~ "particular pagado",
    cod_depe == 5 ~ "administración delegada"  )) %>%
  mutate(modalidad_estudio_1 = if_else(is.na(modalidad_estudio_1), 99, modalidad_estudio_1)) %>% 
  group_by(agno, cod_depe3, modalidad_estudio_1) %>% 
  summarise(frecuencia = n())

profes_por_anio_dependencia <- profes_por_anio_dependencia %>% 
  left_join(full_years %>% 
              mutate(modalidad_estudio_1 = if_else(is.na(modalidad_estudio_1), 99, modalidad_estudio_1)) %>% 
              group_by(agno, modalidad_estudio_1) %>% 
              summarise(total_docentes = n()),
            by = c("agno", "modalidad_estudio_1")
  ) %>% 
  mutate(porcentaje = frecuencia / total_docentes * 100 ) %>% 
  group_by(agno, modalidad_estudio_1) %>% 
  mutate(comprobar = sum(porcentaje)) %>% 
  ungroup()



profes_por_anio_dependencia %>% 
  ggplot(aes(x = agno, y = porcentaje, group = cod_depe3, color = as.factor(cod_depe3))) +
  geom_line() +
  facet_wrap(~modalidad_estudio_1)

############################3
############################
############################
############################


x <-  extrar_columnas_relevantes("data/colegios/")

docentes <- read_csv2("data/docentes/docentes_csv/Docentes_2004.csv")

docentes %>% clean_names() %>%  names()
docentes %>% 
  count(COD_DEPE)

list.files("data/docentes/")

docentes <- read_csv2("data/colegios/colegios_csv/Directorio_Oficial_EE_2023.csv")
docentes <- read_csv2("data/docentes/docentes_csv/Docentes_2023.csv")

docentes %>% 
  group_by()

names(docentes)
profesores <-  data %>% 
  filter(agno == 2023) %>% 
  group_by(mrun) %>% 
  slice(1) %>% 
  ungroup()

profes_dep <- profesores %>%   
  group_by(cod_depe) %>% 
  summarise(total_profes = n())

profes_dep_educ_docente <- profesores %>% 
  group_by(cod_depe, modalidad_estudio_1) %>% 
  summarise(frecuencia = n()) %>% 
  left_join(profes_dep, by = "cod_depe") %>% 
  mutate(porcentaje = frecuencia / total_profes * 100)


profes_dep_educ_docente %>% 
  ggplot(aes(x = cod_depe, y = porcentaje, fill = as.factor(modalidad_estudio_1) )) +
  geom_bar(stat = "identity", position = "dodge")

########################
########################

profes_dep <- profesores %>%   
  group_by(tip_insti_id_1) %>% 
  summarise(total_profes = n())

profes_dep_educ_docente <- profesores %>% 
  group_by(cod_depe, tip_insti_id_1) %>% 
  summarise(frecuencia = n()) %>% 
  left_join(profes_dep, by = "cod_depe") %>% 
  mutate(porcentaje = frecuencia / total_profes * 100)


profes_dep_educ_docente %>% 
  ggplot(aes(x = cod_depe, y = porcentaje, fill = as.factor(modalidad_estudio_1) )) +
  geom_bar(stat = "identity", position = "dodge")

rm <- profesores %>% 
  filter(cod_reg_rbd == 13)
  
cor(rm$)