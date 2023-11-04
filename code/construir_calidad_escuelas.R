


library(tidyverse)
library(janitor)
library(readxl)

##############
# Cargar datos
##############

# Leemos el directorio de colegios del 2022
directorio_colegios <- read_csv2("data/colegios/colegios_csv/Directorio_Oficial_EE_2022.csv")

# Simce para el año 2022 de segundo y cuarto básico
simce2 <- read_delim("data/simce/Simce 2° medio 2022/Archivos CSV (Planos)/simce2m2022_rbd_final.csv", delim = "|")
simce4 <- read_delim("data/simce/Simce 4° básico 2022/Archivos CSV (Planos)/simce4b2022_rbd_final.csv", delim = "|")

# Datos sobre dotación docente
dotacion_colegios <- read_csv2("data/colegios/Resumen-docentes-EE-2022/20210906_Dotación_docente_2022_20220630_PUBL.csv")

# Evaluación docente
evaluacion <- read_csv2("data/docentes/Evaluacion-Docente-2021/20230203_EVALUACION_DOCENTE_2021_20221109_PUBL.csv")

# Cargar datos que contienen categoría de desempeño de los colegios
desempenio_basica <- read_excel("data/simce/CDB2019/CDB2019.xlsx")

# Cargar datos que contienen categoría de desempeño de los colegios
desempenio_media <- read_excel("data/simce/CDM2019/CDM2019.xlsx")



######################
# Directorio colegios
######################

# Dejamos solo los colegios activos
colegios_activos <- directorio_colegios %>% 
  clean_names() %>% 
  filter(estado_estab == 1 & matricula == 1) %>% 
  mutate(id_colegio = paste(rbd, dgv_rbd, sep = "-")) %>% 
  select(id_colegio, rbd, nom_rbd, nom_com_rbd, cod_depe, cod_depe2, mat_total,
         latitud, longitud,
         cod_reg_rbd
         )


##############
# Tablas simce
##############

simce2_edit <- simce2 %>% 
  clean_names()  %>% 
  mutate(id_colegio = paste(rbd, dvrbd, sep =  "-")) %>% 
  select(id_colegio, adecuado_mate2 =  palu_eda_ade_mate2m_rbd, adecuado_lenguaje2 = palu_eda_ade_lect2m_rbd)

simce4_edit <- simce4 %>% 
  clean_names()  %>% 
  mutate(id_colegio = paste(rbd, dvrbd, sep =  "-")) %>% 
  select(id_colegio, adecuado_mate4 =  palu_eda_ade_mate4b_rbd, adecuado_lenguaje4 = palu_eda_ade_lect4b_rbd)

########################
# Tabla dotación colegio
########################

# Ordenar la inforación sobre horas contratadas 
dotacion_colegios_edit <- dotacion_colegios %>% 
  clean_names()  %>% 
  mutate(id_colegio = paste(rbd, dgv_rbd, sep = "-")) %>% 
  select(id_colegio, hh_tot, hh_a, dc_a)

  
######################
# Evaluación docente
#####################
evaluacion_edit <- evaluacion %>% 
  clean_names() %>% 
  mutate(id_colegio = paste(rbd, dgv_rbd, sep = "-")) %>% 
  select(id_colegio, portafolio = pf_pje, escala_portafolio = pf_esc, categoria_carrera_docente = pf_cat_carrera) %>% 
  group_by(id_colegio) %>%
  summarise(media_evaluacion = mean(portafolio))
  


######################
# desempeño colegios
###################

desempenio_basica_edit <- desempenio_basica %>% 
  clean_names() %>% 
  mutate(categoria_desempeno_2019_basica = categoria_desempeno_2019) %>% 
  select(categoria_desempeno_2019_basica, rbd, nombre_establecimiento )

desempenio_media_edit <- desempenio_media %>% 
  clean_names() %>% 
  mutate(categoria_desempeno_2019_media = categoria_desempeno_2019) %>% 
  select(categoria_desempeno_2019_media, rbd )


###########
# Unir todo
###########

colegios_final <- colegios_activos %>% 
  left_join(simce2_edit, by = "id_colegio") %>% 
  left_join(simce4_edit, by = "id_colegio") %>% 
  left_join(dotacion_colegios_edit, by = "id_colegio") %>% 
  left_join(desempenio_basica_edit, by = "rbd") %>% 
  left_join(desempenio_media_edit, by = "rbd") %>% 
  mutate(horas_por_alumno = hh_tot / mat_total,
         horas_aula_por_alumno = hh_a / mat_total,
         profesores_por_alumno = dc_a / mat_total 
         )

arrow::write_feather(colegios_final, "data/outputs/indicadores_colegios.feather")

##################
# Mirar promedios
################

colegios_final %>% 
  group_by(nom_com_rbd) %>% 
  summarise(horas = mean(horas_por_alumno),
            adecuado_lenguaje2 = mean(adecuado_lenguaje2, na.rm = T),
            adecuado_lenguaje4 = mean(adecuado_lenguaje4, na.rm = T)
            ) %>% 
  view()


colegios_final %>% 
  group_by(cod_depe) %>% 
  summarise(horas = mean(horas_por_alumno, na.rm = T),
            adecuado_lenguaje2 = mean(adecuado_lenguaje2, na.rm = T),
            adecuado_lenguaje4 = mean(adecuado_lenguaje4, na.rm = T),
            horas_aula = mean(horas_aula_por_alumno, na.rm = T),
            media_evaluacion = mean(media_evaluacion, na.rm = T),
            profesores = mean(profesores_por_alumno, na.rm = T)
  ) %>% 
  view()

rm <- colegios_final %>% 
  filter(cod_reg_rbd == 13)

# Correlaciones
cor(rm$horas_aula_por_alumno, rm$adecuado_mate2, use = "complete.obs")
cor(rm$profesores_por_alumno, rm$adecuado_mate4, use = "complete.obs")
cor(rm$profesores_por_alumno, rm$adecuado_mate2, use = "complete.obs")
cor(rm$horas_aula_por_alumno, rm$adecuado_mate4, use = "complete.obs")

##################
# Visualización
################


rm <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  filter(nombre_comuna != "San Jose de Maipo")

colegios_rm <- colegios_final %>% 
  filter(cod_reg_rbd == 13 & !is.na(categoria_desempeno_2019_basica)) %>%  
  filter(! categoria_desempeno_2019_basica %in% c("MEDIO-BAJO (NUEVO)", 
                                                  "SIN CATEGORIA: BAJA MATRICULA",
                                                  "SIN CATEGORIA: FALTA DE INFORMACIÓN"
                                                  ) ) %>% 
  slice_sample(n = 500)

ggplot(rm) + 
  geom_sf(aes( geometry = geometry)) +
  geom_point(data = colegios_rm, aes(longitud, latitud, color = categoria_desempeno_2019_basica ), size = 0.2)
  
colegios_rm_simce <- colegios_final %>% 
  filter(cod_reg_rbd == 13 & !is.na(adecuado_lenguaje2)) %>%  
  filter(! categoria_desempeno_2019_basica %in% c("MEDIO-BAJO (NUEVO)", 
                                                  "SIN CATEGORIA: BAJA MATRICULA",
                                                  "SIN CATEGORIA: FALTA DE INFORMACIÓN"
  ) ) %>% 
  
  slice_sample(n = 500)


ggplot(rm) + 
  geom_sf(aes( geometry = geometry)) +
  geom_point(data = colegios_rm_simce, aes(longitud, latitud, color =  adecuado_lenguaje2 ), size = 0.4)


