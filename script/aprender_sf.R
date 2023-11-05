
library(sf)
library(chilemapas)

r = sqrt(2)/10
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))
pt3 = st_point(c(.9,.1))
b1 = st_buffer(pt1, r)
b2 = st_buffer(pt2, r)
b3 = st_buffer(pt3, r)
(ls0 = st_nearest_points(b1, b2)) # sfg



poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 13) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)


punto <- data.frame(x = -70.661978, y = -33.453925)


r = sqrt(2)/10
pt1 = st_point(c(70.661978,-33.45392))
b1 = st_buffer(pt1, r)
b1[[1]]

class(b1)

st_as_sf(b1)

figura <- data.frame(geometry = b1)
class(comunas_los_rios$geometry)

ggplot(comunas_los_rios) + 
  geom_sf(aes( geometry = geometry)) + 
  #geom_sf(data = figura, aes( geometry = geometry) ) +
  geom_point(data = punto, aes(x, y) )




r = sqrt(2)/10
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))
pt3 = st_point(c(.9,.1))
b1 = st_buffer(pt1, r)
b2 = st_buffer(pt2, r)
b3 = st_buffer(pt3, r)
(ls0 = st_nearest_points(b1, b2)) # sfg

(ls = st_nearest_points(st_sfc(b1), st_sfc(b2, b3))) # sfc
#> Geometry set for 2 features 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0.2 ymin: 0.1 xmax: 0.8 ymax: 0.8
#> CRS:           NA
#> LINESTRING (0.2 0.2, 0.8 0.8)
#> LINESTRING (0.2414214 0.1, 0.7585786 0.1)
plot(b1, xlim = c(-.2,1.2), ylim = c(-.2,1.2), col = NA, border = 'green')
plot(st_sfc(b2, b3), add = TRUE, col = NA, border = 'blue')
plot(ls, add = TRUE, col = 'red')
plot()

