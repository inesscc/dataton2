library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(shiny)
library(sf)
library(mapview)
library(plotly)

## estructura de datos esperada
# 1 región
# 2 comuna
# 3 manzana
# 4 RBD colegio mas cercano
# 5 distancia colegio (minima en metroz)
# 6 población
# 7 calidad de la institución
# 8 NSE (manzana)
# 9 geometria manzana
# 10 geometry point colegio
# _____________________________________________________________________

# 1. # creamos datos de juguete
# data = chilemapas::censo_2017_comunas %>%
#   group_by(codigo_comuna) %>%
#   summarise(poblacion = sum(poblacion)) %>%
#   left_join(chilemapas::codigos_territoriales %>% select(-codigo_provincia,-nombre_provincia)) %>%
#   right_join(chilemapas::mapa_comunas) %>%
#   mutate(dist = runif(n()),
#          qa = runif(n()),
#          nse = runif(n())) %>%
#    select(pop =poblacion, everything()) %>%
#    sf::st_as_sf()
# #
#data <-  sf::read_sf("data/shapes/Manzanas/pob_manzana_censo2017.shp")
# # 2. # guardamos en formato shape
# 
n_reg <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
# 
# # 2.1 separamos el dato por regiones y lo guardamos
# purrr::map(n_reg,~dir.create(paste0("app/data/tabla_shiny/r", .x)))

# data <- data %>% 
#    mutate(REGION = as.numeric(REGION))
 
# fe = split(data,data$REGION)
# 
# purrr::walk2(fe,n_reg,~st_write(.x,paste0("app/data/tabla_shiny/r",.y,"/datos.shp")))

### función para lectura de datos
read_regional_data <- function(region){
  sf::read_sf(paste0("data/tabla_shiny/r",region,"/datos.shp"))
}



## 3. # construimos listas para inputs

## lista de variables

l_variables <- list("dist","dst_pnd","prm_smt")

names(l_variables) <- c("distance","Weighted distance","quality")

## lista de regiones
l_region <- as.list(n_reg)

names(l_region) <- c("TARAPACÁ","ANTOFAGASTA","ATACAMA","COQUIMBO","VALPARAÍSO","O'HIGGINS","MAULE","BIOBÍO","LA ARAUCANÍA",
  "LOS LAGOS","AYSÉN","MAGALLANES","METROPOLITANA","LOS RÍOS","ARICA Y PARINACOTA")


## 4. Construimos shiny app

ui <- fluidPage(

  shiny::titlePanel("Detection of territorial educational exclusion"),
  sidebarLayout(
  # sidebar
    sidebarPanel(
      selectInput("input_region","Region selection",choices = l_region, selected = "12"),
      radioButtons("input_variable","Variable",choices = l_variables, selected = "dist")


  ),
  # main panel
  mainPanel(

    ## opción con plotly
    #plotly::plotlyOutput("ggplot"),

    ## opción con ggplot
    plotOutput("ggplot")

      )
   )
)

server <- function(input, output, session) {



 plot <-  reactive({

   print(input$input_variable)
   print(input$input_region)

  ## opción con ggplot
   p <- ggplot(read_regional_data(input$input_region), aes(personas = totl_prs) ) + # %>% mutate(dist = if_else(totl_prs <= 5, NA, dist)
     geom_sf() +
     geom_sf(aes_string(fill = input$input_variable), lwd = 0) +
     scale_fill_continuous(high = "red", low = "green") +
     theme_bw()
   #ggplotly(p, tooltip = c("personas"))
   p

   
   # metro <- sf::read_sf(paste0("app/data/tabla_shiny/r","6","/datos.shp"))
   # p <- ggplot(metro, aes(z = totl_prs)) +
   #    geom_sf() +
   #    geom_sf(aes_string(fill = "prm_smt"), lwd = 0) +
   #    scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
   #   theme_bw()
   # # 
   #     ggplotly(p)
   #    

## opción con plotly
   # plotly::plot_ly(
   # read_regional_data(input$input_region),
   #    #split = ~nmbr_cm,
   #    #color = ~var_in,
   #    alpha = 1,
   #    showlegend = FALSE
   #  )
 })


 
 
 
output$ggplot <- renderPlot(width = 800, height = 700, {
   plot()
 })


## opción con plotly

 # output$ggplot <- renderPlotly({
 #   plot()
 # })



}

shinyApp(ui, server)
