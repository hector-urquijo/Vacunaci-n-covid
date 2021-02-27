library(shiny)
library(shinydashboard)
library(tmap)
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(mapview)
library(ggplot2)
library(rmapshaper)
library(lattice)
library(leaflet)
library(leafpop)
library(viridis)
library(readxl)
library(xlsx)

#directorio de trabjo  
setwd("C:/Users/hecto/OneDrive/Documentos/hv hu 2020/vacunacion_covid/Vacunacion-covid")
#cargar mapa
#covidmundo <- st_read("https://raw.githubusercontent.com/hector-urquijo/Vacunacion-covid/main/Paises_Mundo.shp")
covidmundo <- st_read("Paises_Mundo.shp")
vacunacovid <- read_excel("proyecto politico 2022.xlsx", sheet = "covid_mundialr")
covidmundo <- merge(covidmundo, vacunacovid, by = "PAÍS", all = TRUE)

Paises_en_proceso_de_vacunacion <- subset(covidmundo, por_poblacion_vacunada != " "  ,
                                          select=c(PAÍS,por_poblacion_vacunada))
Vacunacion_mundial <- covidmundo
#tmap_mode("view")
ui  <-  fluidPage ( 
 # tmapOutput ( "mapacov" ), 
  leafletOutput("mapacov")
  #  selectInput ( "var" ,  "Variable" ,  world_vars ) 
  
)

server = function(input, output) {
  #graficar
    output$mapacov <- renderLeaflet ({
    tm <- tm_shape(Vacunacion_mundial) + tm_polygons(alpha = 0.5) +
          tm_shape(Paises_en_proceso_de_vacunacion) + tm_polygons(col= "por_poblacion_vacunada",
                                                              alpha = 0.5,palette = "YlGnBu",
                                                             legend.show=FALSE)
    tmap_leaflet(tm)
})
  
}
shinyApp(ui, server)   

