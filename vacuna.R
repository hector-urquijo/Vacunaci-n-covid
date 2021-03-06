library(shiny)
library(shinydashboard)
library(tmap)
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(mapview)
library(ggplot2)
library(dplyr)
library(rmapshaper)
library(lattice)
library(leaflet)
library(leafpop)
library(viridis)
library(readxl)
library(xlsx)

#directorio de trabjo  
setwd("C:/Users/hecto/OneDrive/Documentos/hv hu 2020/vacunacion_covid/Vacunacion-covid")
#cargar mapa   https://raw.githubusercontent.com/hector-urquijo/Vacunacion-covid/main/Paises_Mundo.shp
#covidmundo <- st_read("https://github.com/hector-urquijo/Vacunacion-covid/raw/main/Paises_Mundo.shp")
covidmundo <- st_read("Paises_Mundo.shp")
#vacunacovid <- read_excel("data_vacunacion.xlsx") #proyecto politico 2022.xlsx", sheet = "covid_mundialr
urlcov <- "https://raw.githubusercontent.com/hector-urquijo/Vacunacion-covid/main/data_vacunacion.xlsx"
download.file(urlcov,destfile = "datavac.xlsx",mod="wb")
vacunacovid <- read_excel("datavac.xlsx")

Vacunacion_mundial <- merge(x= covidmundo, y=vacunacovid,  all = TRUE)
Paises_en_proceso_de_vacunacion <- subset(Vacunacion_mundial, por_poblacion_vacunada != " "  ,
                                          select=c(PA�S,por_poblacion_vacunada))
#Vacunacion_mundial <- covidmundo
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

