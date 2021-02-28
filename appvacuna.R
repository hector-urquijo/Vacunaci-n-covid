
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
#vacunacovid <- read_excel("proyecto politico 2022.xlsx", sheet = "covid_mundialr")
urlcov <- "https://raw.githubusercontent.com/hector-urquijo/Vacunacion-covid/main/data_vacunacion.xlsx"
download.file(urlcov,destfile = "datavac.xlsx",mod="wb")
vacunacovid <- read_excel("datavac.xlsx")

Vacunacion_mundial <- merge(x= covidmundo, y=vacunacovid,  all = TRUE)
Paises_en_proceso_de_vacunacion <- subset(Vacunacion_mundial, por_poblacion_vacunada != " "  ,
                                          select=c(PAÍS,por_poblacion_vacunada))
America <- filter(Vacunacion_mundial, Continente == "America")
Vacunacion_por_pais_america <- subset(Vacunacion_mundial, Continente == "America" ,
                             select = c(PAÍS, por_poblacion_vacunada))
                             
#tmap_mode("view")
ui  <-  fluidPage ( 
  titlePanel("Vacunación Covid 19 por pais"),
  # tmapOutput ( "mapacov" ), 
  mainPanel(
  tabsetPanel(position = "below",  
  tabPanel("Mundial", leafletOutput("mapacov")),
  tabPanel("America", leafletOutput("mapamerica"))
  #  selectInput ( "var" ,  "Variable" ,  world_vars ) 
  )
)
)
server = function(input, output) {
  #graficar
  output$mapacov <- renderLeaflet ({
    tm <- tm_shape(Vacunacion_mundial) + tm_polygons(alpha = 0.5) +
      tm_shape(Paises_en_proceso_de_vacunacion) + 
      tm_polygons(col= "por_poblacion_vacunada",alpha = 0.5,palette = "YlGnBu",
                                                              legend.show=FALSE)
    tmap_leaflet(tm)
  })
output$mapamerica <- renderLeaflet({  
  ma <- tm_shape(America) + tm_polygons(alpha = 0.3)+
    tm_shape(Vacunacion_por_pais_america) + tm_polygons("por_poblacion_vacunada",alpha = 0.5,palette = "YlGnBu",
                                                   legend.show=FALSE) 
  tmap_leaflet(ma)
}) 
}
shinyApp(ui, server)   


