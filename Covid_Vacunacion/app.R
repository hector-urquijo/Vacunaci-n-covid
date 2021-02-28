#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#library(shinydashboard)
library(shiny)
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

#directorio de trabajo  
setwd("C:/Users/hecto/OneDrive/Documentos/hv hu 2020/vacunacion_covid/Vacunacion-covid/Covid_Vacunacion")
#covidmundo <- st_read("https://github.com/hector-urquijo/Vacunacion-covid/raw/main/Paises_Mundo.shp")
covidmundo <- st_read("Paises_Mundo.shp")
#urlcov <- "https://raw.githubusercontent.com/hector-urquijo/Vacunacion-covid/main/data_vacunacion.xlsx"
#download.file(urlcov,destfile = "datavac.xlsx",mod="wb")
#vacunacovid <- read_excel("datavac.xlsx")
vacunacovid <- read_excel("data_vacunacion.xlsx")
Vacunacion_mundial <- merge(x= covidmundo, y=vacunacovid,  all = TRUE)
Paises_en_proceso_de_vacunacion <- subset(Vacunacion_mundial, por_poblacion_vacunada != " "  ,
                                          select=c(PAÍS,por_poblacion_vacunada))
America <- filter(Vacunacion_mundial, Continente == "America")
Vacunacion_por_pais_america <- subset(Vacunacion_mundial, Continente == "America" ,
                                      select = c(PAÍS, por_poblacion_vacunada))

# Define UI for application that draws a histogram
ui <- navbarPage("Vacunación Covid por país", #fluidPage ( 
                 mainPanel(
                   tabsetPanel(position = "below",  
                               tabPanel("Mundial", leafletOutput("mapacov")),
                               tabPanel("América", leafletOutput("mapamerica"))
                               #  selectInput ( "var" ,  "Variable" ,  world_vars ) 
                   )
                 )
)
    

# Define server logic required to draw a histogram
server <- function(input, output) {

  #graficar
  output$mapacov <- renderLeaflet ({
    tm <- tm_shape(Vacunacion_mundial) + tm_polygons(alpha = 0.5)+tmap_options(max.categories = 50) +
      tm_shape(Paises_en_proceso_de_vacunacion) + 
      tm_polygons(col= "por_poblacion_vacunada",alpha = 0.5,palette = "YlGnBu",
                  legend.show=FALSE)+tmap_options(max.categories = 50)
    tmap_leaflet(tm)
  })
  output$mapamerica <- renderLeaflet({  
    ma <- tm_shape(America) + tm_polygons(alpha = 0.3)+tmap_options(max.categories = 50)+
      tm_shape(Vacunacion_por_pais_america) + tm_polygons("por_poblacion_vacunada",alpha = 0.5,palette = "YlGnBu",
                                                          legend.show=FALSE) +tmap_options(max.categories = 50)
    tmap_leaflet(ma)
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
