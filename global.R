options(bitmapType = "cairo")
library(DBI)
library(RPostgres)
library(ggplot2)
library(shiny)
library(bs4Dash)
library(ciTools)
library(lubridate)
library(vitaltable)
library(leaflet)
library(dplyr)
library(shinythemes)
library(janitor)
library(plotly)
library(reshape2)
library(forcats)
library(sf)
library(tidyr)
library(DT)
library(haven)
library(writexl)
library(fresh)
library(shinymanager)
library(shinyWidgets)
library(readr)
library(shinyjs)
library(colourpicker)
library(readxl)
library(leaflet.extras)
library(stringr)
library(qs)

#---------------------------
# Carrega os dados do painel
#---------------------------

## Tela Linkage
base_linkage <- qread("dados/base_linkage.qs")

# Tela SINAN Viol
df_sinan <- qread("dados/sinan_viol.qs")

# Tela SINAN Iexo
df_iexo <- qread("dados/df_iexo.qs")

# Tela SIH
df_sih <- qread("dados/df_sih.qs")

# Tela SIM
df_sim <- qread("dados/df_sim.qs")

# Linha de vida
df_linha_vida <- qread("dados/linha_vida.qs")

#Mapa
pontos_viol <- qread("dados/pontos_viol_real.qs")

#---------------------------
# Define o tema customizado com 'fresh'
#---------------------------
tema <- fresh::create_theme(
  fresh::bs4dash_status(
    info = "#121E87",
    secondary = "#FF5054",
    danger = "#FFC73B",
    primary = "#0099D6",
    warning = "#121E54"
  )
)

#---------------------------
# Lista de faixa etária
#---------------------------
faixa_etarias_filtros <- c(
  "<1", "01-04", "05-09", "10-14", "15-19",
  "20-29", "30-39", "40-49", "50-59", "60-69", 
  "70-79", "80+", "Ignorada"
)

#---------------------------
# Lista de raça/cor
#---------------------------
raca_cor_filtros <- c(
  "Branca", "Preta", "Parda", "Indígena", "Amarela", "Ignorada"
  )

#---------------------------
# Lista de anos
#---------------------------
ano_filtros <- c(2016, 2017, 2018,2019, 2020, 2021, 2022)



