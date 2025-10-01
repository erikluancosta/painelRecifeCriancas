# Carregamento dos dados
load('dados/cnes.RData')
#load('dados/pontos_viol.RData')
load('dados/cnes_join.RData')
#load('dados/pontos_viol_real.RData')
#pontos_viol <- qs::qread('dados/pontos_viol_real.qs')

pontos_viol <- pontos_viol |> 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

df_linha_vida2 <- df_linha_vida |>
  left_join(cnes_join, by = "id_unico")

# Une os dados de pontos com os dados de linha de vida pela coluna "id_unico"
pontos_viol <- pontos_viol |>
  left_join(df_linha_vida2, by = c("id_unico" = "id_unico"))

pontos_viol <- pontos_viol |>
  mutate(cd_cnes_unid_not = as.numeric(cd_cnes_unid_not))

# Converte colunas de data no dataframe df_linha_vida2
df_linha_vida2 <- df_linha_vida2 |>
  mutate(
    dt_evento_inicio = as.Date(dt_evento_inicio),
    dt_evento_fim = as.Date(dt_evento_fim),
    dt_registro = as.Date(dt_registro),
    dt_comum = coalesce(dt_registro, dt_evento_inicio, dt_evento_fim)
  )

# Ler o arquivo GeoJSON do mapa
mapa <- st_read("dados/bairros.geojson")

# Módulo UI do mapa
mapa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(HTML(paste0("
      #mapa_container {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
      }
      .leaflet-container {
        height: 100vh !important;
        width: 100vw !important;
      }
      #", ns("filtros"), ", #", ns("mapa_panel"), " {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0px 0px 15px rgba(0, 0, 0, 0.3);
        display: none;
      }
    "))),
    div(id = "mapa_container", 
        leafletOutput(ns("mapa"))
    ),
    # Botão para mostrar/ocultar filtros
    absolutePanel(
      top = 80, right = 20,
      actionButton(ns("toggle_filtros"), "Filtros")
    ),
    # Painel de filtros
    absolutePanel(
      top = 130, right = 20,
      id = ns("filtros"),
      h4("Filtros"),
      numericInput(
        ns("filtro_id_pessoa"), "ID Pessoa:",
        value = NA,
        min = min(pontos_viol$id_pessoa, na.rm = TRUE),
        max = max(pontos_viol$id_pessoa, na.rm = TRUE),
        step = 0
      ),
      sliderInput(
        ns("filtro_idade"), "Idade:",
        min = min(pontos_viol$nu_idade_anos, na.rm = TRUE),
        max = max(pontos_viol$nu_idade_anos, na.rm = TRUE),
        value = c(min(pontos_viol$nu_idade_anos, na.rm = TRUE), 
                  max(pontos_viol$nu_idade_anos, na.rm = TRUE))
      ),
      selectInput(
        ns("filtro_raca"), "Raça/cor:",
        choices = c("Todas", "Branca", "Preta", "Parda", "Amarela", "Indígena", "Ignorada"),
        selected = "Todas"
      )
    ),
    # Botão para mostrar/ocultar opções do mapa
    #absolutePanel(
    #  top = 80, right = 120,
    #  actionButton(ns("toggle_mapa"), "Mapa")
    #),
    # Painel de opções do mapa
    absolutePanel(
      top = 130, right = 120,
      id = ns("mapa_panel"),
      h4("Mapa"),
      selectInput(
        ns("map_style"), "Estilo do mapa:",
        choices = c("Padrão", "Carto Positron"),
        selected = "Carto Positron"
      ),
      sliderInput(
        ns("fill_opacity"), "Intensidade do preenchimento:",
        min = 0, max = 1, value = 0.3, step = 0.1
      ),
      sliderInput(
        ns("line_opacity"), "Intensidade da borda:",
        min = 0, max = 1, value = 0.9, step = 0.1
      ),
      colourInput(
        ns("polygon_color"), "Cor do shapefile:",
        value = "#FFC73B"
      ),
      colourInput(
        ns("default_color"), "Cor padrão dos pontos:",
        value = "#121E87"
      ),
      colourInput(
        ns("highlight_color"), "Cor do destaque:",
        value = "#FF5054"
      ),
      actionButton(ns("toggle_heatmap"), "Ativar Heatmap")
    )
  )
}