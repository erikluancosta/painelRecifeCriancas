#load('dados/df_sim.RData')


df_sim <- df_sim |> 
  faixa_etaria_func()

sim_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Filtros",
        collapsible = FALSE,
        maximizable = FALSE,
        fluidRow(
          column(4,
                 wellPanel(
                   pickerInput(
                     inputId = ns("filtro_idade"),
                     label = strong("Faixa Etária"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       noneSelectedText = "Nenhuma seleção"
                     ),
                     choices = faixa_etarias_filtros,
                     selected = faixa_etarias_filtros
                   )
                 )
          ),
          column(4,
                 wellPanel(
                   pickerInput(
                     inputId = ns("filtro_raca"),
                     label = strong("Raça/cor"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       noneSelectedText = "Nenhuma seleção"
                     ),
                     choices = raca_cor_filtros,
                     selected = raca_cor_filtros
                   )
                 )
          ),
          column(4,
                 wellPanel(
                   pickerInput(
                     inputId = ns("filtro_ano"),
                     label = strong("Ano"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       noneSelectedText = "Nenhuma seleção"
                     ),
                     choices = ano_filtros,
                     selected = ano_filtros
                   )
                 )
          )
        )
      )
    ),
    
    fluidRow(
      
      box(title=strong("Frequência de notificação por ano"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("freq_ano_graf"))
      )
    ),
    
    fluidRow(
      box(
        title = strong("Faixa etária"),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("faixa_etaria_graf"), height = "400px"),  # Aumenta a altura do gráfico
        downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")
      ),
      
      box(
        title = strong("Raça/cor"),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("raca_cor_graf"), height = "400px"),  # Aumenta a altura do gráfico
        downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela")
      )
    ),
    
    fluidRow(
      box(
        title = strong("Proporção de óbitos por causas externas (capítulo XIX e XX) da CID-10, 2016-2022"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        div(
          style = "overflow-y: scroll;",
          plotlyOutput(ns("graf_obito"), height = "900px")  # Ajusta a altura conforme necessário
        )
      )
    ),
    
    fluidRow(
      column(12,
             dataTableOutput(ns("sim"))
      )
    )
  )
}