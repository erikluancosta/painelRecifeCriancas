source('global.R')
source('R/funcoes/faixa_etaria_func.R')

#df_iexo <- readr::read_csv2('dados/tela_sinan_iexo.csv', col_types = cols(.default = col_character()))

df_iexo <- df_iexo |> 
  faixa_etaria_func()


iexo_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(box
             (width = 12,
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
                            choices = faixa_etarias_filtros,
                            selected = faixa_etarias_filtros,
                            options = list(
                              `actions-box` = TRUE,
                              noneSelectedText = "Nenhuma seleção"
                            )
                          ))),
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
                            selected = raca_cor_filtros)
                        )),
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
                 
               ),
               fluidRow(
                 column(12,
                        wellPanel(
                          pickerInput(
                            inputId = ns("filtro_circuns"),
                            label = "Circunstância",
                            multiple = TRUE,
                            options = list(
                              `actions-box` = TRUE,
                              noneSelectedText = "Nenhuma seleção"
                            ),
                            choices = (df_iexo$ds_circunstan) |> unique() |> sort(),
                            selected = df_iexo$ds_circunstan)
                        ))
                 
                 
               )
             )),
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
      
      box(title = strong('Faixa etária'),
          width = 6,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("faixa_etaria_graf")),
          downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")),
      
      box(title = strong('Raça/cor'),
          width = 6,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("raca_cor_graf")),
          downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      
      box(title=strong("Proporção por Circunstância"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("circunstancia_graf")),
          downloadButton(outputId = ns("download_tab_circunstancia"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      
      box(title=strong("Proporção por agente intoxicante"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("ag_intox_graf")),
          downloadButton(outputId = ns("download_tab_ag_intox"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      
      box(title=strong("Proporção de tipo de atendimento por hospitalização"),
          width = 12,
          status = "secondary",
          maximizable = FALSE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("atend_hospit_graf")),
          downloadButton(outputId = ns("download_atend_hospit_graf"), label = "Download da Tabela"))
    )
  )
  
}
