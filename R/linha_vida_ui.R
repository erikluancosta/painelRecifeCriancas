# Carregando linha da vida
#load("dados/linha_vida3.RData")

df_linha_vida <- df_linha_vida |> 
  group_by(id_pessoa) |> 
  mutate(
    so_sinan = ifelse(all(banco == "SINAN_VIOL"), 1, 0)
  ) |> 
  ungroup()

# Faixa etária
df_linha_vida <- df_linha_vida |> 
  group_by(id_pessoa) |> 
  mutate(idade_minima = min(nu_idade_anos, na.rm = TRUE)) |> 
  ungroup() |> 
  faixa_etaria_func()

linhavida_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = strong("Reconstituição de Trajetórias – Linha da Vida"),
        width = 12,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        p("Veja no gráfico abaixo a linha da vida de cada uma das mulheres identificadas nos bancos de dados. Considere que cada linha corresponde a uma mulher e em cada ícone é possível verificar por qual sistema de saúde ela passou."),
        p(HTML('<b>Como usar o gráfico interativo</b>')),
        tags$ul(
          tags$li(HTML("<strong>Destacar informações de uma mulher específica:</strong> Para visualizar detalhes sobre uma mulher que sofreu violência, basta clicar em um dos pontos que representam um registro dela em um dos sistemas de informação.")),
          tags$li(HTML("<strong>Desmarcar a seleção:</strong> Para desmarcar a mulher selecionada e retornar à visão geral, clique duas vezes fora do ponto selecionado.")),
          tags$li(HTML("<strong>Aplicar filtros:</strong> Para visualizar recortes específicos dos dados, utilize os filtros disponíveis. Selecione as características desejadas e o gráfico será atualizado automaticamente para exibir apenas as mulheres que correspondem aos critérios selecionados.")),
          tags$li(HTML("<strong>Filtro <em>Exclusivo SINAN Violências</em>:</strong> Ao selecionar essa opção no filtro de bancos de dados, o gráfico apresentará <u>somente</u> as mulheres cujos registros estão exclusivamente no SINAN Violências. Você ainda pode combinar esse recorte com os demais filtros (raça/cor, faixa etária, tipo de violência, etc.). Se, depois de marcado <em>Exclusivo SINAN Violências</em>, você marcar qualquer outro banco (SIM, SIH, SINAN Intoxicação Exógena, etc.), o modo exclusivo é desativado automaticamente e o gráfico volta a mostrar os resultados de todos os bancos selecionados."))
        ),
        fluidRow(
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_raca"),
                               label = strong("Raça/cor"),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = raca_cor_filtros,
                               selected = c("Branca", "Preta", "Parda"))
                 )),
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_banco"),
                               label = strong("Banco de dados"),
                               multiple = TRUE,
                               choices = c(
                                 "Exclusivo SINAN Violências"     = "EXCLUSIVO_SINAN_VIOL",
                                 "SINAN Violências" = "SINAN_VIOL",
                                 "SIM", "SIH",
                                 "SINAN Intoxicação Exógena" = "SINAN_IEXO"),
                               selected = c("SIM"))
                 )),
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
                 ))
        ),
        fluidRow(
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_violencias2"),
                               label = "Tipo de Violência",
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c(
                                 "Violência física" = "fl_viol_fisic", 
                                 "Violência psicológica" = "fl_viol_psico", 
                                 "Tortura" = "fl_viol_tort",
                                 "Violência sexual" = "fl_viol_sexu",
                                 "Tráfico de humanos" = "fl_viol_traf",
                                 "Violência financeira" = "fl_viol_finan",
                                 "Negligência" = "fl_viol_negli",
                                 "Trabalho infantil" = "fl_viol_infan",
                                 "Intervenção legal" = "fl_viol_legal",
                                 "Outras violências" = "fl_viol_outr"
                               ),
                               selected = NULL)
                 )),
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_autoprovocada"),
                               label = strong("Violência auto provocada"),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c("Sim" = 1, "Não" = 0),
                               selected = c(1, 0))
                 )),
          column(4,
                 wellPanel(
                   pickerInput(inputId = ns("filtro_obitos"),
                               label = "Óbitos",
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 noneSelectedText = "Nenhuma seleção"
                               ),
                               choices = c("Sim" = 1, "Não" = 0),
                               selected = c(1, 0))
                 ))
          
        ),
        
        div(
          HTML("<b>Legenda:</b>"),
          span(style = "color:#0099D6; font-size: 20px;", HTML("&#9670;")), " SINAN Intoxicação Exogena       ",
          #span(style = "color:#3bd80d; font-size: 23px;", HTML("&#9679;")), " Esus APS       ",
          span(style = "color:#FFC73B; font-size: 17px;", HTML("&#9650;")), " SIH       ",
          span(style = "color:#121E87; font-size: 17px;", HTML("&#9632;")), " SIM       ",
          span(style = "color:#ff5054; font-size: 12px;", HTML("&#10060;")), " SINAN Violências       "
        ),
        fluidRow(
          column(
            width = 8,
            plotlyOutput(ns("linha_vida_geral_"), height = "600px")
          ),
          column(
            width = 4,
            div(
              style = "height: 520px;",
              uiOutput(ns("selected_point_info"))
            )
          )
        ),
        p(''),
        p("Após o linkage, para cada banco de dados, foi selecionada a data mais importante de cada evento: no SINAN, a data de notificação; no SIH, a data da internação; nos Boletins de Ocorrência, a data do BO; no SIM, a data do óbito; e nos Boletins de Ocorrência Letais, a data do óbito. A partir dessas datas, foi possível construir a linha da vida das mulheres que tiveram notificação de violência no SINAN, permitindo um acompanhamento detalhado dos eventos ao longo do tempo.")
      )
    )
  )
}
