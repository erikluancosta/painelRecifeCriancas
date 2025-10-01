# -----------------
# Bases e pré‑processamento
# -----------------
#load("dados/base_linkage4.RData")   # objeto: base_linkage

# Base principal de linkage (para as caixas/filtros)
base_linkage <- base_linkage |>
  select(id_pareamento, id_pessoa, ds_raca, faixa_etaria_padrao,
         banco, cd_causabas,
         fl_viol_fisic, fl_viol_psico, fl_viol_tort, fl_viol_sexu,
         fl_viol_traf, fl_viol_finan, fl_viol_negli, fl_viol_infan,
         fl_viol_legal, fl_viol_outr, FL_ESUS_APS, FL_SINAN_VIOL, FL_SINAN_IEXO,
         sg_sexo, linkada)

# Base de óbitos (SIM)
df_obitos <- base_linkage |> filter(banco == "SIM")

# Dicionário CID
load("dados/icd_map_ufmg.Rdata")      # objeto: icd_map
icd_map <- icd_map |>
  rename(cd_causabas = icd_code_4,
         causa_resumida = CIDBR_RESUMIDO_EXTERNAS) |>
  select(cd_causabas, causa_resumida) |> 
  mutate(
    causa_resumida = case_when(
      causa_resumida == "Ext W40-W49 \tEnvenenamento, intoxicação por ou exposição a substâncias nocivas" ~ "Ext Outras",
      TRUE ~ causa_resumida
    )
  )

df_obitos <- df_obitos |>
  left_join(icd_map, by = "cd_causabas") |>
  mutate(causa_resumida = case_when(
    causa_resumida == "Ignorado" ~ "Ignorado",
    TRUE                         ~ causa_resumida
  ))

#Grafico comparativo entre bancos
viol_iexo <- base_linkage |>
  filter(banco %in% c("SINAN_VIOL", "SINAN_IEXO")) |> 
  select(banco, FL_SINAN_VIOL, FL_SINAN_IEXO, id_pessoa)

so_viol <- viol_iexo |> 
  filter(FL_SINAN_VIOL == 1, FL_SINAN_IEXO != 1) |> 
  select(id_pessoa) |> 
  distinct() |> 
  nrow()

so_iexo <- viol_iexo |> 
  filter(FL_SINAN_VIOL != 1, FL_SINAN_IEXO == 1) |> 
  select(id_pessoa) |> 
  distinct() |> 
  nrow()

ambos <- viol_iexo |> 
  filter(FL_SINAN_VIOL == 1, FL_SINAN_IEXO == 1) |> 
  select(id_pessoa) |> 
  distinct() |> 
  nrow()


categoria <- c("SINAN Violências", "SINAN Intoxicação Exógena", "Ambos")
quantidade <- c(so_viol, so_iexo, ambos)


# Monta o dataframe
df <- data.frame(
  banco = categoria,
  n = quantidade,
  cor = c("#FF5054", 
          "#FFC73B", 
          "#0099D6")
)



# ================================================================
# UI
# ================================================================
linkage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML(".info-box .info-box-number { font-size: 32px; }"))
    ),
    fluidRow(
      box(width = 12,
          title = strong("Filtros"),
          collapsible = FALSE,
          fluidRow(
            column(4,
                   wellPanel(
                     pickerInput(
                       inputId = ns("filtro_idade"),
                       label = "Faixa Etária",
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE),
                       choices = faixa_etarias_filtros,
                       selected = faixa_etarias_filtros
                     )
                   )
            ),
            column(4,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_raca"),
                                 label = "Raça/cor",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
                                 choices = raca_cor_filtros,
                                 selected = raca_cor_filtros
                     )
                   )
            ),
            column(4,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_banco"),
                                 label = "Banco de dados",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
                                 choices = c("SIM",
                                             "SIH",
                                             "SINAN Violências" = "SINAN_VIOL",
                                             "SINAN Intox Exogena"="SINAN_IEXO"),
                                 selected = c("SIM", "SIH", "SINAN_VIOL", "SINAN_IEXO")
                     )
                   )
            )
          ),
          fluidRow(
            column(8,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_violencias"),
                                 label = "Tipo de Violência",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
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
                   )
            ),
            column(4,
                   wellPanel(
                     pickerInput(inputId = ns("filtro_esus"), #filtro_cadunico
                                 label = "Registro no ESUS APS",
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE),
                                 choices = c("Sim" = 1, "Não" = 0),
                                 selected = c(1, 0)
                     )
                   )
            )
          )
      )
    ),
    fluidRow(
      column(6,
             tags$div(
               id = ns("box_num_registros"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("num_registros"), width = 12)
             )
      ),
      column(6,
             tags$div(
               id = ns("box_reg_pareado"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("reg_pareado"), width = 12)
             )
      )
    ),
    fluidRow(
      column(6,
             tags$div(
               id = ns("box_mulheres"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("num_mulheres"), width = 12)
             )
      ),
      column(6,
             tags$div(
               id = ns("box_mulheres_pareadas"),
               class = "clickable-box",
               bs4Dash::infoBoxOutput(ns("mulheres_pareadas"), width = 12)
             )
      )
    ),
    fluidRow(
      box(
        title = strong('Faixa etária'),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("faixa_etaria_graf")),
        downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")
      ),
      box(
        title = strong('Raça/cor'),
        width = 6,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("raca_cor_graf")),
        downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela")
      )
    ),
    fluidRow(
      box(
        title = strong("Comparação das proporções das causas de óbito entre mulheres com notificação de violência e sem notificação de violência (2019 - 2022)"),
        width = 12,
        height = "720px", # <--- Added height argument
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("causas_obito_linkage"), height = "95%"), # Consider adding height = "100%" or similar here if needed
        downloadButton(outputId = ns("download_bolha"), label = "Download da Tabela")
      )
    ),
    fluidRow(
      box(
        title = strong("Número de mulheres no SINAN Violências, SINAN Intoxicação Exógena e ambos sistemas (2019 - 2022)"),
        width = 12,
        #height = "720px", # <--- Added height argument
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("graf_viol_iexo"), height = "100%"), # Consider adding height = "100%" or similar here if needed
        downloadButton(outputId = ns("download_viol_iexo"), label = "Download da Tabela")
      )
    )
  )
}