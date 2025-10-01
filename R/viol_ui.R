source('global.R')
# Carregando os dados



df_sinan <- df_sinan |>
  mutate(
    rede_enc_sau = case_when((rede_sau == "1" | enc_saude == "1") ~ 1,
                             TRUE ~ 0),
    assit_soc_creas = case_when((assist_soc == "1" | enc_creas == "1") ~ 1,
                                TRUE ~ 0),
    atend_enc_mulh = case_when((atend_mulh == "1" | enc_mulher == "1") ~ 1,
                               TRUE ~ 0),
    cons_enc_tutela = case_when((cons_tutel == "1" | enc_tutela == "1") ~ 1,
                                TRUE ~ 0),
    mpu_enc_mpu = case_when((mpu == "1" | enc_mpu == "1") ~ 1,
                            TRUE ~ 0),
    deleg_enc_cria = case_when((deleg_cria == "1" | enc_dpca == "1") ~ 1,
                               TRUE ~ 0),
    deleg_enc_mulh = case_when((deleg_mulh == "1" | enc_deam == "1") ~ 1,
                               TRUE ~ 0),
    deleg_enc_deleg = case_when((deleg == "1" | enc_deleg == "1") ~ 1,
                                TRUE ~ 0),
    infan_enc_juv = case_when((infan_juv == "1" | enc_vara == "1") ~ 1),
    banco = "SINAN",
    ds_autor_sexo = case_when(
      autor_sexo == "1" ~ "Masculino",
      autor_sexo == "2" ~ "Feminino",
      autor_sexo == "3" ~ "Ambos os sexos",
      TRUE ~ "Ignorado"
    ),
    autor_alco = case_when(
      autor_alco == "1" ~ "Sim",
      autor_alco == "2" ~ "Não",
      TRUE ~ "Ignorado"
    ),
    les_autop = case_when(
      les_autop == "1" ~ "Sim",
      les_autop == "2" ~ "Não",
      les_autop == "9" | is.na(les_autop) ~ "Ignorado",
      TRUE ~ les_autop
    ),
    local_ocor = case_when(
      local_ocor == "01" ~ "Residência",
      local_ocor == "02" ~ "Habitação coletiva",
      local_ocor == "03" ~ "Escola",
      local_ocor == "04" ~ "Local de prática esportiva",
      local_ocor == "05" ~ "Bar ou similar",
      local_ocor == "06" ~ "Via publica",
      local_ocor == "07" ~ "Comércio/Serviços",
      local_ocor == "08" ~ "Industrias/ construção",
      local_ocor == "09" ~ "Outro",
      local_ocor == "99" ~ "Ignorado",
      TRUE ~ "Ignorado"
    ),
    out_vezes = case_when(
      out_vezes == "1" ~'Sim',
      out_vezes == "2" ~'Não',
      out_vezes == "9" ~'Ignorado',
      TRUE ~ "Ignorado"
    )
  ) |> 
  faixa_etaria_func()




# Categoria do sinan para o tipo de agressão
agc <- data.frame(
  categoria = c("ag_forca", "ag_enfor", "ag_objeto", "ag_corte", "ag_quente",
                "ag_enven", "ag_fogo", "ag_ameaca", "ag_outros"),
  ds_tp_ag = c("Força corporal / espancamento",
               "Enforcamento / sufocação",
               "Objeto contundente",
               "Objeto cortante / perfurante",
               "Substância quente",
               "Envenenamento",
               "Arma de fogo",
               "Ameaça",
               "Outros meios")
)

# Deficiêcnias do sinan
defic <- data.frame(
  categoria = c("def_trans", "def_fisica", "def_mental", "def_visual",
                "def_auditi", "def_out", "def_espec"),
  ds_tp_def = c("Transtorno mental ou comportamental",
                "Deficiência física",
                "Deficiência mental / intelectual",
                "Deficiência visual",
                "Deficiência auditiva",
                "Outras deficiências",
                "Deficiência não especificada")
)

# Transtornos 
transt <- data.frame(
  categoria = c("tran_ment", "tran_comp"),
  ds_tp_transtorno = c("Transtorno mental", "Transtorno comportamental")
)

# UI do módulo SINAN
sinan_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Filtros",
        collapsible = FALSE,
        maximizable = FALSE,
        fluidRow(
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_idade"),
                label = strong("Faixa Etária"),
                choices = faixa_etarias_filtros,
                selected = faixa_etarias_filtros,
                options = list(
                  `actions-box` = TRUE,
                  noneSelectedText = "Nenhuma seleção"
                ),
                multiple = TRUE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_raca"),
                label = strong("Raça/cor"),
                choices = raca_cor_filtros,
                selected = raca_cor_filtros,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_ano"),
                label = strong("Ano"),
                choices = ano_filtros,
                selected = ano_filtros,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          )
        ),
        fluidRow(
          column(
            8,
            wellPanel(
              pickerInput(
                inputId = ns("filtro_violencias"),
                label = "Tipo de Violência",
                choices = c(
                  "Violência física" = "viol_fisic", 
                  "Violência psicológica" = "viol_psico", 
                  "Violência sexual" = "viol_sexu",
                  "Tortura" = "viol_tort",
                  "Negligência" = "viol_negli",
                  "Violência financeira" = "viol_finan",
                  "Trabalho infantil" = "viol_infan",
                  "Intervenção legal" = "viol_legal",
                  "Tráfico de humanos" = "viol_traf",
                  "Outras violências" = "viol_outr"
                ),
                selected = NULL,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("les_autop_fil"),
                label = strong("Lesão autoprovocada"),
                choices = c("Sim", "Não", "Ignorado"),
                selected = c("Sim", "Não", "Ignorado"),
                options = list(`actions-box` = TRUE),
                multiple = TRUE
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
        title = strong("Informações do SINAN"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        # Primeira linha contendo os filtros
        fluidRow(
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("evolution_filter"),
                label = "Tipo de variável do SINAN:",
                choices = c("Encaminhamentos" = "enc", 
                            "Procedimentos" = "proc", 
                            "Relação com o agressor" = "rel", 
                            "Tipo de violência" = "viol",
                            "Meio de agressão" = "agc",
                            "Deficiências" = "defic",
                            "Transtornos" = "transt"),
                selected = "enc",
                options = list(`actions-box` = TRUE),
                multiple = FALSE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("extrato_sinan_filter"),
                label = "Extratificado por:", 
                choices = c("Raça/cor" = 'ds_raca', 
                            "Faixa etária" = 'faixa_etaria_padrao',
                            "Ano da notificação" = 'ano',
                            "Outras vezes" = "out_vezes",
                            "Local de ocorrência" = 'local_ocor'),
                selected = "ds_raca",
                options = list(`actions-box` = TRUE),
                multiple = FALSE
              )
            )
          ),
          column(
            4,
            wellPanel(
              pickerInput(
                inputId = ns("valor_sinan_filter"),
                label = "Valor:", 
                choices = c("Frequência" = FALSE,
                            "Porcentagem" = TRUE),
                selected = FALSE,
                options = list(`actions-box` = TRUE),
                multiple = FALSE
              )
            )
          )
        ),
        # Segunda linha contendo o gráfico e tabela
        fluidRow(
          column(
            12,
            dataTableOutput(ns("sinan")),
            downloadButton(outputId = ns("download_tab_sinan"), label = "Download da Tabela")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = strong("Sexo do agressor por suspeita do uso de álcool"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("sexo_alcool_graf"))
      )
    ),
    
    # TAB 2
    fluidRow(
      box(
        title = strong("Tabela cruzando informações"),
        width = 12,
        status = "secondary",
        maximizable = FALSE,
        closable = FALSE,
        solidHeader = TRUE,
        fluidRow(
          column(3,
                 wellPanel(
                   pickerInput(
                     inputId = ns("var1"),
                     label = "Variável da linha",
                     choices = c("Faixa etária" = 'faixa_etaria_padrao', 
                                 "Raça/cor" = 'ds_raca', 
                                 "Ano da notificação" = 'ano',
                                 "Outras vezes"='out_vezes',
                                 "Local de ocorrência" = 'local_ocor'),
                     selected = "faixa_etaria_padrao",
                     options = list(`actions-box` = TRUE),
                     multiple = FALSE
                   ),
                   pickerInput(
                     inputId = ns("var2"),
                     label = "Variável da coluna",
                     choices = c(#"Faixa etária" = 'faixa_etaria_padrao', 
                       "Raça/cor" = 'ds_raca', 
                       "Ano da notificação" = 'ano',
                       "Outras vezes"='out_vezes'
                       #"Local de ocorrência" = 'local_ocor'
                     ),
                     selected = "ds_raca",
                     options = list(`actions-box` = TRUE),
                     multiple = FALSE
                   ),
                   downloadButton(outputId = ns("download_tab2_sinan"), label = "Download da Tabela")
                 )
          ),
          column(9,
                 dataTableOutput(ns("tabela_cruzada"))
          )
        )
      )
    ))
}