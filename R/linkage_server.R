# ================================================================
# SERVER
# ================================================================
linkage_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns  # Get namespace
      
      # Initialize selected box
      selected_box <- reactiveVal("mulheres")  # Default selection
      
      # Capture clicks
      shinyjs::onclick("box_mulheres", {
        selected_box("mulheres")
      })
      
      shinyjs::onclick("box_mulheres_pareadas", {
        selected_box("mulheres_pareadas")
      })
      
      # Output Número de registros ----
      output$num_registros <- renderValueBox({
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",  # Define o tamanho da fonte aqui
            base_linkage |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
              ) |>
              nrow()
          ),
          subtitle = "Total de registros processados",
          color = "danger",
          icon = icon("address-card")
        )
      })
      
      output$reg_pareado <- renderValueBox({
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",
            base_linkage |>
              dplyr::filter(
                !is.na(id_pareamento),
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
              ) |>
              nrow()
          ),
          subtitle = "Registros processados de mulheres identificadas no linkage",
          color = "danger",
          icon = icon("code-compare")
        )
      })
      
      output$num_mulheres <- renderValueBox({
        color <- ifelse(selected_box() == "mulheres", "primary", "danger")
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",
            base_linkage |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
              ) |>
              distinct(id_pessoa) |>
              nrow()
          ),
          subtitle = "Total de mulheres distintas identificadas",
          icon = icon("venus"),
          color = color
        )
      })
      
      output$mulheres_pareadas <- renderValueBox({
        color <- ifelse(selected_box() == "mulheres_pareadas", "primary", "danger")
        valueBox(
          value = tags$div(
            style = "font-size: 38px;",
            base_linkage |>
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco,
                FL_ESUS_APS %in% input$filtro_esus
              ) |>
              distinct(id_pareamento) |>
              nrow()
          ),
          subtitle = "Número de mulheres identificadas no linkage",
          icon = icon("link"),
          color = color
        )
      })
      
      # Faixa etaria grafico ----
      output$faixa_etaria_graf <- renderPlotly({
        # Determine if 'Tipo de Violência' filter should be applied
        if (selected_box() == "mulheres_pareadas") {
          filtro_viol <- input$filtro_violencias
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- base_linkage %>%
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- base_linkage
          }
          # Apply linkage filter
          df_filtrado <- df_filtrado %>% filter(linkada == 1)
        } else {
          # Ignore 'Tipo de Violência' filter
          df_filtrado <- base_linkage
          # Apply linkage filter
          df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
        }
        
        a <-  df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco,
            FL_ESUS_APS %in% input$filtro_esus
          ) |>
          distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          faixa_etaria_graf()
      })
      
      # Faixa etária download
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          if (selected_box() == "mulheres_pareadas") {
            filtro_viol <- input$filtro_violencias
            if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
              df_filtrado <- base_linkage %>%
                filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
            } else {
              df_filtrado <- base_linkage
            }
            df_filtrado <- df_filtrado %>% filter(linkada == 1)
          } else {
            df_filtrado <- base_linkage
            df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
          }
          
          tabela_fxetaria <-  df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco,
              FL_ESUS_APS %in% input$filtro_esus
            ) |>
            distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          writexl::write_xlsx(tabela_fxetaria, path = file)
        }
      )
      
      # Raça/cor ----
      output$raca_cor_graf <- renderPlotly({
        if (selected_box() == "mulheres_pareadas") {
          filtro_viol <- input$filtro_violencias
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- base_linkage %>%
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- base_linkage
          }
          df_filtrado <- df_filtrado %>% filter(linkada == 1)
        } else {
          df_filtrado <- base_linkage
          df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
        }
        
        # Prepare os dados
        dados_preparados <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco,
            FL_ESUS_APS %in% input$filtro_esus
          ) |>
          distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          mutate(ds_raca = str_to_title(ds_raca)) |>
          raca_cor_graf()
      })
      
      # Raça/cor download
      output$download_tab_raca_cor <- downloadHandler(
        file = function() {
          paste("dados-raca-cor-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          if (selected_box() == "mulheres_pareadas") {
            filtro_viol <- input$filtro_violencias
            if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
              df_filtrado <- base_linkage %>%
                filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
            } else {
              df_filtrado <- base_linkage
            }
            df_filtrado <- df_filtrado %>% filter(linkada == 1)
          } else {
            df_filtrado <- base_linkage
            df_filtrado <- df_filtrado %>% filter(linkada %in% c(0,1))
          }
          
          tabela_raca <-  df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco,
              FL_ESUS_APS %in% input$filtro_esus
            ) |>
            distinct(id_pessoa, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(ds_raca)
          
          writexl::write_xlsx(tabela_raca, path = file)
        }
      )
      
      # ------ GRÁFICO DE BOLHAS (3 colunas) ------
      output$causas_obito_linkage <- renderPlotly({
        
        df_sim <- df_obitos  
        # Filtragem pelo tipo de violência, se existir filtro
        #f_viol <- input$filtro_violencias
        #df_sim <- if (!is.null(f_viol) && length(f_viol) > 0) {
        #  df_obitos |> filter_at(vars(all_of(f_viol)), any_vars(. == 1))
        #} else {
        #  df_obitos
        #}
        
        # ---- 1) Óbitos com notificação (SINAN VIOL) ----
        a <- df_sim |>
          filter(banco == "SIM", FL_SINAN_VIOL == 1) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          transmute(causa_resumida,
                    nivel = "Óbitos com notificação de violência",
                    valor = `%`,
                    n = n)
        
        # ---- 2) Óbitos – SINAN IEXO ----
        b <- df_sim |>
          filter(banco == "SIM", FL_SINAN_IEXO == 1) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          transmute(causa_resumida,
                    nivel = "Óbitos com notificação de intoxicação exógena",
                    valor = `%`,
                    n = n)
        
        # ---- 3) Óbitos sem notificação ----
        c <- df_sim |>
          filter(banco == "SIM", FL_SINAN_VIOL != 1, FL_SINAN_IEXO != 1) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          transmute(causa_resumida,
                    nivel = "Óbitos sem notificação",
                    valor = `%`,
                    n = n)
        
        # ---- Combina tudo em formato longo ----
        df_bolhas <- bind_rows(a, b, c)
        
        # 1. Todas as causas existentes em QUALQUER nível
        todas_causas <- df_bolhas |> distinct(causa_resumida) |> pull(causa_resumida)
        
        # 2. Causas ordenadas do maior para menor na coluna de "Óbitos com notificação de violência"
        ordem_causas_notif <- a |>
          arrange(desc(valor)) |>
          pull(causa_resumida)
        
        # 3. Causas que NÃO estão presentes nos óbitos com notificação
        causas_restantes <- setdiff(todas_causas, ordem_causas_notif)
        
        # 4. Concatena ordenados + restantes
        ordem_final <- c(ordem_causas_notif, causas_restantes)
        
        # >>>> INVERTE OS NÍVEIS PARA FICAR MAIOR NO TOPO <<<<
        df_bolhas <- df_bolhas |>
          mutate(
            causa_resumida = factor(causa_resumida, levels = rev(ordem_final)),
            nivel = factor(nivel,
                           levels = c("Óbitos com notificação de violência",
                                      "Óbitos com notificação de intoxicação exógena",
                                      "Óbitos sem notificação"))
          )
        
        cores <- c("Óbitos com notificação de violência"  = "#FF5054",
                   "Óbitos com notificação de intoxicação exógena"     = "#FFC73B",
                   "Óbitos sem notificação"  = "#121E87")
        
        g <- ggplot(df_bolhas, aes(x = nivel,
                                   y = causa_resumida,
                                   size = valor,
                                   color = nivel,
                                   text = paste0("Para ", tolower(nivel), ": ",
                                                 round(valor, 2), "%\n",
                                                 "Causa: ", causa_resumida,
                                                 "\nRegistros: ", n))) +
          geom_point(alpha = 0.9) +
          geom_text(aes(label = paste0(round(valor, 2), "%")),
                    nudge_x = 0.25,
                    hjust = 0,
                    fontface = "bold",
                    size = 3.5) +
          scale_size_continuous(range = c(1, 9.2),
                                breaks = pretty(df_bolhas$valor, n = 5)) +
          scale_color_manual(values = cores) +
          labs(x = NULL, y = NULL, size = "Proporção") +
          theme_test() +
          theme(axis.text.x = element_text(hjust = 1, vjust = 1),
                legend.position = "none")
        
        ggplotly(g, tooltip = "text") |>
          layout(hoverlabel = list(bgcolor = "#FAF4F0",
                                   font = list(color = "black")))
      })
      
      
      
      output$download_bolha <- downloadHandler(
        filename = function() {
          paste("Comparativos_causa_obito", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          # Código de geração do df_bolhas (mantém tudo que você tem)
          
          df_sim <- df_obitos  
          
          # ---- 1) Óbitos com notificação (SINAN VIOL) ----
          a <- df_sim |>
            filter(banco == "SIM", FL_SINAN_VIOL == 1) |>
            tab_1(causa_resumida) |>
            filter(causa_resumida != "Total") |>
            transmute(causa_resumida,
                      nivel = "Óbitos com notificação de violência",
                      valor = `%`,
                      n = n)
          
          # ---- 2) Óbitos – SINAN IEXO ----
          b <- df_sim |>
            filter(banco == "SIM", FL_SINAN_IEXO == 1) |>
            tab_1(causa_resumida) |>
            filter(causa_resumida != "Total") |>
            transmute(causa_resumida,
                      nivel = "Óbitos com notificação de intoxicação exógena",
                      valor = `%`,
                      n = n)
          
          # ---- 3) Óbitos sem notificação ----
          c <- df_sim |>
            filter(banco == "SIM", FL_SINAN_VIOL != 1, FL_SINAN_IEXO != 1) |>
            tab_1(causa_resumida) |>
            filter(causa_resumida != "Total") |>
            transmute(causa_resumida,
                      nivel = "Óbitos sem notificação",
                      valor = `%`,
                      n = n)
          
          # ---- Combina tudo em formato longo ----
          df_bolhas <- bind_rows(a, b, c)
          
          df_bolhas <- df_bolhas |> pivot_wider(
            names_from = nivel,
            values_from = c(valor, n),
            values_fill = list(valor = 0, n = 0)
          )
          
          # CORREÇÃO: salvar df_bolhas, não tabela_fxetaria
          writexl::write_xlsx(df_bolhas, path = file)
        }
      )
      
      
      
      # ---- Comparação SINAN Violências vs SINAN Intoxicação Exógena ----
      output$graf_viol_iexo <- renderPlotly({
        # Cria o gráfico de barras
        p <- ggplot(df, aes(x = banco,
                            y = n,
                            fill = cor,
                            text = paste0(
                              "Fonte: ", banco, 
                              "\nNúmero de mulheres: ", n
                            ))) +
          geom_bar(stat = "identity") +
          scale_fill_identity() +
          labs(x = "Banco de dados", y = "Número de registros") +
          theme_minimal() +
          theme(legend.position = "none") +
          geom_text(aes(label = n), vjust = -0.5, size = 5) # Adiciona os rótulos
        
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0", # Cor de fundo do tooltip
            font = list(color = "black")
          )
        )
      })
      
      output$download_viol_iexo <- downloadHandler(
        filename = function() {
          paste("mulheres_bancos", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          df <- df |> 
            select(-cor)
          
          # CORREÇÃO: salvar df
          writexl::write_xlsx(df, path = file)
        }
      )
      
    })
  
}