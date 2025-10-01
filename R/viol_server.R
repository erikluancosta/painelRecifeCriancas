# Server do módulo SINAN
sinan_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$freq_ano_graf <- renderPlotly({
        
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        
        a <- df_filtrado |> 
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil
            
          ) |> 
          ano_graf()
      })
      
      # Gráfico Faixa Etária
      output$faixa_etaria_graf <- renderPlotly({
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        a <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          faixa_etaria_graf()
        
      })
      
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-sinan-viol", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          tabela_fxetaria <- df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              les_autop %in% input$les_autop_fil,
              ano %in% input$filtro_ano
            ) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          writexl::write_xlsx(tabela_fxetaria, file)
        }
      )
      
      # Gráfico Raça/Cor
      output$raca_cor_graf <- renderPlotly({
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        dados_preparados <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            les_autop %in% input$les_autop_fil,
            ds_raca %in% input$filtro_raca,
            ano %in% input$filtro_ano
          ) |>
          raca_cor_graf()
      })
      
      output$download_tab_raca_cor <- downloadHandler(
        filename = function() {
          paste("dados-raca-cor-sinan-viol", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          tabela_raca <- df_filtrado |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              les_autop %in% input$les_autop_fil,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_raca)
          
          writexl::write_xlsx(tabela_raca, file)
        }
      )
      
      # Tabela SINAN
      output$sinan <- renderDataTable({
        rel <- vitaltable::rel |>
          mutate(ds_tp_rel = ifelse(ds_tp_rel == "Cônjugue", "Cônjuge", ds_tp_rel))
        
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        filtered_df <- get(input$evolution_filter)
        tabela_sinan <- df_filtrado |>
          filter(les_autop %in% input$les_autop_fil,
                 ano %in% input$filtro_ano,
                 ds_raca %in% input$filtro_raca,
                 faixa_etaria_padrao %in% input$filtro_idade) |>
          vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |>
          as.data.frame()
        
        if (input$evolution_filter == "enc") {
          tabela_sinan <- tabela_sinan |>
            rename(Encaminhamentos = tipo_filtered_df) |>
            mutate(Encaminhamentos = ifelse(Encaminhamentos == "tipo_filtered_df", "Nenhum encaminhamento", Encaminhamentos))
        } else if (input$evolution_filter == "proc") {
          tabela_sinan <- tabela_sinan |>
            rename(Procedimentos = tipo_filtered_df) |>
            mutate(Procedimentos = ifelse(Procedimentos == "tipo_filtered_df", "Nenhum procedimento", Procedimentos))
        } else if (input$evolution_filter == "rel") {
          tabela_sinan <- tabela_sinan |>
            rename(`Relacionamento com o agressor` = tipo_filtered_df) |>
            mutate(`Relacionamento com o agressor` = ifelse(`Relacionamento com o agressor` == "tipo_filtered_df", "Nenhum relacionamento registrado", `Relacionamento com o agressor`))
        } else if (input$evolution_filter == "viol") {
          tabela_sinan <- tabela_sinan |>
            rename(`Tipo de violência` = tipo_filtered_df) |>
            mutate(`Tipo de violência` = ifelse(`Tipo de violência` == "tipo_filtered_df", "Nenhum tipo de violência registrado", `Tipo de violência`))
        } else if (input$evolution_filter == "agc") {
          tabela_sinan <- tabela_sinan |>
            rename(`Meio de agressão` = tipo_filtered_df) |>
            mutate(`Meio de agressão` = ifelse(`Meio de agressão` == "tipo_filtered_df", "Nenhum tipo de meio de agressão registrado", `Meio de agressão`))
        } else if (input$evolution_filter == "defic") {
          tabela_sinan <- tabela_sinan |>
            rename(`Deficiência` = tipo_filtered_df) |>
            mutate(`Deficiência` = ifelse(`Deficiência` == "tipo_filtered_df", "Nenhuma deficiência registrada", `Deficiência`))
        } else if (input$evolution_filter == "transt") {
          tabela_sinan <- tabela_sinan |>
            rename(`Transtorno` = tipo_filtered_df) |>
            mutate(`Transtorno` = ifelse(`Transtorno` == "tipo_filtered_df", "Nenhum transtorno registrado", `Transtorno`))
        }
        
        datatable(tabela_sinan, options = list(
          pageLength = 20,
          rowCallback = JS(
            "function(row, data, index) {",
            "  if(data[0] === 'Total') {",
            "    $('td', row).css('font-weight', 'bold');",
            "  }",
            "}"
          )
        )) %>%
          formatStyle("Total", fontWeight = "bold")
      })
      
      output$download_tab_sinan <- downloadHandler(
        filename = function() {
          paste("dados-sinan-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          rel <- vitaltable::rel |>
            mutate(ds_tp_rel = ifelse(ds_tp_rel == "Cônjugue", "Cônjuge", ds_tp_rel))
          
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          filtered_df <- get(input$evolution_filter)
          tabela_sinan <- df_filtrado |>
            filter(les_autop %in% input$les_autop_fil,
                   ano %in% input$filtro_ano,
                   ds_raca %in% input$filtro_raca,
                   faixa_etaria_padrao %in% input$filtro_idade) |>
            vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |>
            as.data.frame()
          
          if (input$evolution_filter == "enc") {
            tabela_sinan <- tabela_sinan |>
              rename(Encaminhamentos = tipo_filtered_df) |>
              mutate(Encaminhamentos = ifelse(Encaminhamentos == "tipo_filtered_df", "Nenhum encaminhamento", Encaminhamentos))
          } else if (input$evolution_filter == "proc") {
            tabela_sinan <- tabela_sinan |>
              rename(Procedimentos = tipo_filtered_df) |>
              mutate(Procedimentos = ifelse(Procedimentos == "tipo_filtered_df", "Nenhum procedimento", Procedimentos))
          } else if (input$evolution_filter == "rel") {
            tabela_sinan <- tabela_sinan |>
              rename(`Relacionamento com o agressor` = tipo_filtered_df) |>
              mutate(`Relacionamento com o agressor` = ifelse(`Relacionamento com o agressor` == "tipo_filtered_df", "Nenhum relacionamento registrado", `Relacionamento com o agressor`))
          } else if (input$evolution_filter == "viol") {
            tabela_sinan <- tabela_sinan |>
              rename(`Tipo de violência` = tipo_filtered_df) |>
              mutate(`Tipo de violência` = ifelse(`Tipo de violência` == "tipo_filtered_df", "Nenhum tipo de violência registrado", `Tipo de violência`))
          } else if (input$evolution_filter == "agc") {
            tabela_sinan <- tabela_sinan |>
              rename(`Meio de agressão` = tipo_filtered_df) |>
              mutate(`Meio de agressão` = ifelse(`Meio de agressão` == "tipo_filtered_df", "Nenhum tipo de meio de agressão registrado", `Meio de agressão`))
          } else if (input$evolution_filter == "defic") {
            tabela_sinan <- tabela_sinan |>
              rename(`Deficiência` = tipo_filtered_df) |>
              mutate(`Deficiência` = ifelse(`Deficiência` == "tipo_filtered_df", "Nenhuma deficiência registrada", `Deficiência`))
          } else if (input$evolution_filter == "transt") {
            tabela_sinan <- tabela_sinan |>
              rename(`Transtorno` = tipo_filtered_df) |>
              mutate(`Transtorno` = ifelse(`Transtorno` == "tipo_filtered_df", "Nenhum transtorno registrado", `Transtorno`))
          }
          writexl::write_xlsx(tabela_sinan, file)
        }
      )
      
      # Gráfico: Sexo do agressor por suspeita do uso de álcool
      output$sexo_alcool_graf <- renderPlotly({
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        a <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tab_2(ds_autor_sexo, autor_alco) |>
          filter(ds_autor_sexo != "Total") |>
          select(-Total)
        
        a <- a |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),
                               names_to = "autor_alco",
                               values_to = "n")
        
        b <- df_filtrado |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tab_2(ds_autor_sexo, autor_alco, pct_row = TRUE) |>
          filter(ds_autor_sexo != "Total") |>
          select(-Total)
        
        b <- b |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),
                               names_to = "autor_alco",
                               values_to = "%")
        
        c <- merge(a, b, by = c('ds_autor_sexo', 'autor_alco')) |>
          mutate(text = paste("Sexo do agressor:", ds_autor_sexo, "\nProporção: ", round(`%`, 1), "%", "\nRegistros: ", n))
        
        c$ds_autor_sexo <- factor(c$ds_autor_sexo, levels = c("Masculino", "Feminino", "Ambos os sexos", "Ignorado"))
        d <- c |> 
          ggplot(aes(x = ds_autor_sexo, y = n, fill = autor_alco, text = text)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = c("Sim" = "#121E87", "Ignorado" = "#9ba2cb", "Não" = "#FF5054")) +
          geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour = "#FAF4F0", size = 3) +
          labs(x = "", y = "Frequência entre as categorias", fill = "Suspeita de uso de álcool") +
          theme_minimal() +
          ggtitle("")
        
        ggplotly(d, tooltip = "text") |> layout(
          hoverlabel = list(bgcolor = "#FAF4F0", font = list(color = "black"))
        )
      })
      
      
      
      ## FUNCAO DA TABELA 2
      tabela_2 <- function(df, var_row, var_col, freq_var = NULL, pct = FALSE, pct_row = FALSE, dec = 1) {
        var_row_sym <- rlang::sym(var_row)
        var_col_sym <- rlang::sym(var_col)
        
        if (!is.null(freq_var)) {
          freq_var_sym <- rlang::sym(freq_var)
        }
        
        if (is.null(freq_var)) {
          # Caso freq_var seja NULL, aplica-se a lógica de contagem simples
          df <- df %>%
            group_by(!!var_row_sym, !!var_col_sym) %>%
            summarise(
              contagem = n(),
              .groups = 'drop'
            )
        } else {
          df <- df %>%
            group_by(!!var_row_sym, !!var_col_sym) %>%
            summarise(
              contagem = as.numeric(sum(!!freq_var_sym, na.rm = TRUE)),
              .groups = 'drop'
            )
        }
        
        df <- df %>%
          pivot_wider(
            names_from = !!var_col_sym,
            values_from = contagem,
            values_fill = 0
          ) %>%
          adorn_totals("col") %>%
          arrange(-Total)
        
        if (pct) {
          df <- df %>%
            filter(
              !!var_row_sym != 'Total'
            ) %>%
            mutate(
              across(
                where(is.numeric),
                ~round((. / sum(.)) * 100, dec)
              )
            )
        }
        
        if (pct_row) {
          df <- df %>%
            filter(
              !!var_row_sym != 'Total'
            ) %>%
            mutate(
              across(
                where(is.numeric),
                ~round((. / Total) * 100, dec)
              )
            )
        }
        
        df <- df %>%
          adorn_totals("row", name = "Total")
        
        return(as.data.frame(df))
      }
      
      
      output$tabela_cruzada <- renderDataTable({
        req(input$var1, input$var2)
        
        filtro_viol <- input$filtro_violencias
        
        if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
          df_filtrado <- df_sinan |>
            filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
        } else {
          df_filtrado <- df_sinan
        }
        
        dados <- df_sinan |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            les_autop %in% input$les_autop_fil,
            ano %in% input$filtro_ano
          ) |>
          tabela_2(
            var_row = input$var1, 
            var_col = input$var2,
            freq_var = NULL
          )
        
        datatable(dados, options = list(
          pageLength = 20,
          rowCallback = JS(
            "function(row, data, index) {",
            "  if(data[0] === 'Total') {",
            "    $('td', row).css('font-weight', 'bold');",
            "  }",
            "}"
          )))
      })
      
      output$download_tab2_sinan<- downloadHandler(
        filename = function() {
          paste("dados-sinan-cruzados", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          req(input$var1, input$var2)
          
          filtro_viol <- input$filtro_violencias
          
          if (!is.null(filtro_viol) && length(filtro_viol) > 0) {
            df_filtrado <- df_sinan |>
              filter_at(vars(all_of(filtro_viol)), any_vars(. == 1))
          } else {
            df_filtrado <- df_sinan
          }
          
          dados <- df_sinan |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              les_autop %in% input$les_autop_fil,
              ano %in% input$filtro_ano
            ) |>
            tabela_2(
              var_row = input$var1, 
              var_col = input$var2,
              freq_var = NULL
            )
          
          writexl::write_xlsx(dados, file)
        }
      )
      
    }
  )
}