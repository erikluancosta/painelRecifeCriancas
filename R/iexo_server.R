iexo_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$freq_ano_graf <- renderPlotly({
        a <- df_iexo |> 
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns
          ) |> 
          ano_graf()
      })
      
      # Gráfico de Faixa Etária
      output$faixa_etaria_graf <- renderPlotly({
        a <-  df_iexo |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |>
          faixa_etaria_graf()
      })
      
      # Faixa etária download
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-sinan-iexo", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          tabela_fxetaria <-  df_iexo |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          writexl::write_xlsx(tabela_fxetaria, file)
          
        }
      )
      
      # Gráfico de Raça/Cor
      output$raca_cor_graf <- renderPlotly({
        dados_preparados <- df_iexo  |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |>
          raca_cor_graf()
      })
      
      # Raça/cor download
      output$download_tab_raca_cor <- downloadHandler(
        filename = function() {
          paste("dados-raca-cor-sinan-iexo", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          tabela_raca <-  df_iexo |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_raca) 
          writexl::write_xlsx(tabela_raca, file)
          
          
        })
      
      # Gráfico de Circunstâncias
      output$circunstancia_graf <- renderPlotly({
        data_filtered <- df_iexo |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |>
          tab_1(ds_circunstan) |>
          filter(ds_circunstan != "Total")
        
        data_filtered$ds_circunstan <- with(data_filtered, reorder(ds_circunstan, n))
        
        p <- ggplot(data_filtered, aes(
          x = ds_circunstan, y = `%`, fill = "#121E87",
          text = paste("Circunstância:", ds_circunstan,  "\nProporção: ", `%`,"%", "\nRegistros: ", n)
        )) +
          geom_bar(stat = "identity") +
          labs(x = "Circunstância", y = "Proporção") +
          theme_minimal() +
          theme(legend.position = "none") +
          coord_flip()
        
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # tabela circunstancia
      output$download_tab_circunstancia <- downloadHandler(
        filename = function() {
          paste("dados-circunstancia-sinan-iexo", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          tabela_raca <-  df_iexo |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_circunstan) 
          writexl::write_xlsx(tabela_raca, file)
          
        })
      
      
      output$ag_intox_graf <- renderPlotly({
        data_filtered <- df_iexo |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |>
          tab_1(ds_agente_tox) |>
          filter(ds_agente_tox != "Total")
        
        data_filtered$ds_agente_tox <- with(data_filtered, reorder(ds_agente_tox, n))
        
        p <- ggplot(data_filtered, aes(
          x = ds_agente_tox, y = `%`, fill = "#121E87",
          text = paste("Agente intoxicante:", ds_agente_tox,  "\nProporção: ", `%`,"%", "\nRegistros: ", n)
        )) +
          geom_bar(stat = "identity") +
          labs(x = "Agente intoxicante", y = "Proporção") +
          theme_minimal() +
          theme(legend.position = "none") +
          coord_flip()
        
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
      # Download handler for Agente Intoxicante
      output$download_tab_ag_intox <- downloadHandler(
        filename = function() {
          paste("dados-agente-intoxicante-sinan-iexo", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          tabela_ag_intox <- df_iexo |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_agente_tox) 
          
          writexl::write_xlsx(tabela_ag_intox, file)
          
        }
      )
      
      
      output$atend_hospit_graf <- renderPlotly({
        
        # Se não houver seleção, retorna o dataframe completo
        filtro_viol <- input$filtro_violencias
        
        
        a <- df_iexo |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |> 
          tab_2(ds_tpatend, ds_hospital) |> 
          filter(ds_tpatend != "Total") |> 
          select(-Total)
        
        a <- a |> tidyr::pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                                      names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                                      values_to = "n")
        
        b <- df_iexo |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ds_circunstan %in% input$filtro_circuns,
            ano %in% input$filtro_ano
          ) |> 
          tab_2(ds_tpatend, ds_hospital, pct_row=T) |> 
          filter(ds_tpatend != "Total") |>
          select(-Total)
        
        b <- b |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                               names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                               values_to = "%")
        
        
        c <- merge(a,b, by=c('ds_tpatend', 'ds_hospital')) |> mutate(text = paste("Tipo de atendimento:", ds_tpatend, "\nProporção: ", round(`%`, 1), "%", "\nRegistros: ", n))
        
        c$ds_tpatend <- factor(c$ds_tpatend, levels = c("Ambulatorial", "Domiciliar", "Hospitalar", "Ignorado", "Nenhum"))
        d <- c |> 
          ggplot(aes(x = ds_tpatend, y = n, fill = ds_hospital, text = text)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = c("Sim" = "#121E87", 
                                       "Ignorado" = "#9ba2cb", 
                                       "Não" = "#FF5054")) +
          geom_text(aes(
            label = n), 
            position = position_stack(vjust = 0.5),
            colour ="#FAF4F0",
            size = 3
          ) +
          labs(x = "", y = "Frequência entre as categorias", fill = "Hospitalização") +
          theme_minimal() +
          ggtitle("")
        
        # Convertendo o ggplot para plotly e ajustando os tooltips
        ggplotly(d, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
        
      })
      
      
      # tabela hospitalizacoes
      output$download_atend_hospit_graf <- downloadHandler(
        filename = function() {
          paste("dados-atendimento_hospitalizacao-sinan-iexo", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          a <- df_iexo |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |> 
            tab_2(ds_tpatend, ds_hospital) |> 
            filter(ds_tpatend != "Total") |> 
            select(-Total)
          
          a <- a |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                                 names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                                 values_to = "n")
          
          b <- df_iexo |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ds_circunstan %in% input$filtro_circuns,
              ano %in% input$filtro_ano
            ) |> 
            tab_2(ds_tpatend, ds_hospital, pct_row=T) |> 
            filter(ds_tpatend != "Total") |>
            select(-Total)
          
          b <- b |> pivot_longer(cols = c("Ignorado", "Não", "Sim"),  # Colunas que você quer transformar
                                 names_to = "ds_hospital",             # Nova coluna para os nomes das colunas anteriores
                                 values_to = "%")
          
          
          c <- merge(a,b, by=c('ds_tpatend', 'ds_hospital'))
          
          writexl::write_xlsx(c, file)
        })
      
    }
  )
}