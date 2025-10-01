sim_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$freq_ano_graf <- renderPlotly({
        
        a <- df_sim |> 
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca
          ) |> 
          ano_graf()
      })
      
      # Gráfico Faixa etária
      output$faixa_etaria_graf <- renderPlotly({
        
        a <-  df_sim |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ano %in% input$filtro_ano
          ) |> 
          faixa_etaria_graf()
      })
      
      # Download da tabela Faixa etária
      output$download_tab_faixa_etaria <- downloadHandler(
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          tabela_fxetaria <-  df_sim |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(faixa_etaria_padrao) |>
            filter(faixa_etaria_padrao != "Total") |>
            arrange(faixa_etaria_padrao)
          write_xlsx(tabela_fxetaria, file)
        }
      )
      
      # Gráfico Raça/cor
      output$raca_cor_graf <- renderPlotly({
        dados_preparados <- df_sim |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            ano %in% input$filtro_ano
          ) |>
          raca_cor_graf()
        
      })
      
      # Raça/cor download
      output$download_tab_raca_cor <- downloadHandler(
        filename = function() {
          paste("dados-raca-cor-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          tabela_raca <-  df_sim |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              ano %in% input$filtro_ano
            ) |>
            tab_1(ds_raca) 
          write_xlsx(tabela_raca, file)
        })
      
      
      
      
      # Gráfico de Óbitos
      output$graf_obito <- renderPlotly({
        ## Colocando a descricao dos SIH
        cid_10 <- read.csv2('dados/cid_10.csv')
        
        cid_10 <- cid_10 |> unique()
        # cid_10 <- openxlsx::read.xlsx('dados/cid10_tratado.xlsx')
        
        df_sim_mod <- df_sim |> 
          filter(
            ano %in% input$filtro_ano
          ) |> 
          left_join(cid_10, by = c("cd_causabas" = "SUBCAT")) |> 
          filter(
            CAPITULO %in% c(
              "Capítulo XIX - Lesões, envenenamento e algumas outras conseqüências de causas externas",
              "Capítulo XX - Causas externas de morbidade e de mortalidade"
            )
          ) |> 
          mutate(
            CAPITULO = ifelse(is.na(CAPITULO), "Sem registro", CAPITULO)
          ) |> 
          vitaltable::tab_1(DESCRICAO_CAT) |> 
          filter(DESCRICAO_CAT != "Total") 
        
        # Ajusta os níveis do fator e quebra o texto das categorias
        df_sim_mod$DESCRICAO_CAT <- stringr::str_wrap(df_sim_mod$DESCRICAO_CAT, width = 60)
        df_sim_mod <- df_sim_mod |> mutate(DESCRICAO_CAT = forcats::fct_reorder(DESCRICAO_CAT, n))
        
        gr_ob <- df_sim_mod |> 
          ggplot(aes(
            x = `%`, y = DESCRICAO_CAT,
            text = paste("Capítulo da CID-10:", DESCRICAO_CAT, "\nProporção:", `%`, "%", "\nRegistros:", n)
          )) +
          geom_bar(stat = "identity", width = 0.8, fill = "#121E87") +
          scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
          theme_minimal() +
          theme(
            axis.text.y = element_text(size = 10, lineheight = 1.5),  # Aumenta o lineheight
            axis.text.x = element_text(size = 11),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(size = 8)
          ) +
          labs(
            x = NULL,
            y = "Proporção"
          )
        
        # Ajusta a altura do gráfico de acordo com o número de categorias
        plot_height <- nrow(df_sim_mod) * 50  # Ajuste o multiplicador conforme necessário
        
        ggplotly(gr_ob, tooltip = "text", height = plot_height) |> layout(
          hoverlabel = list(
            bgcolor = "#FAF4F0",
            font = list(color = "black")
          )
        )
      })
      
    }
  )
}