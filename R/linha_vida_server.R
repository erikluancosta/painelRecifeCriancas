linhavida_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      selected_point <- reactiveVal(NULL)
      
      # ── Exclusividade “Exclusivo SINAN Violências” ─────────
      prev_sel <- reactiveVal(character())
      observeEvent(input$filtro_banco, ignoreNULL = FALSE, {
        sel  <- input$filtro_banco
        prev <- prev_sel()
        
        if ("EXCLUSIVO_SINAN_VIOL" %in% sel && length(sel) > 1) {
          if ("EXCLUSIVO_SINAN_VIOL" %in% prev) {
            updatePickerInput(session, "filtro_banco",
                              selected = setdiff(sel, "EXCLUSIVO_SINAN_VIOL"))
          } else {
            updatePickerInput(session, "filtro_banco", selected = "EXCLUSIVO_SINAN_VIOL")
          }
        }
        prev_sel(sel)
      })
      
      # ── Função de filtragem de bancos ─────────────────────
      filtrar_bancos <- function(df, bancos_sel) {
        if ("EXCLUSIVO_SINAN_VIOL" %in% bancos_sel) return(filter(df, so_sinan == 1))
        if (length(bancos_sel) > 0) {
          cond <- list()
          if ("SINAN_VIOL" %in% bancos_sel)  cond <- append(cond, list(df$FL_SINAN_VIOL == 1))
          if ("SIM"        %in% bancos_sel)  cond <- append(cond, list(df$FL_SIM        == 1))
          if ("SIH"        %in% bancos_sel)  cond <- append(cond, list(df$FL_SIH        == 1))
          if ("SINAN_IEXO" %in% bancos_sel)  cond <- append(cond, list(df$FL_SINAN_IEXO == 1))
          if ("Boletins de Ocorrência Letais" %in% bancos_sel)
            cond <- append(cond, list(df$FL_SESAP_OB == 1))
          df <- filter(df, Reduce(`|`, cond))
        }
        df
      }
      
      # ── Dados reativos (aplicação de filtros) ─────────────
      df_filtrado <- reactive({
        df_tmp <- df_linha_vida
        
        if (!is.null(input$filtro_violencias2) && length(input$filtro_violencias2) > 0)
          df_tmp <- filter_at(df_tmp, vars(all_of(input$filtro_violencias2)), any_vars(. == 1))
        
        df_tmp <- filter(df_tmp,
                         ds_raca_padronizada %in% input$filtro_raca,
                         faixa_etaria_padrao %in% input$filtro_idade,
                         FL_SIM              %in% as.numeric(input$filtro_obitos))
        
        if (!is.null(input$filtro_autoprovocada) && length(input$filtro_autoprovocada) > 0)
          df_tmp <- filter(df_tmp, fl_les_autop %in% as.numeric(input$filtro_autoprovocada))
        
        df_tmp
      })
      
      # ── Escalas de cor/forma ──────────────────────────────
      colors_banco <- c("SINAN_VIOL" = "#FF5054",
                        "SIM"        = "#121E87",
                        "SIH"        = "#FFC73B",
                        "SINAN_IEXO" = "#0099D6",
                        "Boletins de Ocorrência Letais" = "#3bd80d")
      
      shapes_banco <- c("SINAN_VIOL" = 4,
                        "SIM"        = 15,
                        "SIH"        = 17,
                        "SINAN_IEXO" = 18,
                        "Boletins de Ocorrência Letais" = 19)
      
      # ── Gráfico principal ─────────────────────────────────
      output$linha_vida_geral_ <- renderPlotly({
        req(input$filtro_banco)
        
        df_aux <- df_filtrado() |>
          filter(!is.na(id_pareamento)) |>
          filtrar_bancos(input$filtro_banco) |>
          arrange(id_pareamento, dt_comum) |>
          mutate(par_reduzido_1 = as.numeric(factor(id_pareamento)) * 20)
        
        sel_pt <- selected_point()
        
        g <- ggplot(df_aux, aes(x = dt_comum, y = par_reduzido_1, text = id_pareamento))
        
        if (is.null(sel_pt)) {
          g <- g +
            geom_line(aes(group = par_reduzido_1),
                      color = "lightgray", size = 0.15) +
            geom_point(aes(color = banco, shape = banco), size = 0.9)
        } else {
          g <- g +
            geom_line(aes(group = par_reduzido_1,
                          color = ifelse(par_reduzido_1 == sel_pt$y,
                                         "black", "lightgray")),
                      size = 0.15) +
            geom_point(aes(color = banco, shape = banco,
                           alpha = ifelse(par_reduzido_1 == sel_pt$y, 1, 0.65)),
                       size = 0.9)
        }
        
        # ── Escala de cores única (sem avisos) ──────────────
        valores_cor <- colors_banco
        if (!is.null(sel_pt))
          valores_cor <- c(valores_cor, "black" = "black", "lightgray" = "lightgray")
        
        g <- g +
          scale_color_manual(values = valores_cor) +
          scale_shape_manual(values = shapes_banco) +
          labs(x = "Ano do evento", y = "") +
          theme_minimal() +
          theme(panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.y        = element_blank(),
                axis.ticks.y       = element_blank()) +
          scale_x_date(date_labels = "%Y")
        
        # ── Conversão para plotly ───────────────────────────
        p <- ggplotly(g, tooltip = "text", source = "A") |>
          layout(showlegend = FALSE,
                 hoverlabel = list(bgcolor = "#FAF4F0",
                                   font = list(color = "black")))
        
        # Registro explícito dos eventos
        p <- event_register(p, "plotly_click")
        p <- event_register(p, "plotly_doubleclick")
        p
      })
      
      # ── Caixa de detalhes ─────────────────────────────────
      output$selected_point_info <- renderUI({
        pt <- selected_point()
        
        if (is.null(pt)) {
          box(
            title       = strong("Detalhes do Evento Selecionado"),
            width       = 12,
            status      = "danger",
            solidHeader = TRUE,
            style       = "height: 520px; overflow-y: auto;",
            h5("Ao lado, cada linha representa a história ...")
          )
        } else {
          df_sel <- df_filtrado() |>
            filter(!is.na(id_pareamento)) |>
            filtrar_bancos(input$filtro_banco) |>
            arrange(id_pareamento, dt_comum) |>
            mutate(par_reduzido_1 = as.numeric(factor(id_pareamento)) * 20) |>
            filter(par_reduzido_1 == pt$y)
          
          box(
            title       = strong("Detalhes do Evento Selecionado"),
            width       = 12,
            status      = "danger",
            solidHeader = TRUE,
            style       = "height: 520px; overflow-y: auto;",
            p(HTML(paste0("<b>Demográfico:</b><br><b>Raça/cor:</b> ",
                          df_sel$ds_raca_padronizada[1],
                          "<br><b>Idade:</b> ", df_sel$nu_idade_anos[1],
                          "<br>", df_sel$texto_final[1]))),
            div(style = "text-align: right;",
                actionButton(ns("ok_button"), "Finalizar leitura"))
          )
        }
      })
      
      # ── Eventos de seleção (sem avisos plotly) ────────────
      observeEvent(
        suppressWarnings(event_data("plotly_click", source = "A")),
        {
          sel <- event_data("plotly_click", source = "A")
          if (!is.null(sel)) selected_point(sel)
        }
      )
      
      observeEvent(
        suppressWarnings(event_data("plotly_doubleclick", source = "A")),
        selected_point(NULL)
      )
      
      observeEvent(input$ok_button, selected_point(NULL))
    }
  )
}