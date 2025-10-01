# Módulo server do mapa
mapa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Variável para controlar o estado do heatmap
    heatmap_active <- reactiveVal(FALSE)
    
    # Variável para armazenar o CNES (unidade selecionada)
    selected_cnes <- reactiveVal(NULL)
    
    # Variável para monitorar o tempo do último clique (para detectar double-click)
    last_click_time <- reactiveVal(Sys.time())
    
    # Toggles para mostrar/ocultar filtros e painel do mapa
    observeEvent(input$toggle_filtros, {
      shinyjs::toggle("filtros")
    })
    
    observeEvent(input$toggle_mapa, {
      shinyjs::toggle("mapa_panel")
    })
    
    observeEvent(input$toggle_heatmap, {
      heatmap_active(!heatmap_active())
      updateActionButton(session, "toggle_heatmap",
                         label = ifelse(heatmap_active(), "Desativar Heatmap", "Ativar Heatmap"))
      update_map()
    })
    
    observeEvent(input$mapa_marker_click, {
      click <- input$mapa_marker_click
      if (!is.null(click$id) && click$id != "") {
        selected_cnes(click$id)
      }
    })
    
    observeEvent(input$mapa_click, {
      current_time <- Sys.time()
      time_diff <- as.numeric(difftime(current_time, last_click_time(), units = "secs"))
      # Se for double-click (< 0.5s) e houver um CNES selecionado, desmarcar
      if (time_diff < 0.5 && isTruthy(selected_cnes())) {
        selected_cnes(NULL)
      }
      last_click_time(current_time)
    })
    
    # Reactive para os dados filtrados – usamos req() para garantir que os inputs essenciais estão definidos
    filtered_pontos <- reactive({
      req(input$filtro_raca, input$filtro_idade)
      data <- pontos_viol
      
      if (input$filtro_raca != "Todas") {
        data <- data %>% filter(ds_raca == input$filtro_raca)
      }
      
      # Garante que input$filtro_idade tenha os dois valores
      if (length(input$filtro_idade) < 2) {
        return(data)
      }
      
      data <- data %>% filter(
        nu_idade_anos >= input$filtro_idade[1],
        nu_idade_anos <= input$filtro_idade[2]
      )
      
      data$highlight <- FALSE
      if (!is.null(input$filtro_id_pessoa) &&
          !is.na(input$filtro_id_pessoa) &&
          input$filtro_id_pessoa != "") {
        selected_id <- as.numeric(input$filtro_id_pessoa)
        data$highlight[data$id_pessoa == selected_id] <- TRUE
      }
      
      if (isTruthy(selected_cnes())) {
        data <- data %>% filter(cd_cnes_unid_not == as.numeric(selected_cnes()))
      }
      
      data
    })
    
    # Ícone para as unidades de saúde (CNES)
    hospitalIcon <- makeIcon(
      iconUrl = "www/img/hospital.png",
      iconWidth = 30,
      iconHeight = 30
    )
    
    # Função reativa para atualizar o mapa
    update_map <- reactive({
      req(input$map_style, input$fill_opacity, input$line_opacity, input$polygon_color)
      
      map <- leaflet() %>% setView(lng = -34.946671, lat = -8.039802, zoom = 12)
      
      if (input$map_style == "Carto Positron") {
        map <- map %>% addProviderTiles("CartoDB.Positron")
      } else if (input$map_style == "Stamen Toner") {
        map <- map %>% addProviderTiles("Stamen.Toner")
      } else if (input$map_style == "Stamen Watercolor") {
        map <- map %>% addProviderTiles("Stamen.Watercolor")
      } else {
        map <- map %>% addTiles()
      }
      
      map <- map %>% addPolygons(
        data = mapa,
        color = input$polygon_color, 
        weight = 1, 
        opacity = input$line_opacity,
        fillOpacity = input$fill_opacity
      )
      
      # Caso um CNES esteja selecionado, filtra os dados de CNES
      cnes_data <- cnes
      if (isTruthy(selected_cnes())) {
        cnes_data <- cnes_data %>% filter(cd_cnes_unid_not == as.numeric(selected_cnes()))
      }
      
      map <- map %>% addMarkers(
        data = cnes_data,
        lng = ~longitude_cnes,
        lat = ~latitude_cnes,
        icon = hospitalIcon,
        popup = ~paste("<b>Unidade de saúde:</b>", NO_FANTASIA),
        group = "cnes",
        layerId = ~cd_cnes_unid_not
      )
      
      if (heatmap_active()) {
        if (nrow(filtered_pontos()) > 0) {
          map <- map %>% addWebGLHeatmap(
            data = filtered_pontos(),
            lng = ~Longitude, 
            lat = ~Latitude,
            size = 60000,
            group = "heatmap"
          )
        }
      } else {
        if (nrow(filtered_pontos()) > 0) {
          map <- map %>% addCircleMarkers(
            data = filtered_pontos(),
            lng = ~Longitude, 
            lat = ~Latitude,
            color = ~ifelse(highlight, input$highlight_color, input$default_color),
            radius = ~ifelse(highlight, 5, 1),
            fillOpacity = 0.7,
            popup = ~paste(
              "<b>DADOS DEMOGRÁFICOS</b> <br>",
              "<b>Idade:</b>", nu_idade_anos, "<br>",
              "<b>Raça/cor:</b>", ds_raca_padronizada, "<br>",
              "<br><br>", texto_final
            ),
            group = "markers"
          )
        }
      }
      
      map
    })
    
    output$mapa <- renderLeaflet({
      update_map()
    })
    
    # Atualiza o mapa via leafletProxy sempre que os inputs mudam
    observe({
      leafletProxy("mapa") %>% 
        clearGroup("markers") %>% 
        clearGroup("heatmap") %>% 
        clearGroup("cnes")
      
      cnes_data <- cnes
      if (isTruthy(selected_cnes())) {
        cnes_data <- cnes_data %>% filter(cd_cnes_unid_not == as.numeric(selected_cnes()))
      }
      
      leafletProxy("mapa") %>%
        addMarkers(
          data = cnes_data,
          lng = ~longitude_cnes,
          lat = ~latitude_cnes,
          icon = hospitalIcon,
          popup = ~paste("<b>Unidade de saúde:</b>", NO_FANTASIA),
          group = "cnes",
          layerId = ~cd_cnes_unid_not
        )
      
      if (heatmap_active()) {
        if (nrow(filtered_pontos()) > 0) {
          leafletProxy("mapa") %>%
            addHeatmap(
              data = filtered_pontos(),
              lng = ~Longitude, 
              lat = ~Latitude,
              intensity = ~1,
              blur = 20,
              max = 0.05,
              radius = 15,
              gradient = colorRampPalette(c("blue", "red"))(5),
              group = "heatmap"
            )
        }
      } else {
        if (nrow(filtered_pontos()) > 0) {
          leafletProxy("mapa") %>%
            addCircleMarkers(
              data = filtered_pontos(),
              lng = ~Longitude, 
              lat = ~Latitude,
              color = ~ifelse(highlight, input$highlight_color, input$default_color),
              radius = ~ifelse(highlight, 5, 1),
              fillOpacity = 0.7,
              popup = ~paste(
                "<b>DADOS DEMOGRÁFICOS</b> <br>",
                "<b>Idade:</b>", nu_idade_anos, "<br>",
                "<b>Raça/cor:</b>", ds_raca_padronizada, "<br>",
                "<br><br>", texto_final
              ),
              group = "markers"
            )
        }
      }
    })
    
  })
}