raca_cor_graf <- function(df){
  
  dados_preparados <- df |> 
  tab_1(ds_raca) |>
    filter(ds_raca != "Total")
  
  racas_ordenadas <- unique(dados_preparados$ds_raca)
  racas_ordenadas <- racas_ordenadas[racas_ordenadas != "Ignorada"]
  racas_ordenadas <- c(racas_ordenadas, "Ignorada") 
  racas_ordenadas <- rev(racas_ordenadas)
  
  dados_preparados$ds_raca <- factor(dados_preparados$ds_raca, levels = racas_ordenadas)
  
  cores <- setNames(rep("#121E87", length(racas_ordenadas)), racas_ordenadas)
  cores["Ignorada"] <- "#9ba2cb"
  
  b <- ggplot(dados_preparados, aes(
    x = ds_raca, y = `%`, fill = ds_raca, 
    text = paste("Raça/Cor:", ds_raca, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
  )
  )  +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = cores) +
    labs(
      x = "Raça/cor",
      y = "Proporção"
    ) +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") 
  ggplotly(b, tooltip = "text") |> layout(
    hoverlabel = list(
      bgcolor = "#FAF4F0",
      font = list(color = "black")
    )
  )
  
}