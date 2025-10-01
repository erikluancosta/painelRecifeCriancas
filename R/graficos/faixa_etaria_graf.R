faixa_etaria_graf <- function(df) {
  a <- df |> 
  tab_1(faixa_etaria_padrao) |>
    filter(faixa_etaria_padrao != "Total") |> 
    mutate(cor = ifelse(faixa_etaria_padrao == "Ignorada", "#9ba2cb", "#121E87")) |>
    ggplot(aes(
      x = faixa_etaria_padrao, y = `%`, fill = cor, 
      text = paste("Faixa etária:", faixa_etaria_padrao, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
    )
    ) + 
    geom_bar(stat = "identity")+
    scale_fill_identity() +
    labs(x = "Faixa etária", y = "Proporção") + 
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text( hjust = 1)) 
  
  ggplotly(a, tooltip = "text") |> layout(
    hoverlabel = list(
      bgcolor = "#FAF4F0",
      font = list(color = "black")
    )
  )
  
}