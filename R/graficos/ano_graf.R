ano_graf <- function(df) {
  
  a <- df |> 
    tab_1(ano) |>
    filter(ano != "Total") |>
    ggplot(aes(
      x = ano, 
      y = `n`, 
      group = 1,
      color = "#9ba2cb",
      text = paste("Ano:", ano, "\nProporção: ", `%`,"%", "\nRegistros: ", n)
    )) +
    geom_line(size = 1) +
    scale_color_identity() +
    labs(x = "Ano", y = "Frequência") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(a, tooltip = "text") |> layout(
    hoverlabel = list(
      bgcolor = "#FAF4F0",
      font = list(color = "black")
    )
  )
  
}