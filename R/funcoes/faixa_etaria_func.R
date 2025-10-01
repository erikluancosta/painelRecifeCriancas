faixa_etaria_func <- function(df){
  
  df <- df |> 
    dplyr::mutate(
      nu_idade_anos = as.numeric(nu_idade_anos),
      faixa_etaria_padrao = dplyr::case_when(
        nu_idade_anos < 1 ~ "<1",
        nu_idade_anos >= 1 & nu_idade_anos <= 4 ~ "01-04",
        nu_idade_anos >= 5 & nu_idade_anos <= 9 ~ "05-09", 
        nu_idade_anos >= 10 & nu_idade_anos <= 14 ~ "10-14", 
        nu_idade_anos >= 15 & nu_idade_anos <= 19 ~ "15-19", 
        nu_idade_anos >= 20 & nu_idade_anos <= 29 ~ "20-29", 
        nu_idade_anos >= 30 & nu_idade_anos <= 39 ~ "30-39", 
        nu_idade_anos >= 40 & nu_idade_anos <= 49 ~ "40-49", 
        nu_idade_anos >= 50 & nu_idade_anos <= 59 ~ "50-59", 
        nu_idade_anos >= 60 & nu_idade_anos <= 69 ~ "60-69", 
        nu_idade_anos >= 70 & nu_idade_anos <= 79 ~ "70-79", 
        nu_idade_anos >= 80 ~ "80+", 
        TRUE ~ "Ignorada"
      )
    )
}
