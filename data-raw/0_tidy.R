library(magrittr)

dados <- readr::read_csv("data-raw/Resultados_Totais_Experimento_1_Final.csv")
# dplyr::glimpse(dados)
dados <- janitor::clean_names(dados)

# Arruma tipos de variáveis -----------------------------------------------

dados <- dados %>%
  dplyr::mutate(
    grupo = as.factor(grupo),
    bloco_apresentacao = as.factor(bloco_apresentacao),
    ordem_apresentacao = as.factor(ordem_apresentacao),
    codigo_pseudo = as.factor(codigo_pseudo),
    tempo_resposta = as.double(tempo_resposta)
  )

usethis::use_data(dados, overwrite = TRUE)
