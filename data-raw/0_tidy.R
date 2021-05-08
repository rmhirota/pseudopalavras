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

# Agrupa variáveis --------------------------------------------------------

dados <- dados %>%
  dplyr::mutate(
    musica = dplyr::case_when(
      musica == "nenhum" ~ 0,
      TRUE ~ 1
    ),
    linguas = dplyr::case_when(
      linguas == "nenhuma" ~ 0,
      TRUE ~ 1
    ),
    area_formacao = dplyr::case_when(
      stringr::str_detect(area_formacao, "Letras") ~ "Letras",
      TRUE ~ "Outro"
    )
  )


usethis::use_data(dados, overwrite = TRUE)
