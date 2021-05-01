library(magrittr)

dados <- readr::read_csv("data-raw/Resultados_Totais_Experimento_1_Final.csv")
dplyr::glimpse(dados)
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

# Aleatorização e vars de delineamento da pesquisa ------------------------

dados %>%
  dplyr::count(aleatorizacao)
dados %>%
  dplyr::count(grupo)
dados %>%
  dplyr::count(bloco_apresentacao)
dados %>%
  dplyr::count(ordem_apresentacao)
dados %>%
  dplyr::count(informante)
dados %>%
  dplyr::count(validacao)


# Análise geral -----------------------------------------------------------

vars_id <- c(
  "id", "grupo", "aleatorizacao", "bloco_apresentacao",
  "ordem_apresentacao", "codigo_pseudo", "informante",
  "validacao"
)
vars_excluir <- c("tempo_resposta")

# skim
dados %>%
  dplyr::select(-vars_id, -vars_excluir) %>%
  skimr::skim()

# ggpairs
dados %>%
  dplyr::select(
    -vars_id, - vars_excluir,
    -c(
      pseudopalavra, vizinhanca_fonologica, vizinhanca_tonicidade,
      palavra_alvo, area_formacao, naturalidade
    )) %>%
  GGally::ggpairs()

# Tonicidade --------------------------------------------------------------

dados %>%
  dplyr::count(tonicidade_producao) %>%
  dplyr::mutate(pct = scales::percent(n/sum(n))) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = tonicidade_producao, y = n,
    label = glue::glue("{n} ({pct})")
  )) +
  ggplot2::geom_col() +
  ggplot2::geom_text(nudge_y = 300) +
  ggplot2::labs(
    x = "Tonicidade observada da pseudopalavra",
    y = "N"
  ) +
  ggplot2::theme_minimal(16)

# Tonicidade observada na pseudopalavra e tonicidade da palavra-alvo
dados %>%
  dplyr::count(tonicidade_producao, tonicidade_alvo)
