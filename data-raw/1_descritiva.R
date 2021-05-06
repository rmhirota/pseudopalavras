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
  dplyr::count(aleatorizacao)  # alto número de não aleatorizados
dados %>%
  dplyr::count(grupo) #desbalanceados
dados %>%
  dplyr::count(bloco_apresentacao) #desbalanceado
dados %>%
  dplyr::count(ordem_apresentacao) #93 tipos de ordem
dados %>%
  dplyr::count(informante)
dados %>%
  dplyr::count(validacao) #n = 2118, q = 1348 e s = 9045

dados %>%
  dplyr::count(pseudopalavra, sort = TRUE) %>%
  View()

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
  skimr::skim() %>%
  View()

# ggpairs
pseudopalavras::dados %>%
  dplyr::select(
    -vars_id, - vars_excluir,
    -c(
      pseudopalavra, vizinhanca_fonologica, vizinhanca_tonicidade,
      palavra_alvo, area_formacao, naturalidade
    )) %>%
  GGally::ggpairs()

# Tonicidade --------------------------------------------------------------

pseudopalavras::dados %>%
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
pseudopalavras::dados %>%
  dplyr::count(tonicidade_producao, tonicidade_alvo) %>%
  knitr::kable()

# Vizinhança Fonológica e Vizinhança Tonicidade (Validação)

dados %>% dplyr::filter(validacao=="n") %>%
  dplyr::group_by(vizinhanca_fonologica) %>%
  dplyr::count() %>% View() # 1 NA e 60 categorias

dados %>% dplyr::filter(validacao=="n") %>%
  dplyr::group_by(vizinhanca_tonicidade) %>%
  dplyr::count() %>% View() # 1 NA e 17 categorias

# Grupo vs tonicidade_producao

prop.table(table(dados$tonicidade_producao,dados$grupo),2)*100

# Estrutura da palavra vs tonicidade_producao

prop.table(table(dados$tonicidade_producao,dados$estrutura_palavra),2)*100

# Informantes

dados %>% dplyr::group_by(informante) %>% dplyr::count() %>% View()

# Pseudopalavra

dados %>% dplyr::group_by(pseudopalavra) %>%  dplyr::count()%>% View()
dados %>% dplyr::distinct(palavra_alvo) %>% dplyr::count()

