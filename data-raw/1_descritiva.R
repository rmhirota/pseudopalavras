library(magrittr)

devtools::load_all()

# Perfil dos participantes ------------------------------------------------

pessoas <- dados %>%
  dplyr::select(informante, idade:naturalidade) %>%
  dplyr::distinct()

# Naturalidade - não usar no modelo
dplyr::count(pessoas, naturalidade, sort = TRUE)
pessoas %>%
  dplyr::mutate(naturalidade = dplyr::case_when(
    naturalidade == "São Paulo" ~ "São Paulo, SP",
    stringr::str_detect(naturalidade, "SP") ~ "Outros municípios de SP",
    TRUE ~ "Outras UF"
  )) %>%
  dplyr::count(naturalidade, sort = TRUE)

# Área de formação e línguas
g_area <- pessoas %>%
  dplyr::mutate(linguas = ifelse(linguas == 0, "Não", "Sim")) %>%
  dplyr::count(area_formacao, linguas, sort = TRUE) %>%
  dplyr::group_by(linguas) %>%
  dplyr::mutate(pct = scales::percent(n/sum(n), .1)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = linguas, y = n, fill = area_formacao,
    label = glue::glue("{n} ({pct})")
  )) +
  ggplot2::geom_col() +
  ggplot2::geom_text(position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::scale_fill_grey(start = 0.4) +
  # ggplot2::scale_fill_viridis_d(option = "cividis", begin = 0.4) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(
    x = "Conhecimento de outra(s) língua(s)",
    y = "N",
    fill = "Área de formação"
  )
ggplot2::ggsave("inst/book/assets/graficos/g1_area_linguas.jpeg", g_area)

# Área de formação e escolaridade
pessoas %>%
  dplyr::mutate(escolaridade = dplyr::case_when(
    escolaridade == "Mestrado" ~ "4. Pós-Graduação (Completo ou Incompleto)",
    escolaridade == "Mestrado Incompleto" ~ "4. Pós-Graduação (Completo ou Incompleto)",
    escolaridade == "Pós-Graduação" ~ "4. Pós-Graduação (Completo ou Incompleto)",
    escolaridade == "Superior Completo" ~ "3. Superior Completo",
    escolaridade == "Superior Incompleto" ~ "2. Superior Incompleto",
    escolaridade == "Fundamental Completo" ~ "1. Fundamental Completo",
  )) %>%
  dplyr::count(escolaridade, area_formacao) %>%
  tidyr::pivot_wider(names_from = area_formacao, values_from = n) %>%
  tidyr::replace_na(list(Letras = 0)) %>%
  dplyr::mutate(Total = Outro + Letras) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(dplyr::across(
    Outro:Total, ~paste0(.x, " (", scales::percent(.x/34, .1), ")")))

# Idade e gênero
g_idade_genero <- pessoas %>%
  dplyr::mutate(idade_cat = dplyr::case_when(
    idade <= 20 ~ "Entre 18 e 20 anos",
    idade <= 30 ~ "Entre 21 e 30 anos",
    idade <= 38 ~ "Entre 31 e 38 anos",
    TRUE ~ "Entre 38 e 60 anos",
  )) %>%
  dplyr::count(genero, idade_cat) %>%
  ggplot2::ggplot(ggplot2::aes(y = n, x = idade_cat, fill = genero, label = n)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::scale_fill_grey(start = 0.4) +
  # ggplot2::scale_fill_viridis_d(option = "cividis", begin = 0.4) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(
    x = "Idade",
    y = "N",
    fill = "Gênero"
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


# Pseudopalavra

dados %>% dplyr::group_by(pseudopalavra) %>%  dplyr::count()%>% View()
dados %>% dplyr::distinct(palavra_alvo) %>% dplyr::count()





