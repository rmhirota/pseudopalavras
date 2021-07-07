devtools::load_all()
library(ggalluvial)

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
  dplyr::mutate(pct = scales::percent(n / sum(n), .1)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = linguas, y = n, fill = area_formacao,
    label = glue::glue("{n} ({pct})")
  )) +
  ggplot2::geom_col() +
  ggplot2::geom_text(position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::scale_fill_manual(values = c("lemonchiffon2", "plum3")) +
  # ggplot2::scale_fill_viridis_d(option = "cividis", begin = 0.4) +
  ggplot2::theme_minimal(12) +
  ggplot2::labs(
    x = "Conhecimento de outra(s) língua(s)",
    y = "N",
    fill = "Área de formação"
  ) +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"))

ggplot2::ggsave("inst/book/assets/graficos/g1_area_linguas.jpeg", g_area, width = 14, height = 7)

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
    Outro:Total, ~ paste0(.x, " (", scales::percent(.x / 34, .1), ")")
  ))

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

f_count <- function(variavel) {
  dados %>%
    dplyr::count({{ variavel }})
}

rlang::exprs(
  aleatorizacao, grupo, bloco_apresentacao,
  ordem_apresentacao, informante, validacao
) %>%
  purrr::map(f_count)

# alto número de não aleatorizados
# grupos desbalanceados
# blocos desbalanceados
# 93 tipos de ordem
# validacao: n = 2118, q = 1348 e s = 9045

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
dados %>%
  dplyr::select(
    -vars_id, -vars_excluir,
    -c(
      pseudopalavra, vizinhanca_fonologica, vizinhanca_tonicidade,
      palavra_alvo, area_formacao, naturalidade
    )
  ) %>%
  GGally::ggpairs()

# Tonicidade --------------------------------------------------------------

dados %>%
  dplyr::count(tonicidade_producao) %>%
  dplyr::mutate(pct = scales::percent(n / sum(n))) %>%
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
  dplyr::count(estrutura_palavra, tonicidade_alvo)
# knitr::kable()

# Vizinhança Fonológica e Vizinhança Tonicidade (Validação)
dados %>%
  dplyr::filter(validacao == "n") %>%
  dplyr::group_by(vizinhanca_fonologica) %>%
  dplyr::count() %>%
  View() # 1 NA e 60 categorias

dados %>%
  dplyr::filter(validacao == "n") %>%
  dplyr::group_by(vizinhanca_tonicidade) %>%
  dplyr::count() %>%
  View() # 1 NA e 17 categorias

# Grupo vs tonicidade_producao
prop.table(table(dados$tonicidade_producao, dados$grupo), 2) * 100

# Estrutura da palavra vs tonicidade_producao
prop.table(table(dados$tonicidade_producao, dados$estrutura_palavra), 2) * 100

# Pseudopalavra
dados %>%
  dplyr::group_by(pseudopalavra) %>%
  dplyr::count() %>%
  View()
dados %>%
  dplyr::distinct(palavra_alvo) %>%
  dplyr::count()

g_validacao <- pseudopalavras::dados %>%
  dplyr::count(grupo, validacao) %>%
  # dplyr::filter(grupo == 1) %>%
  dplyr::mutate(validacao = dplyr::case_when(
    validacao == "n" ~ "Não validada",
    validacao == "q" ~ "Quase validada",
    validacao == "s" ~ "Validada"
  )) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = grupo, y = n, fill = validacao
  )) +
  # scale_fill_manual(name="Status",values=c("orchid3","lemonchiffon2","plum3"))+
  labs(x = "Grupo de Classificação", y = "N") +
  ggplot2::geom_col() +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::theme_minimal() +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"))

ggplot2::ggsave("inst/book/assets/graficos/g1_validacao_grupo.jpeg", g_validacao, width = 14, height = 7)

# Comparação tonicidade_producao e tonicidade_alvo
g_fluxo <- pseudopalavras::dados %>%
  dplyr::mutate(Coincidente = tonicidade_producao == tonicidade_alvo) %>%
  dplyr::count(grupo, tonicidade_producao, tonicidade_alvo, Coincidente) %>%
  # dplyr::filter(grupo == 1) %>%
  ggplot2::ggplot(ggplot2::aes(
    axis1 = tonicidade_alvo, axis2 = tonicidade_producao, y = n
  )) +
  geom_alluvium(ggplot2::aes(fill = Coincidente)) +
  geom_stratum() +
  ggplot2::geom_text(
    stat = "stratum",
    ggplot2::aes(label = after_stat(stratum))
  ) +
  scale_fill_viridis_d() +
  theme_minimal() +
  ggplot2::facet_wrap(~grupo) +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"))

ggplot2::ggsave("inst/book/assets/graficos/g1_fluxo.jpeg", g_fluxo, width = 14, height = 7)

# silaba por tonicidade
pseudopalavras::dados %>%
  dplyr::count(tonicidade_alvo, tonicidade_producao, silaba_modificada) %>%
  dplyr::mutate(silaba_modificada = dplyr::case_when(
    silaba_modificada == "0" ~ "Mais de uma sílaba modificada",
    silaba_modificada == "1" ~ "Primeira sílaba modificada",
    silaba_modificada == "2" ~ "Segunda sílaba modificada",
    silaba_modificada == "3" ~ "Terceira sílaba modificada"
  )) %>%
  tidyr::pivot_wider(names_from = silaba_modificada, values_from = n) %>%
  janitor::adorn_totals() %>%
  janitor::adorn_percentages(denominator = "col") %>%
  janitor::adorn_pct_formatting(digits = 1) %>%
  janitor::adorn_ns(position = "front") %>%
  knitr::kable(
    col.names = c(
      "Tonicidade Alvo", "Tonicidade Produção", "Mais de uma sílaba modificada",
      "Primeira sílaba modificada", "Segunda sílaba modificada", "Terceira sílaba modificada"
    ),
    caption = "Tonicidade de pseudopalavras por Tonicidade Alvo e Silaba Modificada",
    booktabs = TRUE,
    table.attr = "style='width:100%;'"
  ) %>%
  kableExtra::add_header_above(
    c(" " = 2, "Tonicidade Alvo" = 2, " " = 2)
  )
