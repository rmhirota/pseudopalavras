library(magrittr)

# base original
dados <- readr::read_csv("data-raw/Resultados_Totais_Experimento_1_Final.csv")
dados <- janitor::clean_names(dados)

# Agrupa níveis de variáveis ----------------------------------------------

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

# Inconsistências ---------------------------------------------------------

# dados %>%
#   dplyr::filter(taxa_similaridade < 4, grupo %in% c(2, 4)) %>%
#   dplyr::count(pseudopalavra, grupo, taxa_similaridade) %>%
#   dplyr::select(-n) %>%
#   readr::write_csv("data-raw/inconsistencias_similaridade.csv")
#
# dados %>%
#   dplyr::count(grupo, tonicidade_alvo) %>%
#   tidyr::pivot_wider(names_from = grupo, values_from = n)
#
# dados %>%
#   dplyr::filter(taxa_similaridade < 4, grupo %in% c(2, 4)) %>%
#   dplyr::distinct(pseudopalavra, .keep_all = TRUE) %>%
#   dplyr::count(segmento_modificado)


# Arruma para modelagem ---------------------------------------------------

# base completa
da_modelo <- dados %>%
  dplyr::mutate(
    # arruma tipos de variáveis
    codigo_pseudo = as.factor(codigo_pseudo),
    tempo_resposta = as.double(tempo_resposta),
    informante = as.factor(informante),
    tonicidade_producao = as.factor(tonicidade_producao),
    tonicidade_alvo = as.factor(tonicidade_alvo),
    estrutura_palavra = as.factor(estrutura_palavra),
    grupo = as.factor(grupo),
    musica = as.factor(musica),
    aleatorizacao = as.factor(aleatorizacao),
    bloco_apresentacao = as.factor(bloco_apresentacao),
    vizinhanca_tonicidade = as.factor(vizinhanca_tonicidade),
    segmento_modificado = as.factor(segmento_modificado),
    silaba_modificada = as.factor(silaba_modificada),
    genero = as.factor(genero),
    escolaridade = as.factor(escolaridade),
    area_formacao = as.factor(area_formacao),
    linguas = as.factor(linguas),
    # cria variáveis similaridade e frequência a partir de grupo
    similaridade = as.factor(ifelse(grupo %in% c(1, 3), 1, 0)),
    frequencia = as.factor(ifelse(grupo %in% c(1, 2), 1, 0))
  ) %>%
  # retira variáveis com NAs que não vão ser utilizadas
  dplyr::select(-c(
    vizinhanca_tonicidade, vizinhanca_fonologica
  ))

# redefine níveis base de comparação
da_modelo$tonicidade_producao <- relevel(da_modelo$tonicidade_producao, ref = "paroxítona")
da_modelo$escolaridade <- relevel(da_modelo$escolaridade, ref = "Superior Incompleto")
da_modelo$aleatorizacao <- relevel(da_modelo$aleatorizacao, ref = "s")

# salva base arrumada (completa) no pacote
usethis::use_data(da_modelo, overwrite = TRUE)

# base apenas com validados e quase validados
da_modelo_validadas <- da_modelo %>%
  dplyr::filter(validacao == "s" | validacao == "q")

# salva base arrumada (apenas validados e quase validados) no pacote
usethis::use_data(da_modelo_validadas, overwrite = TRUE)
