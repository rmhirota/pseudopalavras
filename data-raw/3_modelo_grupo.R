library(magrittr)
library(mclogit)
devtools::install()

base <- pseudopalavras::da_modelo
base_sem_naovalidadas <- pseudopalavras::da_modelo_validadas

# Teste retirando amostras de não validadas -------------------------------

# Tirando 25% das não validadas aleatoriamente
pseudopalavras_nval <- base %>%
  dplyr::filter(validacao == 'n') %>%
  dplyr::pull(pseudopalavra) %>%
  unique()

set.seed(24)
# 5 amostras de pseudopalavras não validadas
pseudo_escolhidas <- purrr::map(1:5, ~sample(pseudopalavras_nval, 16))
# 5 bases tirando as pseudopalavras
base_nval_filtrado <- purrr::map(
  pseudo_escolhidas, ~{
    base %>%
      dplyr::filter(!pseudopalavra %in% .x)
  }
)
# ajusta modelos
ajustar_para_amostra <- function(base_amostra) {
  mf_25 <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      grupo + segmento_modificado + bloco_apresentacao + aleatorizacao,
    random = ~ 1 | informante,
    data = base_amostra,
    epsilon = 1e-08,
    maxit = 30,
    method = "PQL"
  ) %>%
    summary(mf_25)
}
# modelos ajustados para as 5 bases
resumos <- purrr::map(base_nval_filtrado, ajustar_para_amostra)
resumos

# Ajuste modelo -----------------------------------------------------------

# Modelo Completo
mt <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    grupo + segmento_modificado + bloco_apresentacao +
    aleatorizacao + musica + linguas + idade + genero + escolaridade +
    area_formacao,
  random = ~1|informante,
  data = base_sem_naovalidadas,
  epsilon = 1e-08,
  maxit = 30,
  method = "PQL"
)
summary(mt)

# Modelo retirando Música - maior p valor nos dois modelos
m1 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + genero + escolaridade + area_formacao,
  random = ~1|informante, data = base_sem_naovalidadas)

# Modelo retirando Área de formação - maior p valor nos dois modelos
m2 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + genero + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m2)

# Modelo retirando Gênero - maior p valor nos dois modelos
m3 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m3)

# Modelo retirando Línguas - maior p valor nos dois modelos
m4 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    idade + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m4)

#Modelo retirando Idade - maior p valor nos dois modelos
m5 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m5)

# Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Escolaridade)
m6 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m6)

mclogit:::AIC.mclogit(m5)
mclogit:::AIC.mclogit(m6)

# Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Bloco)
m7 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m7)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m7)

# O AIC piorou tirando as variáveis com níveis não significantes, então mantemos
# e testamos a variável de indivíduo Linguas
m8 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao + linguas,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m8)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m8)

# Praticamente não teve diferença, então vamos mantar o modelo sem línguas

# MODELO FINAL ------------------------------------------------------------

# modelo final = m6
modelo_grupo <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + grupo +
    segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(modelo_grupo)
tab_modelo_grupo <- broom::tidy(modelo_grupo)

# Salva objetos com modelo
readr::write_rds(modelo_grupo, "data-raw/modelo_grupo.rds")
usethis::use_data(modelo_grupo, overwrite = TRUE)
usethis::use_data(tab_modelo_grupo, overwrite = TRUE)

# predições
modelo_grupo_pred <- modelo_grupo %>%
  predict(type = "response", conditional = TRUE) %>%
  tibble::as_tibble()
usethis::use_data(modelo_grupo_pred, overwrite = TRUE)


