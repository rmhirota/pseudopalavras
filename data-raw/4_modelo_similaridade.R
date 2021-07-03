library(magrittr)
library(mclogit)

base <- pseudopalavras::da_modelo
base_sem_naovalidadas <- pseudopalavras::da_modelo_validadas

# Teste retirando amostras de não validadas -------------------------------

#Tirando 25% das não validadas aleatoriamente
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
      similaridade + frequencia +
      segmento_modificado + bloco_apresentacao + aleatorizacao,
    random = ~1|informante,
    data = base_amostra,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  summary(mf_25)
}
# modelos ajustados para as 5 bases
resumos <- purrr::map(base_nval_filtrado, ajustar_para_amostra)


# Ajuste modelo -----------------------------------------------------------

# Modelo Completo (com interação)
mt <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    musica + linguas + idade + genero + escolaridade + area_formacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(mt)

# Modelo retirando Linguas - maior p valor nos dois modelos
m1 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    musica + idade + genero + escolaridade + area_formacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m1)

#Modelo retirando Genero - maior p valor nos dois modelos
m2 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + area_formacao + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m2)

# Modelo retirando Linguas - maior p valor nos dois modelos
m3 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao + idade +
    area_formacao + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m3)


# Modelo retirando Area de formação - maior p valor nos dois modelos
m4 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao + idade +
    escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m4)

#Modelo retirando Idade - maior p valor nos dois modelos
m5 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m5)

# Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um
# nível significante (Escolaridade)
m6 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m6)

mclogit:::AIC.mclogit(m5)
mclogit:::AIC.mclogit(m6)

# Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um
# nível significante (Bloco)
m7 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m7)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m7)

# O AIC piorou tirando as variáveis com níveis não significantes, então mantemos
# e testamos a variável de indivíduo Linguas
m8 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao + linguas,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m8)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m8)

# Testando m6, mas sem interação
m9 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade +
    frequencia + segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m9)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m9)

# Praticamente não teve diferença, então vamos mantar o modelo mais simples,
# sem interação.

# MODELO FINAL ------------------------------------------------------------

# mf2 = m9
modelo_simfreq <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade +
    frequencia + segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(modelo_simfreq)

readr::write_rds(modelo_simfreq,"data-raw/modelo_simfreq.rds")
tab_modelo_simfreq <- broom::tidy(modelo_simfreq)
usethis::use_data(tab_modelo_simfreq, overwrite = TRUE)



#Oxítonas versus Paroxítonas -----------------------------------------------------------

# No geral, temos que a probabilidade de uma pseudopalavra ser acentuada como oxítona
# (e não como paroxítona) é cerca de 1/5 da probabilidade dela ser acentuada como oxítona
# (e não paroxítona), quando todas as variáveis estão no nível de referência (Tonicidade Alvo é oxítona,
# Estrutura é CV-CV-CV, classificadas como dissimilar e de baixa frequência, com a modificação em uma
# consoante, apresentada no bloco 1 e tendo sido aleatorizada).
#
# A probabilidade de uma pseudopalavra ser acentuada como oxítona (e não como paroxítona)
# quando sua palavra alvo é paroxítona é aproximadamente 1/4 (0.247) da probabilidade dela ser acentuada como
# como oxítona (e não como paroxítona) quando sua palavra alvo é oxítona.
#
# A probabilidade de uma pseudopalavra ser acentuada como oxítona (e não como paroxítona)
# quando sua palavra alvo é proparoxítona é 0.13 vezes a probabilidade dela ser acentuada como
# como oxítona (e não como paroxítona) quando sua palavra alvo é oxítona.
#
# A probabilidade de uma pseudopalavra ser acentuada como oxítona (e não como paroxítona)
# quando sua estrutura é no formato CV-CV-CVC é aproximadament 73 vezes a probabilidade de uma pseudopalavra
# ser acentuada como oxítona (e não como paroxítona) quando sua estrutura é no formato CV-CV-CV.
#
# A probabilidade de uma pseudopalavra ser acentuada como oxítona (e não como paroxítona)
# quando sua palavra alvo é de alta frequência é 0.66 vezes a probabilidade de uma pseudopalavra
# ser acentuada como oxítona (e não como paroxítona) quando sua sua palavra alvo é de baixa frequência.
#
# A probabilidade de uma pseudopalavra ser acentuada como oxítona (e não como paroxítona)
# quando a mudança ocorreu em uma vogal é 1.21 vezes a probabilidade de uma pseudopalavra
# ser acentuada como oxítona (e não como paroxítona) quando a mudança ocorreu em uma consoante.
#
# A probabilidade de uma pseudopalavra ser acentuada como oxítona (e não como paroxítona)
# quando não houve aleatorização é aproximadamente 0.53 vezes a probabilidade de uma pseudopalavra
# ser acentuada como oxítona (e não como paroxítona) quando houve aleatorização.


#Proparoxítonas versus Paroxítonas -----------------------------------------------------------


# A probabilidade de uma pseudopalavra ser acentuada como proparoxítona (e não como paroxítona)
# quando sua palavra alvo é paroxítona é aproximadamente 0.54 da probabilidade dela ser acentuada como
# como proparoxítona (e não como paroxítona) quando sua palavra alvo é oxítona.
#
# A probabilidade de uma pseudopalavra ser acentuada como proparoxítona (e não como paroxítona)
# quando sua palavra alvo é proparoxítona é 5.27 vezes a probabilidade dela ser acentuada como
# como proparoxítona (e não como paroxítona) quando sua palavra alvo é oxítona.
#
# A probabilidade de uma pseudopalavra ser acentuada como proparoxítona (e não como paroxítona)
# quando sua estrutura é no formato CV-CV-CVC é 6.10 vezes a probabilidade de uma pseudopalavra
# ser acentuada como proparoxítona (e não como paroxítona) quando sua estrutura é no formato CV-CV-CV.
#
# A probabilidade de uma pseudopalavra ser acentuada como proparoxítona (e não como paroxítona)
# quando ela foi classificada como similar à sua palavra alvo é 3.7 vezes a probabilidade de uma pseudopalavra
# ser acentuada como proparoxítona (e não como paroxítona) quando ela foi classificada como dissimilar à sua palavra alvo.
#
# A probabilidade de uma pseudopalavra ser acentuada como proparoxítona (e não como paroxítona)
# quando a mudança ocorreu em uma vogal é 1.34 vezes a probabilidade de uma pseudopalavra
# ser acentuada como proparoxítona (e não como paroxítona) quando a mudança ocorreu em uma consoante.

