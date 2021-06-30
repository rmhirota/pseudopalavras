library(mclogit)

base <- readr::read_rds("data-raw/base_modelo.rds")
dplyr::glimpse(base)

base <- base %>%
  dplyr::mutate(
    similaridade = as.factor(ifelse(grupo %in% c(1, 3), 1, 0)),
    frequencia = as.factor(ifelse(grupo %in% c(1, 2), 1, 0))
  )
base$tonicidade_producao = relevel(base$tonicidade_producao, ref = "paroxítona")
base$escolaridade = relevel(base$escolaridade, ref = "Superior Incompleto")
base$aleatorizacao = relevel(base$aleatorizacao, ref = "s")

readr::write_rds(base, "data-raw/base_modelo.rds")
base_sem_naovalidadas <- base %>%
  dplyr::filter(validacao == "s"|validacao == "q")


# Modelo Completo (com interação)
mt <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    musica + linguas + idade + genero + escolaridade + area_formacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(mt)

# Modelo retirando Música - maior p valor nos dois modelos
m1 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + genero + escolaridade + area_formacao,
  random = ~1|informante, data = base_sem_naovalidadas,library(mclogit)

  # modelos salvos ----------------------------------------------------------

  summary(mf)

  # modelo com base apenas de validados e quase validados
  validados_mf <- readr::read_rds("data-raw/m_validados_mf.rds")
  summary(validados_mf)

  # modelo com base apenas de não validados
  nao_validados_mf <- readr::read_rds("data-raw/nao_validados_mf.rds")
  summary(nao_validados_mf)

  # modelo com a base completa, considerando validacao como covariável
  mf_validacao <- readr::read_rds("data-raw/m_mf_validacao.rds")
  summary(mf_validacao)

  # modelo considerando covariáveis similaridade e frequência (em vez de grupo)
  mf_similar <- readr::read_rds("data-raw/m_mf_similar.rds")
  summary(mf_similar)

  # modelo considerando covariáveis similaridade e frequência (em vez de grupo)
  # base apenas com validados
  validados_mf_similar <- readr::read_rds("data-raw/m_validados_mf_similar.rds")
  summary(validados_mf_similar)



  # base --------------------------------------------------------------------

  base <- readr::read_rds("data-raw/base_modelo.rds")

  base <- base %>%
    dplyr::mutate(
      validacao = as.factor(ifelse(validacao == "q", "s", validacao)),
      similar = dplyr::case_when(
        grupo == 1 | grupo == 3 ~ "similar",
        TRUE ~ "dissimilar"
      ),
      similar = as.factor(similar),
      frequencia = dplyr::case_when(
        grupo == 1 | grupo == 2 ~ "alta freq",
        TRUE ~ "baixa freq"
      ),
      frequencia = as.factor(frequencia)
    )

  base$validacao = relevel(base$validacao, ref = "s")

  b_validados <- base %>%
    dplyr::filter(validacao %in% c("s", "q"))
  b_nao_validados <- base %>%
    dplyr::filter(!validacao %in% c("s", "q"))

  # modelo para validados ---------------------------------------------------

  validados_mf <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      grupo + segmento_modificado+bloco_apresentacao + aleatorizacao,
    random = ~1|informante,
    data = b_validados,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  summary(validados_mf)
  readr::write_rds(validados_mf, "data-raw/m_validados_mf.rds")

  mt <- mblogit(
    tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
      grupo+segmento_modificado+bloco_apresentacao+
      aleatorizacao+musica+linguas+idade+genero+escolaridade+area_formacao,
    random = ~1|informante,
    data = b_validados,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  summary(mt)

  mclogit:::AIC.mclogit(mt)
  mclogit:::AIC.mclogit(validados_mf)


  # modelo para não validados -----------------------------------------------

  nao_validados_mf <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      grupo + segmento_modificado+bloco_apresentacao + aleatorizacao,
    random = ~1|informante,
    data = b_nao_validados,
    epsilon = 1e-05, maxit = 500,
    method = "PQL"
  )
  summary(nao_validados_mf)
  readr::write_rds(nao_validados_mf, "data-raw/nao_validados_mf.rds")

  predict(nao_validados_mf, type="response", conditional = TRUE)


  # considerando validacao como covariável ----------------------------------


  mf_validacao <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      grupo + segmento_modificado+bloco_apresentacao + aleatorizacao +
      validacao,
    random = ~1|informante,
    data = base,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  readr::write_rds(mf_validacao, "data-raw/m_mf_validacao.rds")

  summary(mf_validacao)
  mclogit:::AIC.mclogit(mf_validacao)
  mclogit:::AIC.mclogit(mf)

  # testando com variável similar -------------------------------------------

  mf_similar <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      segmento_modificado+bloco_apresentacao + aleatorizacao +
      similar + frequencia + validacao,
    random = ~1|informante,
    data = base,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  summary(mf_similar)
  readr::write_rds(mf_similar, "data-raw/m_mf_similar.rds")

  validados_mf_similar <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      segmento_modificado+bloco_apresentacao + aleatorizacao +
      similar + frequencia,
    random = ~1|informante,
    data = b_validados,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  summary(validados_mf_similar)
  readr::write_rds(validados_mf_similar, "data-raw/m_validados_mf_similar.rds")

  # testando diminuindo as não validadas -------------------------------------------

  #Tirando 25% das não validadas aleatoriamente
  pseudopalavras_nval <- base %>%
    filter(validacao == 'n') %>%
    distinct(pseudopalavra)
  set.seed(8)
  pseudo_escolhidas <- sample_n(pseudopalavras_nval, 16) %>% pull(pseudopalavra)
  base_nval_filtrado <- base %>% filter(!pseudopalavra %in% pseudo_escolhidas)


  #primeiro linguas, genero, musica, area de formacao, idade, escolaridade
  mf_25 <- mblogit(
    tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
      grupo+segmento_modificado+bloco_apresentacao + aleatorizacao,
    random = ~1|informante,
    data = base_nval_filtrado,
    epsilon = 1e-08, maxit = 30,
    method = "PQL"
  )
  summary(mf_25)


  #Tirando 50% das não validadas aleatoriamente
  set.seed(10)
  pseudo_escolhidas <- sample_n(pseudopalavras_nval, 32) %>% pull(pseudopalavra)
  base_nval_filtrado <- base %>% filter(!pseudopalavra %in% pseudo_escolhidas)

  mf_50 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
                    grupo+segmento_modificado+bloco_apresentacao+
                    aleatorizacao, random = ~1|informante,data=base_nval_filtrado,
                  epsilon = 1e-08, maxit = 30, method = "PQL")

  summary(mf_50)


  #Tirando 75% das não validadas aleatoriamente
  set.seed(80)
  pseudo_escolhidas <- sample_n(pseudopalavras_nval, 47) %>% pull(pseudopalavra)
  base_nval_filtrado <- base %>% filter(!pseudopalavra %in% pseudo_escolhidas)

  mf_75 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
                    grupo+segmento_modificado+bloco_apresentacao+
                    aleatorizacao, random = ~1|informante,data=base_nval_filtrado,
                  epsilon = 1e-08, maxit = 30, method = "PQL")

  summary(mf_75)

  # Validação é importante, como lidar?
  # - não validados: n pequeno (63 pseudopalavras)
  # - muda sinal de alguns betas

  # Aleatorização é importante

  # Separar grupo em similaridade e frequência?
  # - outra alternativa: deixar só frequência e usar taxa de similaridade (como numérica)





  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m1)

#Modelo retirando Área de formação - maior p valor nos dois modelos
m2 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + genero + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m2)

# Modelo retirando Gênero - maior p valor nos dois modelos
m3 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    linguas + idade + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m3)

# Modelo retirando Línguas - maior p valor nos dois modelos
m4 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao +
    idade + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m4)

#Modelo retirando Idade - maior p valor nos dois modelos
m5 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao + escolaridade,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m5)

# Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Escolaridade)
m6 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m6)

mclogit:::AIC.mclogit(m5)
mclogit:::AIC.mclogit(m6)

# Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Bloco)
m7 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
    segmento_modificado + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(m7)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m7)

# O AIC piorou tirando as variáveis com níveis não significantes, então mantemos
# e testamos a variável de indivíduo Linguas
m8 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade*frequencia +
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

# mf = m9
mf2 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade +
    frequencia + segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(mf2)


# Modelos considerando validação ------------------------------------------

base <- base %>%
  dplyr::mutate(
    validacao = as.factor(ifelse(validacao == "q", "s", validacao)))

base$validacao = relevel(base$validacao, ref = "s")

b_validados <- base %>%
  dplyr::filter(validacao %in% c("s", "q"))
b_nao_validados <- base %>%
  dplyr::filter(!validacao %in% c("s", "q"))

# modelo para validados ---------------------------------------------------

validados_mf <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade +
    frequencia + segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante, data = b_validados,
  epsilon = 1e-08, maxit = 30, method = "PQL")
summary(validados_mf)
readr::write_rds(validados_mf, "data-raw/m_validados_mf.rds")

# tirando 25% das não validadas
pseudopalavras_nval <- base %>%
  filter(validacao == 'n') %>%
  distinct(pseudopalavra)
# set.seed(8)
set.seed(24)
pseudo_escolhidas <- sample_n(pseudopalavras_nval, 16) %>% pull(pseudopalavra)
base_nval_filtrado <- base %>% filter(!pseudopalavra %in% pseudo_escolhidas)

# primeiro linguas, genero, musica, area de formacao, idade, escolaridade
mf_25 <- mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra + similaridade +
    frequencia + segmento_modificado + bloco_apresentacao + aleatorizacao,
  random = ~1|informante,
  data = base_nval_filtrado,
  epsilon = 1e-08, maxit = 30,
  method = "PQL"
)
summary(mf_25)

# para seed = 8, similaridade deu não significativo
# para seed = 24, similaridade deu significativo
# vamos seguir como base SEM as palavras não validadas.


# Interpretação -----------------------------------------------------------





