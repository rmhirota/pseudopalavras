library(mclogit)

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
pseudopalavras_nval <- base%>% filter(validacao == 'n') %>%distinct(pseudopalavra)
set.seed(8)
pseudo_escolhidas <- sample_n(as.data.frame(pseudopalavras_nval), 16)
base_nval_filtrado <- base %>% filter( !pseudopalavra %in% c("firafe","pafeta","tibela",
        "taleto","patuta","farefo","cobare","davavel","pileto","pabeto","lateta","tarana",
        "titale","patala","sacala","dipinar"))



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


#Tirando 75% das não validadas aleatoriamente
pseudo_escolhidas <- sample_n(as.data.frame(pseudopalavras_nval), 47)
base_nval_filtrado <- base %>% filter(!pseudopalavra %in% c( "cadada","tapina","pafeta","firufo" , "canadu"  ,"recitol", "jomite" , "sacala" , "puteta" , "taleto",
                                                              "tibela","cobare","tarana","decode" , "comitu",  "parafa" , "fanada",  "tisaca" , "coceca",  "pateda",
                                                              "titale","tisiba","tetala","perana"  ,"davavel" ,"pileto"  ,"farefo",  "dipinar" ,"poleto",  "copite" ,
                                                              "parane","fecital","cabate","pefala",  "tabala"  ,"cabara",  "secuno",  "patala"  ,"comote",  "tisoca" ,
                                                              "patana","canida","zutavel","babela" , "canaga"  ,"comife" , "cabure" ))

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




