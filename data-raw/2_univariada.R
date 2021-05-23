# univariada

# Testes:
# chisq.test()
# prop.test()
# fisher.test()

devtools::load_all()


# resposta X grupo --------------------------------------------------------

# qtd de respostas por grupo
dplyr::count(dados, grupo)

dados %>%
  dplyr::count(grupo, tonicidade_producao) %>%
  tidyr::pivot_wider(names_from = tonicidade_producao, values_from = n) %>%
  dplyr::select(-grupo) %>%
  as.matrix() %>%
  chisq.test()
# H_0: grupo não determina localização acentual (não há associação entre grupo e tonicidade)
# H_1: grupo determina localização acentual (há associação entre grupo e tonicidade)
# Pearson's Chi-squared test
#
# data:  .
# X-squared = 123.71, df = 6, p-value < 2.2e-16

# CONFIRMAR: número total de pseudopalavras em cada grupo é diferente, e o teste usa
# contagem. Se temos dois tipos aleatórios (falante e pseudopalavra) esses testes
# ainda fazem sentido?

tab_grupo = table(dados$grupo,dados$tonicidade_producao)
CA_grupo=ca(tab_grupo)
plot(CA_grupo, labels=c(2,2))

# resposta X estrutura ----------------------------------------------------

# qtd de respostas por estrutura
dplyr::count(dados, estrutura_palavra)

dados %>%
  dplyr::count(estrutura_palavra, tonicidade_producao) %>%
  tidyr::pivot_wider(names_from = tonicidade_producao, values_from = n) %>%
  dplyr::select(-estrutura_palavra) %>%
  as.matrix() %>%
  chisq.test()
# H_0: estrutura não determina localização acentual (não há associação entre estrutura e tonicidade)
# H_1: estrutura determina localização acentual (há associação entre estrutura e tonicidade)
# Pearson's Chi-squared test
#
# data:  .
# X-squared = 6120.7, df = 2, p-value < 2.2e-16

tab_est = table(dados$tonicidade_producao,dados$estrutura_palavra)
CA_est = ca(tab_est)
plot(CA_est, labels=c(2,2))   #parece que só deu 1 dimensão

# resposta X segmento modificado ------------------------------------------

# qtd de respostas por estrutura
dplyr::count(dados, segmento_modificado)

dados %>%
  dplyr::count(estrutura_palavra, tonicidade_producao) %>%
  tidyr::pivot_wider(names_from = tonicidade_producao, values_from = n) %>%
  dplyr::select(-estrutura_palavra) %>%
  as.matrix() %>%
  chisq.test()

# H_0: estrutura não determina localização acentual (não há associação entre estrutura e tonicidade)
# H_1: estrutura determina localização acentual (há associação entre estrutura e tonicidade)
# Pearson's Chi-squared test
#
# data:  .
# X-squared = 6120.7, df = 2, p-value < 2.2e-16

# resposta x tonicidade_alvo ----------------------------------------------

#qtd de respostas por tonicidade_alvo
dplyr::count(dados, tonicidade_alvo, tonicidade_producao)

dados %>%
  dplyr::count(tonicidade_alvo, tonicidade_producao) %>%
  tidyr::pivot_wider(names_from = tonicidade_producao, values_from = n) %>%
  dplyr::select(-tonicidade_alvo) %>%
  as.matrix() %>%

  chisq.test()
# H_0: tonicidade da palavra-alvo não determina localização acentual
# (não há associação entre as variáveis)
# H_1: tonicidade da palavra-alvo determina localização acentual
# Pearson's Chi-squared test
# data:  .
# X-squared = 1823.9, df = 4, p-value < 2.2e-16


tab_alvo = table(dados$tonicidade_producao,dados$tonicidade_alvo)
CA_alvo = ca(tab_alvo)
plot(CA_alvo, labels=c(2,2))


# resposta x validacao ----------------------------------------------------
# Vale olhar a validação?


# resposta X taxa de similaridade -----------------------------------------

dados %>%
  dplyr::mutate(similar = ifelse(
    taxa_similaridade %in% 1:3, "similar", "dissimilar"
  ))

dados %>%
  dplyr::filter(grupo == 4, taxa_similaridade == 2) %>%
  dplyr::count(pseudopalavra)

# resposta X taxa de similaridade -----------------------------------------

dados %>%
  dplyr::mutate(similar = ifelse(
    taxa_similaridade %in% 1:3, "similar", "dissimilar"
  ))

dados %>%
  dplyr::filter(grupo == 4, taxa_similaridade == 2) %>%
  dplyr::count(pseudopalavra)


# resposta x formação --------------------------------------------------------------------
dados %>% dplyr::distinct(informante, .keep_all = TRUE) %>%
  dplyr::count(area_formacao,tonicidade_producao) %>%
  tidyr::pivot_wider(names_from = tonicidade_producao, values_from = n) %>%
   dplyr::select(-area_formacao) %>% as.matrix() %>%  fisher.test()

#Fisher's Exact Test for Count Data
#data:  .
#p-value = 0.5224
#alternative hypothesis: two.sided

