library(magrittr)
library(dplyr)
# Teste Kappa -------------------------------------------------------------
library(irr)
# documentação: https://www.rdocumentation.org/packages/irr/versions/0.84.1/topics/kappam.fleiss
# exemplos: https://www.datanovia.com/en/lessons/fleiss-kappa-in-r-for-multiple-categorical-variables/

da_kappa <- pseudopalavras::dados %>%
  dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
  tidyr::pivot_wider(names_from = informante, values_from = tonicidade_producao) %>%
  dplyr::select(-pseudopalavra)

kappam.fleiss(da_kappa, detail = TRUE)


# Teste Gwet --------------------------------------------------------------
library(irrCAC)
# vignette: https://cran.r-project.org/web/packages/irrCAC/vignettes/overview.html

da <- pseudopalavras::dados %>%
  dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
  dplyr::group_by(pseudopalavra) %>%
  dplyr::summarise(
    oxitona = sum(tonicidade_producao == "oxítona"),
    paroxitona = sum(tonicidade_producao == "paroxítona"),
    proparoxitona = sum(tonicidade_producao == "proparoxítona")
  ) %>%
  dplyr::select(-pseudopalavra)

gwet.ac1.dist(da)
# com esse pacote não tem como fazer o teste para cada nível


# Teste para cada grupo ---------------------------------------------------

teste_grupo <- function(num_grupo) {
  da <- pseudopalavras::dados %>%
    dplyr::filter(grupo == num_grupo) %>%
    dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
    dplyr::group_by(pseudopalavra) %>%
    dplyr::summarise(
      oxitona = sum(tonicidade_producao == "oxítona"),
      paroxitona = sum(tonicidade_producao == "paroxítona"),
      proparoxitona = sum(tonicidade_producao == "proparoxítona")
    ) %>%
    dplyr::select(-pseudopalavra)
  gwet.ac1.dist(da) %>%
    dplyr::mutate(grupo = paste0("Grupo ", num_grupo)) %>%
    dplyr::relocate(grupo)
}

purrr::map_dfr(1:4, teste_grupo)


# Teste para cada estrutura -----------------------------------------------

teste_estrutura <- function(estrut) {
  da <- pseudopalavras::dados %>%
    dplyr::filter(estrutura_palavra == estrut) %>%
    dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
    dplyr::group_by(pseudopalavra) %>%
    dplyr::summarise(
      oxitona = sum(tonicidade_producao == "oxítona"),
      paroxitona = sum(tonicidade_producao == "paroxítona"),
      proparoxitona = sum(tonicidade_producao == "proparoxítona")
    ) %>%
    dplyr::select(-pseudopalavra)
  gwet.ac1.dist(da) %>%
    dplyr::mutate(estrutura = estrut) %>%
    dplyr::relocate(estrutura)
}

purrr::map_dfr(unique(pseudopalavras::dados$estrutura_palavra), teste_estrutura)



#teste para segmento modificado


teste_segmento <- function(seg) {
  da <- pseudopalavras::dados %>%
    dplyr::filter(segmento_modificado == seg) %>%
    dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
    dplyr::group_by(pseudopalavra) %>%
    dplyr::summarise(
      oxitona = sum(tonicidade_producao == "oxítona"),
      paroxitona = sum(tonicidade_producao == "paroxítona"),
      proparoxitona = sum(tonicidade_producao == "proparoxítona")
    ) %>%
    dplyr::select(-pseudopalavra)
  gwet.ac1.dist(da) %>%
    dplyr::mutate(segmento_modificado = seg) %>%
    dplyr::relocate(segmento_modificado)
}

purrr::map_dfr(unique(pseudopalavras::dados$segmento_modificado), teste_segmento)


#teste para silaba modificada

teste_silaba <- function(silab) {
  da <- pseudopalavras::dados %>%
    dplyr::filter(silaba_modificada == silab) %>%
    dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
    dplyr::group_by(pseudopalavra) %>%
    dplyr::summarise(
      oxitona = sum(tonicidade_producao == "oxítona"),
      paroxitona = sum(tonicidade_producao == "paroxítona"),
      proparoxitona = sum(tonicidade_producao == "proparoxítona")
    ) %>%
    dplyr::select(-pseudopalavra)
  gwet.ac1.dist(da) %>%
    dplyr::mutate(silaba_modificada = silab) %>%
    dplyr::relocate(silaba_modificada)
}

purrr::map_dfr(unique(pseudopalavras::dados$silaba_modificada), teste_silaba)



