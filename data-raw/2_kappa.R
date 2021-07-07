library(magrittr)
devtools::install()

# Kappa de Fleiss (generalizado )-----------------------------------------
# documentação: https://www.rdocumentation.org/packages/irr/versions/0.84.1/topics/kappam.fleiss
# exemplos: https://www.datanovia.com/en/lessons/fleiss-kappa-in-r-for-multiple-categorical-variables/

da_kappa <- pseudopalavras::dados %>%
  dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
  tidyr::pivot_wider(names_from = informante, values_from = tonicidade_producao) %>%
  dplyr::select(-pseudopalavra)

irr::kappam.fleiss(da_kappa, detail = TRUE)

# AC1 de Gwet ------------------------------------------------------------
# vignette irrCAC: https://cran.r-project.org/web/packages/irrCAC/vignettes/overview.html
# com esse pacote não tem como fazer o teste para cada nível

da <- pseudopalavras::dados %>%
  dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
  dplyr::group_by(pseudopalavra) %>%
  dplyr::summarise(
    oxitona = sum(tonicidade_producao == "oxítona"),
    paroxitona = sum(tonicidade_producao == "paroxítona"),
    proparoxitona = sum(tonicidade_producao == "proparoxítona")
  ) %>%
  dplyr::select(-pseudopalavra)

irrCAC::gwet.ac1.dist(da)

# AC1 de Gwet para cada grupo --------------------------------------------

ac1_grupo <- function(num_grupo) {
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
  irrCAC::gwet.ac1.dist(da) %>%
    dplyr::mutate(grupo = paste0("Grupo ", num_grupo)) %>%
    dplyr::relocate(grupo)
}

purrr::map_dfr(1:4, ac1_grupo)


# AC1 de Gwet para cada estrutura ---------------------------------------

ac1_estrutura <- function(estrut) {
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
  irrCAC::gwet.ac1.dist(da) %>%
    dplyr::mutate(estrutura = estrut) %>%
    dplyr::relocate(estrutura)
}
purrr::map_dfr(unique(pseudopalavras::dados$estrutura_palavra), ac1_estrutura)

# AC1 de Gwet para cada segmento modificada -----------------------------

ac1_segmento <- function(seg) {
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
  irrCAC::gwet.ac1.dist(da) %>%
    dplyr::mutate(segmento_modificado = seg) %>%
    dplyr::relocate(segmento_modificado)
}
purrr::map_dfr(unique(pseudopalavras::dados$segmento_modificado), ac1_segmento)


# AC1 de Gwet para cada sílaba modificada -------------------------------

ac1_silaba <- function(silab) {
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
  irrCAC::gwet.ac1.dist(da) %>%
    dplyr::mutate(silaba_modificada = silab) %>%
    dplyr::relocate(silaba_modificada)
}
purrr::map_dfr(unique(pseudopalavras::dados$silaba_modificada), ac1_silaba)


# AC1 de Gwet para música -----------------------------------------------

ac1_musica <- function(music) {
  da <- pseudopalavras::dados %>%
    dplyr::filter(musica == music) %>%
    dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
    dplyr::group_by(pseudopalavra) %>%
    dplyr::summarise(
      oxitona = sum(tonicidade_producao == "oxítona"),
      paroxitona = sum(tonicidade_producao == "paroxítona"),
      proparoxitona = sum(tonicidade_producao == "proparoxítona")
    ) %>%
    dplyr::select(-pseudopalavra)
  irrCAC::gwet.ac1.dist(da) %>%
    dplyr::mutate(musica = music) %>%
    dplyr::relocate(musica)
}
purrr::map_dfr(unique(pseudopalavras::dados$musica), ac1_musica)
