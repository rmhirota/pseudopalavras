library(magrittr)

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

da_kappa <- pseudopalavras::dados %>%
  dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
  dplyr::group_by(pseudopalavra) %>%
  dplyr::summarise(
    oxitona = sum(tonicidade_producao == "oxítona"),
    paroxitona = sum(tonicidade_producao == "paroxítona"),
    proparoxitona = sum(tonicidade_producao == "proparoxítona")
  ) %>%
  dplyr::select(-pseudopalavra)

gwet.ac1.dist(da_kappa)
# fleiss.kappa.dist(da_kappa)





