library(magrittr)
library(irr)

# deixar dados no formato para teste --------------------------------------

da_kappa <- pseudopalavras::dados %>%
  dplyr::select(informante, tonicidade_producao, pseudopalavra) %>%
  tidyr::pivot_wider(names_from = informante, values_from = tonicidade_producao) %>%
  dplyr::select(-pseudopalavra)

# teste -------------------------------------------------------------------

kappam.fleiss(da_kappa, detail = TRUE)


