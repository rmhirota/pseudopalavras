library(magrittr)
library(mclogit)
devtools::install()

# Modelo final usando grupo -----------------------------------------------

mf <- pseudopalavras::modelo_simfreq

# Probabilidades preditas (p1,p2,p3) por linha da base
p <- pseudopalavras::modelo_simfreq_pred
p %>% dplyr::slice(100:110)

# Efeitos aleatórios ------------------------------------------------------

qqnorm(
  mf$random.effects[[1]],
  xlab = "Quantis Teóricos",
  ylab = "Efeitos Aleatórios Preditos",
  main = ""
)
qqline(mf$random.effects[[1]], col = "red")

# Compara maior probabilidade predita com tonicidade alvo -----------------

compara_predito_observado <- p %>%
  dplyr::mutate(ind = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = paroxítona:proparoxítona) %>%
  dplyr::group_by(ind) %>%
  dplyr::slice(which.max(value)) %>%
  dplyr::pull(name) %>%
  dplyr::mutate(p, predito = .) %>%
  dplyr::bind_cols(observado = pseudopalavras::da_modelo_validadas$tonicidade_producao)

table(compara_predito_observado$predito, compara_predito_observado$observado)

compara_predito_observado %>%
  dplyr::count(predito)

compara_predito_observado %>%
  dplyr::count(observado)

# Medidas de acerto para oxitona ----------------------------------------

tab_oxi <- compara_predito_observado %>%
  dplyr::mutate(
    observado = ifelse(observado == "oxítona", "oxítona", "resto"),
    predito = ifelse(predito == "oxítona", "oxítona", "resto")
  )
tab_oxi <- table(tab_oxi$predito, tab_oxi$observado)

ss_mf_oxitona <- epiR::epi.tests(tab_oxi, conf.level = 0.95)
readr::write_rds(ss_mf_oxitona, "data-raw/acerto_simfreq_oxitona.rds")

# Point estimates and 95 % CIs:
# ---------------------------------------------------------=
# Apparent prevalence                    0.45 (0.44, 0.46)
# True prevalence                        0.44 (0.43, 0.45)
# Sensitivity                            0.87 (0.86, 0.88)
# Specificity                            0.88 (0.87, 0.89)
# Positive predictive value              0.85 (0.84, 0.86)
# Negative predictive value              0.89 (0.88, 0.90)
# Positive likelihood ratio              7.23 (6.74, 7.76)
# Negative likelihood ratio              0.15 (0.14, 0.16)
# ---------------------------------------------------------=

# Medidas de acerto para paroxitona -------------------------------------

tab_par <- compara_predito_observado %>%
  dplyr::mutate(
    observado = ifelse(observado == "paroxítona", "paroxítona", "resto"),
    predito = ifelse(predito == "paroxítona", "paroxítona", "resto")
  )
tab_par <- table(tab_par$predito, tab_par$observado)

ss_mf_paroxitona <- epiR::epi.tests(tab_par, conf.level = 0.95)
readr::write_rds(ss_mf_paroxitona, "data-raw/acerto_simfreq_paroxitona.rds")

# Point estimates and 95 % CIs:
# ---------------------------------------------------------=
# Apparent prevalence                    0.55 (0.54, 0.56)
# True prevalence                        0.52 (0.51, 0.53)
# Sensitivity                            0.89 (0.88, 0.90)
# Specificity                            0.83 (0.82, 0.84)
# Positive predictive value              0.85 (0.84, 0.86)
# Negative predictive value              0.87 (0.86, 0.88)
# Positive likelihood ratio              5.20 (4.89, 5.54)
# Negative likelihood ratio              0.13 (0.12, 0.14)
# ---------------------------------------------------------=

# Medidas de acerto para proparoxitona ----------------------------------

tab_pro <- compara_predito_observado %>%
  dplyr::mutate(
    observado = ifelse(observado == "proparoxítona", "proparoxítona", "resto"),
    predito = ifelse(predito == "proparoxítona", "proparoxítona", "resto")
  )
tab_pro <- table(tab_pro$predito, tab_pro$observado)

ss_mf_proparoxitona <- epiR::epi.tests(tab_pro, conf.level = 0.95)
readr::write_rds(ss_mf_proparoxitona, "data-raw/acerto_simfreq_proparoxitona.rds")

# Point estimates and 95 % CIs:
# ---------------------------------------------------------=
# Apparent prevalence                    0.01 (0.00, 0.01)
# True prevalence                        0.04 (0.03, 0.04)
# Sensitivity                            0.11 (0.08, 0.14)
# Specificity                            1.00 (1.00, 1.00)
# Positive predictive value              0.68 (0.55, 0.79)
# Negative predictive value              0.97 (0.96, 0.97)
# Positive likelihood ratio              54.15 (32.11, 91.33)
# Negative likelihood ratio              0.89 (0.86, 0.93)
# ---------------------------------------------------------=

# Resíduos (não aplicável para multinomial) -------------------------------

r <- matrix(mf$deviance.residuals, 3, 10393) %>%
  t() %>%
  tibble::as_tibble(.name_repair = "minimal") %>%
  purrr::set_names("paroxitona", "oxitona", "proparoxitona")
r %>% dplyr::slice(100:110)

plot_residuo <- function(tonicidade, tonicidade_pred) {
  p %>%
    purrr::set_names("p_par", "p_oxi", "p_pro") %>%
    dplyr::bind_cols(r) %>%
    dplyr::filter({{ tonicidade }} != 0) %>%
    ggplot2::ggplot(ggplot2::aes(y = {{ tonicidade }}, x = {{ tonicidade_pred }})) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Predito", y = "Resíduo",
      title = rlang::expr({{ tonicidade }}),
      subtitle = "Valores preditos para pi e resíduos"
    )
}
plot_residuo(paroxitona, p_par)
plot_residuo(oxitona, p_oxi)
plot_residuo(proparoxitona, p_pro)
