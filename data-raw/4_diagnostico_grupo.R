library(magrittr)

#Lendo o resultado do modelo final, retiradas as não validadas e usando grupo ------------
mf <- readr::read_rds("data-raw/m_mf_grupo_val.rds")
summary(mf)

# Nos dá as probabilidades (p1,p2,p3) por linha da base --------------------------
p <- predict(mf, type = "response", conditional = TRUE) %>%
  tibble::as_tibble()
p %>% dplyr::slice(100:110)


# Resíduos ----------------------------------------------------------------

# r <- mclogit:::residuals.mclogit(mf, type = "deviance")
# mf$deviance.residuals[1:10]
r <- matrix(mf$deviance.residuals, 3,10393) %>%
  t() %>%
  tibble::as_tibble() %>%
  purrr::set_names("paroxitona", "oxitona", "proparoxitona")
r %>% dplyr::slice(100:110)

plot_residuo <- function(tonicidade, tonicidade_pred) {
  p %>%
    purrr::set_names("p_par", "p_oxi", "p_pro") %>%
    dplyr::bind_cols(r) %>%
    dplyr::filter({{tonicidade}} != 0) %>%
    ggplot2::ggplot(ggplot2::aes(y = {{tonicidade}}, x = {{tonicidade_pred}})) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Predito", y = "Resíduo",
      title = rlang::expr({{tonicidade}}),
      subtitle = "Valores preditos para pi e resíduos"
    )
}
plot_residuo(paroxitona, p_par)
plot_residuo(oxitona, p_oxi)
plot_residuo(proparoxitona, p_pro)



#Queremos ver se a maior probabilidade coincide com a tonicidade alvo --------------------------
comp1 <- p %>%
  dplyr::mutate(ind = dplyr::row_number())%>%
  tidyr::pivot_longer(cols = paroxítona:proparoxítona)%>%
  dplyr::group_by(ind)%>%
  dplyr::slice(which.max(value))%>%
  dplyr::pull(name)%>%
  dplyr::mutate(p, max = .)


data.frame(rm = comp1$max, re = base_sem_naovalidadas$tonicidade_producao)

#Nos dá o comparativo entre a variável resposta e a resposta que o modelo daria -------------------

t = table(comp1$max,base_sem_naovalidadas$tonicidade_producao)
t

as.data.frame(comp1$max)%>%group_by(`comp1$max`)%>%count()

as.data.frame(base_sem_naovalidadas$tonicidade_producao)%>%group_by(`base_sem_naovalidadas$tonicidade_producao`)%>%count()

#Fazendo sensibilidade e blablabla para oxitona --------------------------
to = data.frame(m.oxitona = c(3958,692,4650), m.resto = c(611,5132,5743))
rownames(to) = c("Sim", "Não", "Total")
ss_mf_oxitona = epi.tests(t(to), conf.level = 0.95)
readr::write_rds(ss_mf_oxitona, "data-raw/sensibilidade_mf_oxitona.rds")

'''Point estimates and 95 % CIs:
---------------------------------------------------------
Apparent prevalence                    0.45 (0.44, 0.46)
True prevalence                        0.44 (0.43, 0.45)
Sensitivity                            0.87 (0.86, 0.88)
Specificity                            0.88 (0.87, 0.89)
Positive predictive value              0.85 (0.84, 0.86)
Negative predictive value              0.89 (0.89, 0.90)
Positive likelihood ratio              7.29 (6.79, 7.83)
Negative likelihood ratio              0.15 (0.14, 0.16)
---------------------------------------------------------
'''

#Fazendo sensibilidade e blablabla para paroxitona --------------------------
tp = data.frame(m.paroxitona = c(4836,843,5679), m.resto = c(600,4114,4714))
rownames(tp) = c("Sim", "Não", "Total")
ss_mf_paroxitona = epi.tests(t(tp), conf.level = 0.95)

'''Point estimates and 95 % CIs:
---------------------------------------------------------
Apparent prevalence                    0.55 (0.54, 0.56)
True prevalence                        0.52 (0.51, 0.53)
Sensitivity                            0.89 (0.88, 0.90)
Specificity                            0.83 (0.82, 0.84)
Positive predictive value              0.85 (0.84, 0.86)
Negative predictive value              0.87 (0.86, 0.88)
Positive likelihood ratio              5.23 (4.92, 5.57)
Negative likelihood ratio              0.13 (0.12, 0.14)
---------------------------------------------------------
'''


#Fazendo sensibilidade e blablabla para proparoxitona --------------------------
tpr = data.frame(m.proparoxitona = c(42,22,64), m.resto = c(346,9983,10329))
rownames(tpr) = c("Sim", "Não", "Total")
ss_mf_proparoxitona = epi.tests(t(tpr), conf.level = 0.95)

'''Point estimates and 95 % CIs:
---------------------------------------------------------
Apparent prevalence                    0.01 (0.00, 0.01)
True prevalence                        0.04 (0.03, 0.04)
Sensitivity                            0.11 (0.08, 0.14)
Specificity                            1.00 (1.00, 1.00)
Positive predictive value              0.66 (0.53, 0.77)
Negative predictive value              0.97 (0.96, 0.97)
Positive likelihood ratio              49.23 (29.69, 81.63)
Negative likelihood ratio              0.89 (0.86, 0.93)
---------------------------------------------------------
'''


#Resíduos ---------------------------------------------------------------------

#plot(mf2$deviance.residuals)
#plot(residuals(mf2))
#plot(mf2)
#deviance(mf2)

mres1 = matrix(mf$deviance.residuals, 3,10393)

#probabilidade predita - Oxitona
pred_oxitona = p[,1]
res_oxitona = t(mres1)[,1]
plot(res_oxitona~pred_oxitona, xlab = "Probabilidade Predita - Oxítona", ylab = "Resíduos Deviance - Oxítona")

#probabilidade predita - Paroxitona
pred_paroxitona = p[,2]
res_paroxitona = t(mres1)[,2]
plot(res_paroxitona~pred_paroxitona, xlab = "Probabilidade Predita - Paroxítona", ylab = "Resíduos Deviance - Paroxítona")

#probabilidade predita - Proparoxitona
pred_proparoxitona = p[,3]
res_proparoxitona = t(mres1)[,3]
plot(res_proparoxitona~pred_proparoxitona, xlab = "Probabilidade Predita - Proparoxítona", ylab = "Resíduos Deviance - Proparoxítona")


# qqplot resíduos
qqnorm(mf$working.residuals, col ="red", xlab = "Quantiles Teóricos", ylab = "Resíduos do Modelo")
qqline(mf$working.residuals)

#Plotando efeitos Aleátorios
qqnorm(mf$random.effects[[1]], xlab = "Quantiles Teóricos", ylab = "Efeitos Aleatórios Preditos", main = "")
qqline(mf$random.effects[[1]], col ="red")

