#Lendo o resultado do modelo final, retiradas as não validadas e usando similaridade e frequência ------------
mf2 = readr::read_rds("data-raw/m_val_sim_mf2.rds")


#Nos dá as probabilidades (p1,p2,p3) por linha da base --------------------------
p <- predict(mf2, type = "response", conditional = TRUE) %>%
  tibble::as_tibble()
p %>% dplyr::slice(100:110)

#Queremos ver se a maior probabilidade coincide com a tonicidade alvo --------------------------

comp = p%>%mutate(ind = row_number())%>%
  tidyr::pivot_longer(cols = paroxítona:proparoxítona)%>%
  group_by(ind)%>%
  slice(which.max(value))%>%
  pull(name)%>%
  mutate(p, max = .)



data.frame(rm = comp$max, re = base_sem_naovalidadas$tonicidade_producao)

#Nos dá o comparativo entre a variável resposta e a resposta que o modelo daria -------------------
t = table(comp$max,base_sem_naovalidadas$tonicidade_producao)
t

as.data.frame(comp$max)%>%group_by(`comp$max`)%>%count()

as.data.frame(base_sem_naovalidadas$tonicidade_producao)%>%group_by(`base_sem_naovalidadas$tonicidade_producao`)%>%count()


#Fazendo sensibilidade e blablabla para oxitona --------------------------
to = data.frame(m.oxitona = c(3955,697,4652), m.resto = c(614,5127,5741))
rownames(to) = c("Sim", "Não", "Total")
epi.tests(t(to), conf.level = 0.95)


#Fazendo sensibilidade e blablabla para paroxitona --------------------------
tp = data.frame(m.paroxitona = c(4832,847,5679), m.resto = c(604,4110,4714))
rownames(tp) = c("Sim", "Não", "Total")
epi.tests(t(tp), conf.level = 0.95)



#Fazendo sensibilidade e blablabla para proparoxitona --------------------------
tpr = data.frame(m.proparoxitona = c(42,20,62), m.resto = c(346,9985,10331))
rownames(tpr) = c("Sim", "Não", "Total")
epi.tests(t(tpr), conf.level = 0.95)


#Resíduos ---------------------------------------------------------------------

#plot(mf2$deviance.residuals)
#plot(residuals(mf2))
#plot(mf2)
#deviance(mf2)

mres = matrix(mf2$deviance.residuals, 3,10393) #residuos deviance soltados pelo modelo
mres1 = matrix(residuals(mf2, type = "deviance"),3,10393) #funcao do R com tipo deviance


rs <- matrix(mf2$deviance.residuals, 3,10393) %>%
  t() %>%
  tibble::as_tibble() %>%
  purrr::set_names("paroxitona", "oxitona", "proparoxitona")
rs %>% dplyr::slice(100:110)

plot_residuos <- function(tonicidade, tonicidade_pred) {
  p %>%
    purrr::set_names("p_par", "p_oxi", "p_pro") %>%
    dplyr::bind_cols(rs) %>%
    dplyr::filter({{tonicidade}} != 0) %>%
    ggplot2::ggplot(ggplot2::aes(y = {{tonicidade}}, x = {{tonicidade_pred}})) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Predito", y = "Resíduo",
      title = rlang::expr({{tonicidade}}),
      subtitle = "Valores preditos para pi e resíduos"
    )
}
plot_residuos(paroxitona, p_par)
plot_residuos(oxitona, p_oxi)
plot_residuos(proparoxitona, p_pro)



# qqplot resíduos
qqnorm(mf2$working.residuals, col ="red", xlab = "Quantiles Teóricos", ylab = "Resíduos do Modelo")
qqline(mf2$working.residuals)

#Plotando efeitos Aleátorios
qqnorm(mf2$random.effects[[1]], xlab = "Quantiles Teóricos", ylab = "Efeitos Aleatórios Preditos")
qqline(mf2$random.effects[[1]], col ="red")

#plotar as respotas, colorir por individuo e colocar no y a p de cada linha

prob = p%>%mutate(ind = row_number())%>%
  tidyr::pivot_longer(cols = paroxítona:proparoxítona)%>%
  group_by(ind)%>%
  slice(which.max(value))%>%
  pull(value)%>%
  mutate(p, max = .)


graf = data.frame(p.modelo = prob$max,r.data = base_sem_naovalidadas$tonicidade_producao,
                  informante = base_sem_naovalidadas$informante, r.modelo = comp$max )

ggplot(graf, aes(x=r.data, y=p.modelo)) +
  geom_point(aes(color = informante, shape = r.modelo),dotsize = 0.5, binaxis='y',
               position=position_dodge(0.8))



