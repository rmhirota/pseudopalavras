#Lendo o resultado do modelo final, retiradas as não validadas e usando similaridade e frequência ------------
mf2 = readr::read_rds("data-raw/m_val_sim_mf2.rds")


#Nos dá as probabilidades (p1,p2,p3) por linha da base --------------------------
p = predict(mf2, type="response", conditional = TRUE)
p = as.data.frame(p)
p

#Queremos ver se a maior probabilidade coincide com a tonicidade alvo --------------------------

comp = p%>%mutate(ind = row_number())%>%
  tidyr::pivot_longer(cols = paroxítona:proparoxítona)%>%
  group_by(ind)%>%
  slice(which.max(value))%>%
  pull(name)%>%
  mutate(p, max = .)

base_sem_naovalidadas$tonicidade_producao = relevel(base_sem_naovalidadas$tonicidade_producao, ref = "oxítona")
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


