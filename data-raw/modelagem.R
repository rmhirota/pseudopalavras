library(mclogit) #unico que funcionou
library(dplyr)
library(MASS)
library(ggplot2)
library(tidyr)
library(epiR)
library(pROC)
devtools::install()

base <- pseudopalavras::da_modelo_validadas

#Modelo Nulo
m0 = mblogit(tonicidade_producao ~ 1,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")

summary(m0)


#Modelo Completo
mt = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+bloco_apresentacao+
              aleatorizacao+musica+linguas+idade+genero+escolaridade+area_formacao,
            random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")

summary(mt)

#Modelo retirando Linguas - maior p valor nos dois modelos
m1 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+musica+idade+genero+escolaridade+area_formacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL" )
summary(m1)


#Modelo retirando Genero - maior p valor nos dois modelos
m2 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+musica+idade+escolaridade+area_formacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m2)

#Modelo retirando Musica - maior p valor nos dois modelos
m3 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+idade+escolaridade+area_formacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m3)

#Modelo retirando Area de formação - maior p valor nos dois modelos
m4 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+idade+escolaridade,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m4)

#Modelo retirando Idade - maior p valor nos dois modelos
m5 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+escolaridade,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m5)


getSummary.mblogit(m1)
mclogit:::summary.mblogit(m1)

#Comparando os AIC
mclogit:::AIC.mclogit(mt)
mclogit:::AIC.mclogit(m5)

#Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Escolaridade)
m6 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m6)

mclogit:::AIC.mclogit(m5)
mclogit:::AIC.mclogit(m6)

#Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Aleatorização)
m7 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m7)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m7)

#O AIC piorou tirando as variáveis com níveis não significantes, então mantemos e testamos a variável de indivíduo Linguas
m8 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+linguas,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30, method = "PQL")
summary(m8)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m8)

#Piorou, então definimos o modelo final:

# mf = m6
mf = mblogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra +
    grupo + segmento_modificado + bloco_apresentacao +
               aleatorizacao,
  random = ~1|informante,
  data=base_sem_naovalidadas,
  epsilon = 1e-08, maxit = 30,
  method = "PQL"
)
summary(mf)
readr::write_rds(mf, "data-raw/m_mf_grupo_val.rds")
mf = readr::read_rds("data-raw/m_mf_grupo_val.rds")

# qqplot efeitos aleatórios
qqnorm(mf$random.effects[[1]])
qqline(mf$random.effects[[1]], col ="red")


#mf$deviance.residuals: ver a expressão desse resíduo


# qqplot resíduos
plot(mclogit:::residuals.mclogit(mf)) #nao deu certo
DHARMa::simulateResiduals(mf) #não deu certo
qqnorm(mclogit:::residuals.mclogit(mf))
qqline(mclogit:::residuals.mclogit(mf), col ="red")




#fiz mas nao entendi KKK

roc = pROC::multiclass.roc(pseudopalavras::dados$tonicidade_producao, p, levels =
                       c("oxítona","paroxítona","proparoxítona"))
rs = roc[['rocs']]
pROC::plot.roc(rs$`paroxítona/oxítona`[[1]])
pROC::plot.roc(rs$`paroxítona/oxítona`[[2]])
pROC::plot.roc(rs$`paroxítona/proparoxítona`[[1]])
pROC::plot.roc(rs$`paroxítona/proparoxítona`[[2]])
pROC::plot.roc(rs$`oxítona/proparoxítona`[[1]])
pROC::plot.roc(rs$`oxítona/proparoxítona`[[2]])





#Resíduos

plot(mf$deviance.residuals)
plot(residuals(mf))
plot(mf)
deviance(mf)
