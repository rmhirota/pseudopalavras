library(mclogit) #unico que funcionou
library(dplyr)
library(MASS)
library(ggplot2)
library(DHARMa)
library(tidyr)
library(epiR)
library(pROC)
devtools::install()



base = pseudopalavras::dados%>%mutate(
                                      informante=as.factor(informante),
                                      tonicidade_producao = as.factor(tonicidade_producao),
                                      tonicidade_alvo = as.factor(tonicidade_alvo),
                                      estrutura_palavra = as.factor(estrutura_palavra),
                                      grupo = as.factor(grupo),
                                      musica = as.factor(musica),
                                      aleatorizacao = as.factor(aleatorizacao),
                                      bloco_apresentacao = as.factor(bloco_apresentacao),
                                      vizinhanca_tonicidade = as.factor(vizinhanca_tonicidade),
                                      segmento_modificado = as.factor(segmento_modificado),
                                      silaba_modificada = as.factor(silaba_modificada),
                                      genero = as.factor(genero),
                                      escolaridade = as.factor(escolaridade),
                                      area_formacao = as.factor(area_formacao),
                                      linguas = as.factor(linguas))%>%select(
                                        -c(vizinhanca_tonicidade, vizinhanca_fonologica))

readr::write_rds(base, "data-raw/base_modelo.rds")


base$tonicidade_producao = relevel(base$tonicidade_producao, ref = "paroxítona")
base$escolaridade = relevel(base$escolaridade, ref = "Superior Incompleto")
base$aleatorizacao = relevel(base$aleatorizacao, ref = "s")

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
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra+
    grupo + segmento_modificado+bloco_apresentacao+
               aleatorizacao,
  random = ~1|informante,
  data=base,
  epsilon = 1e-08, maxit = 30,
  method = "PQL"
)
summary(mf)

# qqplot efeitos aleatórios
qqnorm(mf$random.effects[[1]])
qqline(mf$random.effects[[1]], col ="red")

# qqplot resíduos
residuals.mclogit() #nao deu certo
DHARMa::simulateResiduals(mf) #não deu certo
qqnorm(mclogit:::residuals.mclogit(mf))
qqline(mclogit:::residuals.mclogit(mf), col ="red")


#Nos dá as probabilidades (p1,p2,p3) por linha da base
p = predict(mf, type="response", conditional = TRUE)
p = as.data.frame(p)

#Queremos ver se a maior probabilidade coincide com a tonicidade alvo

comp = p%>%mutate(ind = row_number())%>%
  tidyr::pivot_longer(cols = paroxítona:proparoxítona)%>%
  group_by(ind)%>%
  slice(which.max(value))%>%
  pull(name)%>%
  mutate(p, max = .)


data.frame(rm = comp$max, re = pseudopalavras::dados$tonicidade_producao)%>%filter(rm== "proparoxítona")

#Nos dá o comparativo entre a variável resposta e a resposta que o modelo daria
t = table(comp$max,pseudopalavras::dados$tonicidade_producao)
epi.tests(t, conf.level = 0.95)
as.data.frame(comp$max)%>%group_by(`comp$max`)%>%count()

as.data.frame(pseudopalavras::dados$tonicidade_producao)%>%group_by(`pseudopalavras::dados$tonicidade_producao`)%>%count()



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








#------------------------Apenas palavras validadas

basev = pseudopalavras::dados %>%       #10.393
  filter(validacao == 's'| validacao == 'q' ) %>%
  mutate(
  informante=as.factor(informante),
  tonicidade_producao = as.factor(tonicidade_producao),
  tonicidade_alvo = as.factor(tonicidade_alvo),
  estrutura_palavra = as.factor(estrutura_palavra),
  grupo = as.factor(grupo),
  musica = as.factor(musica),
  aleatorizacao = as.factor(aleatorizacao),
  bloco_apresentacao = as.factor(bloco_apresentacao),
  vizinhanca_tonicidade = as.factor(vizinhanca_tonicidade),
  segmento_modificado = as.factor(segmento_modificado),
  silaba_modificada = as.factor(silaba_modificada),
  genero = as.factor(genero),
  escolaridade = as.factor(escolaridade),
  area_formacao = as.factor(area_formacao),
  linguas = as.factor(linguas)) %>% select(
  -c(vizinhanca_tonicidade, vizinhanca_fonologica )
  )

basev$tonicidade_producao = relevel(basev$tonicidade_producao, ref = "paroxítona")
basev$escolaridade = relevel(basev$escolaridade, ref = "Superior Incompleto")
basev$aleatorizacao = relevel(basev$aleatorizacao, ref = "s")

m_val = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+bloco_apresentacao+
              aleatorizacao,random = ~1|informante,data=basev)


summary(m_val)
m_val2 = MASS::stepAIC(mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
          grupo+segmento_modificado+silaba_modificada+bloco_apresentacao+
          aleatorizacao+musica+linguas+genero+escolaridade+idade+
          area_formacao,random = ~1|informante,data=basev))
