library(mclogit) #unico que funcionou
library(dplyr)
library(MASS)
library(ggplot2)
library(DHARMa)
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


base$tonicidade_producao = relevel(base$tonicidade_producao, ref = "paroxítona")
base$escolaridade = relevel(base$escolaridade, ref = "Superior Incompleto")
base$aleatorizacao = relevel(base$aleatorizacao, ref = "s")

#Modelo Nulo
m0 = mblogit(tonicidade_producao ~ 1,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30)

summary(m0)


#Modelo Completo
mt = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+bloco_apresentacao+
              aleatorizacao+musica+linguas+idade+genero+escolaridade+area_formacao,
            random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30)

summary(mt)

#Modelo retirando Linguas - maior p valor nos dois modelos
m1 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+musica+idade+genero+escolaridade+area_formacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m1)


#Modelo retirando Genero - maior p valor nos dois modelos
m2 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+musica+idade+escolaridade+area_formacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m2)

#Modelo retirando Musica - maior p valor nos dois modelos
m3 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+idade+escolaridade+area_formacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m3)

#Modelo retirando Area de formação - maior p valor nos dois modelos
m4 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+idade+escolaridade,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m4)

#Modelo retirando Idade - maior p valor nos dois modelos
m5 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao+escolaridade,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
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
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m6)

mclogit:::AIC.mclogit(m5)
mclogit:::AIC.mclogit(m6)

#Vendo o quanto o AIC é afetado ao tirar uma variável com apenas um nível significante (Aleatorização)
m7 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m7)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m7)

#O AIC piorou tirando as variáveis com níveis não significantes, então mantemos e testamos a variável de indivíduo Linguas
m8 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+linguas,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(m8)

mclogit:::AIC.mclogit(m6)
mclogit:::AIC.mclogit(m8)

#Piorou, então definimos o modelo final:

mf = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+bloco_apresentacao+
               aleatorizacao,
             random = ~1|informante,data=base, epsilon = 1e-08,maxit = 30 )
summary(mf)


qqnorm(mf$random.effects[[1]])
qqline(mf$random.effects[[1]], color ="red")

residuals.mclogit() #nao deu certo
DHARMa::simulateResiduals(mf) #não deu certo
mclogit:::residuals.mclogit(mf)

predict(mf, type="response")


#------------------------Apenas palavras validadas

base = pseudopalavras::dados %>%       #10.393
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
  linguas = as.factor(linguas))%>%select(
    -c(vizinhanca_tonicidade, vizinhanca_fonologica))

base$tonicidade_producao = relevel(base$tonicidade_producao, ref = "paroxítona")

m_val = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+silaba_modificada+bloco_apresentacao+
              aleatorizacao+musica+linguas+genero+escolaridade+idade+
              area_formacao,random = ~1|informante,data=base)
summary(m_val)
m_val2 = MASS::stepAIC(mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
          grupo+segmento_modificado+silaba_modificada+bloco_apresentacao+
          aleatorizacao+musica+linguas+genero+escolaridade+idade+
          area_formacao,random = ~1|informante,data=base))
