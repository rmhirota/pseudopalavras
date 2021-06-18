library(mclogit) #unico que funcionou
library(mlogit)
library(lme4)
library(mixcat)
library(gamlss)
library(nnet)

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

m = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+silaba_modificada+bloco_apresentacao+
              aleatorizacao+musica+linguas+genero+escolaridade+idade+
              area_formacao,random = ~1|informante,data=base)



# random = ~1|informante,
#/eb, subset=classd!="Farmers")
summary(m)


m2 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado+silaba_modificada+
               aleatorizacao+musica+linguas+genero+escolaridade+idade+
               area_formacao,random = ~1|informante,data=base)

summary(m2)



m3 = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
               grupo+segmento_modificado,
             random = ~1|informante,data=base)

summary(m3)


qqnorm(m$random.effects[[1]])
qqline(m$random.effects[[1]], col = "steelblue", lwd = 2)


qqnorm(m3$random.effects[[1]])
qqline(m3$random.effects[[1]], col = "purple", lwd = 2)


#------------------------------------------

#nnet

mn = multinom(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
                grupo+segmento_modificado,random = ~1|informante, base)
summary(mn)



#------------------------------------------

#gamlss

set.seed(1)

mg = gamlss(formula = tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+silaba_modificada+musica+linguas+re(random=~1|informante),
            data=base,control = gamlss.control(n.cyc = 30), family = gamlss.dist::MN3())



summary(mg)
getSmo(mg)

obj=str(mg)
qqnorm(mg$mu.coefSmo[[1]]$coefficients$random[[1]])
qqline(mg$mu.coefSmo[[1]]$coefficients$random[[1]], col = "steelblue", lwd = 2)

summary(getSmo(mg)) # summary
ranef(getSmo(mg)) # random effect estimates
coef(getSmo(mg)) # fitted coefficients
intervals(getSmo(mg)) # Confidence intervals


