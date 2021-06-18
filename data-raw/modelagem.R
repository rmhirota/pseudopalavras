#library(mclogit) #unico que funcionou
#library(mlogit)
#library(lme4)
#library(mixcat)
library(gamlss) #ESCOLHIDO
#library(nnet)
library(multgee)


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


#------------------------------------------


#gamlss

mg = gamlss(formula = tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+silaba_modificada+aleatorizacao+bloco_apresentacao+
              musica+linguas+genero+area_formacao+escolaridade+idade+re(random=~1|informante),
            data=base,control = gamlss.control(n.cyc = 30), family = gamlss.dist::MN3())



summary(mg)
getSmo(mg)

obj=str(mg)

stepGAIC(mg) #AIC

stepGAIC(mg, k=log(12511)) #BIC - ainda tem duvida

readr::write_rds(mg, "data-raw/modelo_aic.rds") #salva a saida em um objeto

mf = gamlss(formula = tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+silaba_modificada+aleatorizacao+bloco_apresentacao+
              re(random=~1|informante),
            data=base,control = gamlss.control(n.cyc = 30), family = gamlss.dist::MN3())

summary(mf)


mf2 = gamlss(formula = as.character(tonicidade_producao) ~ tonicidade_alvo+estrutura_palavra+
              grupo+segmento_modificado+silaba_modificada+aleatorizacao+bloco_apresentacao+
              re(random=~1|informante), data=base,control = gamlss.control(n.cyc = 30),
             family = gamlss.dist::MULTIN(type = "3"),
)

summary(mf2)

str(mf)
#usamos residuo do modelo ou residuo do parametro aleatorio?

plot(mf) #graficos dos residuos
plot(ranef(getSmo(mf))) #grafico dos efeitos - não
vcov(mg) #matriz de covariancia dos betas estimados
qqnorm(mf$mu.coefSmo[[1]]$coefficients$random[[1]])
qqline(mf$mu.coefSmo[[1]]$coefficients$random[[1]], col = "steelblue", lwd = 2)

wp(mf) #grafico de residuo do gilberto/patriota


summary(getSmo(mg)) # summary
ranef(getSmo(mg)) # random effect estimates
coef(getSmo(mg)) # fitted coefficients
intervals(getSmo(mg)) # Confidence intervals


