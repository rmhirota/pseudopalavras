library(mclogit) #unico que funcionou
library(mlogit)
library(lme4)
library(mixcat)

base = pseudopalavras::dados%>%mutate(tonicidade_producao = as.factor(tonicidade_producao),
                                      tonicidade_alvo = as.factor(tonicidade_alvo),
                                      estrutura_palavra = as.factor(estrutura_palavra),
                                      grupo = as.factor(grupo),
                                      musica = as.factor(musica))



m = mblogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo +musica+linguas,random = ~1|informante,data=base)

# random = ~1|informante,
#/eb, subset=classd!="Farmers")
summary(m)

'''
da <- pseudopalavras::dados %>%
  dplyr::transmute(
    informante,
    pseudopalavra,
    tonicidade_producao = as.factor(tonicidade_producao),
    tonicidade_alvo = as.factor(tonicidade_alvo),
    estrutura_palavra = as.factor(estrutura_palavra)
  )
da_mlogit <- dfidx::dfidx(da, shape = "long", idx = c("informante", "pseudopalavra"))
fit_mlogit <- mlogit::mlogit(
  tonicidade_producao ~ tonicidade_alvo + estrutura_palavra | informante,
  data = da_mlogit
)
summary(fit_mlogit)
'''

m2 = mlogit(tonicidade_producao ~ tonicidade_alvo+estrutura_palavra+
              grupo +musica|informante,rpar = c(musica='n'),correlation = T, data = base,
            reflevel = "paroxítona")
summary(m2)




m3 = glmer(tonicidade_producao~ tonicidade_alvo+estrutura_palavra+
             grupo+ (1|informante), data=base, family = binomial)

summary(m3)


m4 = npmlt(base$tonicidade_producao ~ base$tonicidade_alvo+base$estrutura_palavra+
             base$grupo, random = ~1+base$informante)

summary(m4)



