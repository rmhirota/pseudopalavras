library(mclogit)

m = mblogit(as.factor(tonicidade_producao)~as.factor(tonicidade_alvo)+as.factor(estrutura_palavra)+as.factor(grupo), asrandom = ~1|informante,data=pseudopalavras::dados)
