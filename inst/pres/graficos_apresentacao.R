#fluxo

library(ggalluvial)
g_f=pseudopalavras::dados %>%
  dplyr::mutate(Coincidente = tonicidade_producao == tonicidade_alvo) %>%
  dplyr::count(grupo, tonicidade_producao, tonicidade_alvo, Coincidente) %>%
  #dplyr::filter(grupo == 1) %>%
  ggplot2::ggplot(ggplot2::aes(
    axis1 = tonicidade_alvo, axis2 = tonicidade_producao, y = n
  )) +
  geom_alluvium(ggplot2::aes(fill = tonicidade_alvo)) +
  geom_stratum() +
  ggplot2::geom_text(
    stat = "stratum",
    ggplot2::aes(label = after_stat(stratum))
  ) +
  labs(fill="Tonicidade Alvo", y="N", x="Grupo")+
  ggplot2::scale_fill_viridis_d()+
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~grupo)+
  theme(plot.background = element_rect(fill = '#fffaf5ff', color = "#fffaf5ff"))

ggplot2::ggsave("inst/book/assets/graficos/g_fluxo_apresentacao.jpeg", g_f, width=10, height = 8)

#pizza
g_pizza=pseudopalavras::dados %>%
  dplyr::mutate(silaba_modificada = dplyr::case_when(
    silaba_modificada == 1 ~ "Primeira sílaba modificada",
    silaba_modificada == 2 ~ "Segunda sílaba modificada",
    silaba_modificada == 3 ~ "Terceira sílaba modificada",
    silaba_modificada == 0 ~ "Mais de duas sílabas modificadas",
  )) %>%
  dplyr::count(silaba_modificada) %>%
  dplyr::mutate(percentual = scales::percent(n/sum(n))) %>%
  ggplot(aes(x ="", y=n, fill=silaba_modificada),label=n) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_text())+
  geom_text(aes(x ="", y=n, label = percentual),
            position = position_stack(vjust = 0.5)) +
  labs(fill="Sílaba Modificada")+
  ggplot2::scale_fill_manual(values=c("turquoise4","mediumturquoise","orchid4","khaki" ))+
  theme(plot.background = element_rect(fill = '#fffaf5ff', color = "#fffaf5ff"))


ggplot2::ggsave("inst/book/assets/graficos/g_pizza.jpeg", g_pizza, width=13, height = 10)


#linguas

linguas=pessoas %>%
  dplyr::mutate(linguas = ifelse(linguas == 0, "Não", "Sim")) %>%
  dplyr::count(area_formacao, linguas, sort = TRUE) %>%
  dplyr::group_by(linguas) %>%
  dplyr::mutate(pct = scales::percent(n/sum(n), .1)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = linguas, y = n, fill = area_formacao,
    label = glue::glue("{n} ({pct})")
  )) +
  ggplot2::geom_col() +
  ggplot2::geom_text(position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::theme_minimal(15) +
  ggplot2::labs(
    x = "Conhecimento de outra(s) língua(s)",
    y = "N",
    fill = "Área de formação")+
  ggplot2::scale_fill_manual(values=c("turquoise4","orchid4" ))+
  theme(plot.background = element_rect(fill = '#fffaf5ff', color = "#fffaf5ff"))





ggplot2::ggsave("inst/book/assets/graficos/g_linguas_app.jpeg", linguas, width=8, height = 6)


#validacao
val = pseudopalavras::dados %>%
  dplyr::distinct(pseudopalavra, .keep_all = TRUE)%>%
  dplyr::count(grupo, validacao,name='qtd') %>%
  # dplyr::filter(grupo == 1) %>%
  dplyr::mutate(validacao = dplyr::case_when(
    validacao == "n" ~ "Não validada",
    validacao == "q" ~ "Quase validada",
    validacao == "s" ~ "Validada"
  )) %>%
  dplyr::group_by(grupo) %>%
  dplyr::mutate(pct = scales::percent(qtd/sum(qtd), .1)) %>%
  ggplot2::ggplot(ggplot2::aes(y = qtd, x = grupo, fill = validacao,
                               label = glue::glue("{qtd} ({pct})"))) +
  ggplot2::geom_col() +
  ggplot2::geom_text(position = ggplot2::position_stack(vjust = .5)) +
  ggplot2::scale_fill_manual(values=c("turquoise4","khaki","orchid4" )) +
  labs(x="Grupo de Classificação", y= "N", fill = "Validação")+
  ggplot2::theme_minimal(15)+
  theme(plot.background = element_rect(fill = '#fffaf5ff', color = "#fffaf5ff"))



ggplot2::ggsave("inst/book/assets/graficos/g_validacao_app.jpeg", val, width=8, height = 6)


