# Pacotes ----

library(ecodados)

library(tidyverse)

# Dados ----

## Importando ----

comp <- ecodados::composicao_aves_filogenetica

## Visualizando ----

comp

## Tratando ----

### Ordem das espécies ----

ordem_especies <- comp %>%
  tibble::rownames_to_column() %>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Espécie",
                      values_to = "Abundância") %>%
  dplyr::rename("Amostra" = rowname) %>%
  dplyr::mutate(Espécie = Espécie %>% stringr::str_replace("_", " "),
                Amostra = Amostra %>% readr::parse_number()) %>%
  dplyr::summarise(media = sum(Amostra * Abundância) / sum(Abundância),
                   .by = Espécie) %>%
  dplyr::arrange(media %>% dplyr::desc())

ordem_especies

### Ordem das parcelas ----

ordem_amostras <- comp %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("Amostra" = rowname) %>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Espécie",
                      values_to = "Abundância") %>%
  dplyr::mutate(Espécie = Espécie %>%
                  stringr::str_replace("_", " "),
                Amostra = Amostra %>%
                  readr::parse_number() %>%
                  as.character()) %>%
  dplyr::left_join(ordem_especies,
                   by = "Espécie") %>%
  dplyr::summarise(MR = sum(media * Abundância) / sum(Abundância),
                   .by = Amostra) %>%
  dplyr::arrange(MR) %>%
  dplyr::distinct()

ordem_amostras

### Tratando ----

comp_trat <- comp %>%
  tibble::rownames_to_column() %>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Espécie",
                      values_to = "Abundância")  %>%
  dplyr::rename("Amostra" = rowname) %>%
  dplyr::mutate(Espécie = Espécie %>% stringr::str_replace("_", " "),
                Espécie = Espécie %>% forcats::fct_relevel(ordem_especies$Espécie),
                Amostra = Amostra %>% readr::parse_number() %>% as.character(),
                Amostra = Amostra %>% forcats::fct_relevel(ordem_amostras$Amostra))

comp_trat

# Gráfico ----

## Criando ----

comp_trat %>%
  ggplot(aes(Amostra, Abundância, fill = Abundância)) +
  geom_col(color = "black", width = 0.75) +
  geom_hline(yintercept = 0, color = "black") +
  facet_grid(Espécie ~ .) +
  scale_fill_viridis_c(breaks = seq(0, 18, 1),
                       option = "turbo") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 25,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, hjust = 0),
        axis.title = element_text(color = "black", size = 15),
        panel.spacing = unit(0, "points"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text.y.right = element_text(angle = 0,
                                          size = 12,
                                          color = "black",
                                          hjust = 0,
                                          face = "italic"),
        legend.position = "bottom",
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15))

## Salvando ----

ggsave(filename = "grafico_codigo_barra.png", height = 12, width = 10)

comp_trat %>%
  dplyr::mutate(Espécie = Espécie %>% forcats::fct_relevel(ordem %>% rev())) %>%
  dplyr::filter(Abundância > 0) %>%
  ggplot(aes(Amostra, Espécie, fill = Abundância)) +
  geom_point(shape = 21, color = "black", aes(size = Abundância), show.legend = FALSE) +
  scale_fill_viridis_c(breaks = seq(1, 18, 1),
                       option = "turbo") +
  scale_y_discrete(position = "right") +
  labs(y = NULL,
       size = NULL) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 25,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, hjust = 0),
        axis.text.y = element_text(size = 12,
                                   face = "italic"),
        axis.title = element_text(color = "black", size = 15),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15))

ggsave(filename = "grafico_codigo_barra_2.png", height = 12, width = 10)


