# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importando ----

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Tratando ----

dados <- dados |>
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

dados |> dplyr::glimpse()

dados

# Gráfico por unidades amostrais ----

## Tratando os dados ----

dados_trat <- dados |>
  dplyr::filter(Ordem == "Anura") |>
  dplyr::summarise(Abundancia = dplyr::n(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  dplyr::left_join(dados |>
                     dplyr::distinct(`Unidade Amostral`) |>
                     dplyr::mutate(Amostra = dplyr::row_number()),
                   by = c("Unidade Amostral"))

dados_trat

## Ordem das espécies ----

ordem_especies <- dados_trat |>
  dplyr::summarise(media = sum(Amostra * Abundancia) / sum(Abundancia),
                   .by = Espécie) |>
  dplyr::arrange(media |> dplyr::desc())

ordem_especies

## Ordem das unidades amostrais ----

ordem_amostras <- dados_trat |>
  dplyr::left_join(ordem_especies,
                   by = "Espécie") |>
  dplyr::summarise(MR = sum(media * Abundancia) / sum(Abundancia),
                   .by = Amostra) |>
  dplyr::arrange(MR) |>
  dplyr::left_join(dados_trat |>
                     dplyr::select(`Unidade Amostral`, Amostra),
                   by = "Amostra") |>
  dplyr::distinct()

ordem_amostras

## Segundo tratamento ----

dados_tratados <- dados_trat |>
  dplyr::mutate(`Unidade Amostral` = `Unidade Amostral` |>
                  forcats::fct_relevel(ordem_amostras$`Unidade Amostral`),
                Espécie = Espécie |>
                  forcats::fct_relevel(ordem_especies$Espécie))

dados_tratados

## Gráfico ----

dados_tratados |>
  ggplot(aes(`Unidade Amostral`, Abundancia, fill = Abundancia)) +
  geom_col(color = "black", width = 0.75) +
  geom_hline(yintercept = 0, color = "black") +
  facet_grid(Espécie ~ .) +
  scale_fill_viridis_c() +
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
        legend.position = "top",
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15))

ggsave(filename = "grafico_codigo_barras.png", height = 10, width = 12)
