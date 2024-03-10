library(tidyverse)

dados <- readr::read_csv("data/covid//2bd9969d-8a24-4661-9fce-9926d754cc50.csv") |> 
  dplyr::mutate(
    regiao = dplyr::case_when(
      UF %in% c("PR", "SC", "RS") ~ "Sul",
      UF %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      UF %in% c("GO", "MS", "MT", "DF") ~ "Centro-Oeste",
      UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    ),
    data = dplyr::case_when(
      Ano_Semana == "1/2024" ~ dmy("06/01/2024"),
      Ano_Semana == "2/2024" ~ dmy("13/01/2024"),
      Ano_Semana == "3/2024" ~ dmy("20/01/2024"),
      Ano_Semana == "4/2024" ~ dmy("27/01/2024"),
      Ano_Semana == "5/2024" ~ dmy("03/02/2024"),
      Ano_Semana == "6/2024" ~ dmy("10/02/2024"),
      Ano_Semana == "7/2024" ~ dmy("17/02/2024")
    )
  ) |> 
  dplyr::group_by(regiao, data) |> 
  dplyr::summarise(total_casos = sum(CasosNovos))

dados |> 
  ggplot() + 
  aes(x = data, y = total_casos, color = regiao) + 
  geom_line(size = 0.5) + 
  geom_point(aes(shape = regiao)) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#D5D2CB"),
        panel.grid.minor.y = element_line(colour = "#E2E1DC"),
        panel.border = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = "#F5F1E5"),
        panel.background = element_rect(fill = "#F5F1E5"),
        legend.box.background = element_rect(linewidth = 0.4, colour = "#D5D3CF"),
        legend.background = element_rect(fill = "#F5F1E5"),
        legend.key = element_rect(fill = "#F5F1E5"),
        ) + 
  labs(y = "Número de Casos", x = "Semana", 
       title = "Número de Casos Novos de Covid-19 por Região do Brasil em 2024",
       subtitle = "Por semana epidemiológica de notificação") + 
  scale_x_date(breaks = unique(dados$data), date_labels = "%d %b") + 
  scale_y_continuous(breaks = seq(0, 40000, 10000)) + 
  ylim(0, 40000) + 
  scale_color_brewer(type = "qual", palette = 6) + 
  scale_shape_manual(values=c(15, 8, 17, 18, 19))





