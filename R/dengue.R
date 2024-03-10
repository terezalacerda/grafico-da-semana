library(ggplot2)

dados <- readr::read_rds("data/dengue/dados_dengue.rds") |> 
  dplyr::mutate(
    num_casos_mil = num_casos/1000,
    numero_habitantes_estado_cem_mil = 44040000/100000,
    num_casos_por_cem_mil_habitantes = num_casos/numero_habitantes_estado_cem_mil
  )

dados |> 
  ggplot() + 
  aes(x = data, y = num_casos_por_cem_mil_habitantes, color = tipo) + 
  geom_line(size = 0.5) + 
  geom_point(aes(shape = tipo)) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#D5D2CB"),
        panel.grid.minor.y = element_line(colour = "#E2E1DC"),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = "#F5F1E5"),
        panel.background = element_rect(fill = "#F5F1E5"),
        legend.box.background = element_rect(linewidth = 0.4, colour = "#D5D3CF"),
        legend.background = element_rect(fill = "#F5F1E5"),
        legend.key = element_rect(fill = "#F5F1E5"),
  ) + 
  labs(title = "Número de Casos de Dengue por 100 mil habitantes, no Estado de São Paulo",
       subtitle = "Casos Notificados e Confirmados, entre janeiro de 2022 e janeiro de 2024", 
       caption = "Dados obtidos em: http://www.saude.sp.gov.br",
       y = "Número de Casos por 100 mil habitantes") + 
  scale_x_date(breaks = unique(dados$data)[c(TRUE, FALSE, FALSE)], date_labels = "%b/%y") + 
  scale_y_continuous(breaks = seq(0, 450, 50)) + 
  scale_color_brewer(type = "qual", palette = 6) + 
  scale_shape_manual(values=c(17, 18)) + 
  geom_vline(xintercept = as.numeric(dados$data[c(1, 25, 49)]),
             linetype = 4, size = 0.2)

