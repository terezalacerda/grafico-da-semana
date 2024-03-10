library(tidyverse)

readr::read_csv("data/gato_cachorro/multiTimeline.csv", skip = 1) |>
  set_names(c("data", "cachorro", "gato")) |> 
  filter(data < dmy("05/02/2024")) |> 
  pivot_longer(-data) |> 
  ggplot(aes(x = data, y = value, color = name)) +
  geom_line(size = 0.7) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  ) + 
  labs(color = "Série", 
       title = "Interesse ao longo do tempo",
       subtitle = 'Buscas por "cachorro" e "gato" no dia 4 de fevereiro de 2024'
  ) + 
  ylim(0, 100) + 
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "3 hours") + 
  scale_color_manual(values = c("#c0042c", "#4884a4"))
