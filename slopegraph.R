pacman::p_load(tidyverse, lubridate, DT, readxl, colortools,
               forcats, scales, ggalt, ggrepel, kableExtra, CGPfunctions)

# O slopegraph usa o geom_line e o geom_point. Podemos fazê-lo usando o ggplot.
# Cada bolinha representa um tempo, no nosso caso jan, fev e mar.
# As bolinhas são ligadas, agrupadas por linhas, no caso Aécio, Lula, Bolsonaro e Ciro.
# A altura das bolinhas no slope são a intensão de voto de cada um em cada mês.

slope <- tibble(
  nome = rep(c("Aécio", "Lula", "Bolsonaro", "Ciro"), 3),
  mes = rep(c("janeiro", "fevereiro", "março"), each = 4),
  inten_voto = c(25,35,10,30, 40,10,15,35, 15,15,25,45))%>%
  mutate(mes = fct_relevel(mes, c("janeiro", "fevereiro", "março")))
  
  
ggplot(slope, aes(x = mes, y = inten_voto, group = nome)) +
  geom_line(aes(color = nome), alpha = 0.8, size = 4) +
  geom_point(aes(color = nome), alpha = 0.8, size = 5.3) +
  # Colocando geom_text_repel
  geom_text_repel(data = slope %>% filter(mes == "janeiro"), 
                  aes(label = paste0(nome, " ", inten_voto, "%")) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 4, 
                  nudge_x = -.45, 
                  direction = "y") +
  geom_text(data = slope %>% filter(mes == "fevereiro"), 
                  aes(label = paste0(inten_voto, "%")) , 
                  vjust = -1, 
                  fontface = "bold", 
                  size = 4) +
  geom_text_repel(data = slope %>% filter(mes == "março"), 
                  aes(label = paste0(nome, " ", inten_voto, "%")) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 4, 
                  nudge_x = .5, 
                  direction = "y") +
  #  Labelling as desired
  labs(title = "Intenções de voto para presidente em janeiro, fevereiro e março de 2018.",
    subtitle = "",
    caption = "fonte: Dados simulados para estudo do slopegraph.") +
    scale_x_discrete(position = "top") +
  theme_bw() +
    # Format tweaks
    # Remove the legend
    theme(legend.position = "none") +
    # Remove the panel border
    theme(panel.border = element_blank()) +
    # Remove just about everything from the y axis
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.y      = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    # Remove a few things from the x axis and increase font size
    theme(axis.title.x     = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(axis.text.x.top      = element_text(size=12)) +
    # Remove x & y tick marks
    theme(axis.ticks       = element_blank()) +
    # Format title & subtitle
    theme(plot.title       = element_text(size=25, face = "bold", hjust = 0.5)) +
    theme(plot.subtitle    = element_text(hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#BFD5E3"),
        panel.background = element_rect(fill = "cornsilk"))


# Também pode fazer usando a função newggslopegraph do CGP

newggslopegraph(slope, mes, inten_voto, nome, 
                Title = "Intenções de voto de janeiro a fevereiro de 2018.", 
                SubTitle = "", 
                Caption = "fonte: Dados simulados para estudo do slopegraph.",
                XTextSize = 14, #os meses
                YTextSize = 8, #os candidatos
                TitleTextSize = 25, SubTitleTextSize = 10, CaptionTextSize = 10,
                LineThickness = 6, LineColor = "ByGroup", DataTextSize = 6,
                WiderLabels = FALSE, RemoveMissing = TRUE) +
  theme(plot.background = element_rect(fill = "#BFD5E3"))
