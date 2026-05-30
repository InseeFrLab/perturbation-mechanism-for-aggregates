# theme ggplot
library(ggplot2)

theme_set(theme_light(base_size = 16))
theme_update(
  axis.line = element_line(color = "grey65", linewidth = 0.5), 
  axis.text = element_text(color = "grey45", size = 10), 
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "#FFFFFF"),
  panel.grid = element_line(colour = "grey95", linewidth = 0.5),
  strip.background = element_rect(colour="grey95", fill="#FFFFFF"), 
  strip.text = element_text(colour = "black")
)
