# An optional custom script to run before Hugo builds your site.
# You can delete it if you do not need it.
library(ggplot2)

my_theme <- function(){
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black", size = 20),
    panel.grid = element_line(colour = "grey"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.box = "vertical",
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 14),
    text = element_text(size = 25),
    legend.key = element_rect(colour = "grey"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = "white")
  )
}
