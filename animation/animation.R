#I created an animation for my nbody using
#this tutorial:
#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
theme_set(theme_bw())
options(browser = "chromium")

coords <- read.csv("/home/nikolay/FMI/SPO/nbody/animation/coordinates.csv")
colnames(coords)

p <- ggplot(
  coords,
  aes(x = x, y = y, colour = as.factor(id))
  ) +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  labs(x = "x", y = "y")

ani <- p + transition_time(iteration) +
    labs(title = "iteration: {frame_time}");

animate(ani, duration = 10, fps = 24,
        renderer = gifski_renderer("hope.gif"), height = 1200, width = 1200)
