library(ggplot2)
library(tidyverse)
require(scales)

theme_set(theme_bw())

coords <- read.csv("./1000bodyCoordinatesIteration0.csv")

p <- ggplot(
  coords,
  aes(x = x, y = y, colour = as.factor(id))
  ) +
  ggtitle("Orb разбиване с 8 процесора") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  labs(x = "x", y = "y")

#add center split
p <- p + annotate("segment",
             x = 500000,
             y = 0,
             xend = 500000,
             yend = 1000000,
             size = 1.5, alpha = 0.7)

#add left split
p <- p + annotate("segment",
             x = 0,
             y = 600000,
             xend = 500000,
             yend = 600000,
             size = 1.5, alpha = 0.7)

#right split
p <- p + annotate("segment",
             x = 500000,
             y = 350000,
             xend = 1000000,
             yend = 350000,
             size = 1.5, alpha = 0.7)

#bottom right split
p <- p + annotate("segment",
             x = 750000,
             y = 0,
             xend = 750000,
             yend = 350000,
             size = 1.5, alpha = 0.7)

#top right split
p <- p + annotate("segment",
             x = 700000,
             y = 350000,
             xend = 700000,
             yend = 1000000,
             size = 1.5, alpha = 0.7)

#bottom left
p <- p + annotate("segment",
             x = 290000,
             y = 0,
             xend = 290000,
             yend = 600000,
             size = 1.5, alpha = 0.7)

p <- p + annotate("segment",
             x = 250000,
             y = 600000,
             xend = 250000,
             yend = 1000000,
             size = 1.5, alpha = 0.7)

ggsave("./orb.png")
