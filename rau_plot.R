# 1. Load packages --------------------------------------------------------
pacman::p_load(haven, tidyverse, ggsci, "gganimate", "transformr", "gifski", rvest, readxl)
theme_set(theme_minimal())

data <- read.csv2("~/Downloads/Cuadro_13042024210421.csv")

# Convert Date column to Date type
data$Date <- as.Date(data$Date)

# Create the plot
p <- ggplot(data, aes(x = Date, y = Uncertainty)) +
  geom_line(color = "navy", size = 2) +
  geom_point(color = "navy", size = 3) +
  theme_classic() +
  labs(x = NULL, y = "Uncertainty") +
  transition_reveal(Date) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") + # Format month as number 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        text =element_text(size = 12))

# Render the animation
animate(p, fps = 6) # fps ajusta que tan rapido se ve

anim_save("rau_graph.gif", fps = 8,
          animation = p, renderer = gifski_renderer(),
          width = 800, height = 600)
