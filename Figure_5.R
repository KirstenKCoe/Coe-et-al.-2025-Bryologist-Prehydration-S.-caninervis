# Fig 5
# load packages
library(tidyverse)
library(ggh4x)

# load data
pr_l <- read.csv("prehy_regen_means_longer.csv", stringsAsFactors = T)

# re-order x axis so control is first
pr_l$treatment <- factor(pr_l$treatment, levels=unique(pr_l$treatment))

d1.color <- "#F9C205" # day 1
d21.color <- "#A7C9EC" # day 21

axis_labeller <- function(labels) {
  # 'labels' is a data.frame with one row per facet
  labels$measurement <- c(
    first_regen_day = "Day",
    regen_pts_d21 = "Count"
  )[labels$measurement]
  return(labels)
}

# Add x and y positions for the labels in dat_text
dat_text <- data.frame(
  label = c("Day of first regeneration", "Number of regeneration points at day 21"),
  measurement = c("first_regen_day", "regen_pts_d21"),
  x = c(7.5, 7.5),  # Adjust x as needed
  y = c(13.5, 5.5)  # Adjust y for proper positioning based on your data
)

fig5 <- ggplot(pr_l,aes(x = treatment, y = y_value, color = measurement)) + 
  geom_line(aes(group = measurement)) +
  scale_color_manual(values=c(first_regen_day = d1.color, regen_pts_d21 = d21.color)) +
  scale_fill_manual(values=c(first_regen_day = d1.color, regen_pts_d21 = d21.color)) +
  
  geom_errorbar(aes(ymin = y_value - se, ymax = y_value + se), colour = 'gray40', width = 0.4) +
  geom_point(aes(fill = measurement), size=3, shape = 21, colour = "black") +
  theme_light(base_size = 11) +  # optional: adjust base font size
  theme(
    panel.border = element_blank(),             # removes the box
    axis.line = element_line(color = "black"),  # adds black x and y axis lines
    axis.text.y = element_text(size = 10),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    strip.text.y.left = element_text(size = 11,
                                     color = "black",
                                     angle = 90,
                                     margin = margin(r = 1)),
    panel.spacing = unit(4, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8)),  # t = top margin (i.e. space above the title)
    strip.switch.pad.wrap = unit(-2.5, "lines")
  ) +

  ylab(NULL) +
  labs(x = "Length of prehydration treatment (hours or days)") +

# Add geom_text and facets to plot 
  facet_grid2(
    rows = vars(measurement),
    switch = "y",
    scales = "free",
    labeller = axis_labeller,
    independent = "x"  ) +
geom_text(data = dat_text, aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, hjust = 0.5, fontface = "bold.italic", size = 4)

fig5

## save as hi rez
ggsave(
  "Figure_5.pdf",
  plot = fig5,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

