# Fig 4
# load packages
library(tidyverse)

# load data
via_grn <- read.csv("data/prehy.csv", stringsAsFactors = T)

via_grn <- via_grn %>%
  mutate(
    treatment_clean = case_when(
      treatment == "Control"        ~ "0 h",
      str_detect(treatment, "h prehy") ~ str_replace(treatment, " h prehy", " h"),
      str_detect(treatment, "d prehy") ~ str_replace(treatment, " d prehy", " d"),
      TRUE ~ treatment  # fallback just in case
    )
  )

via_grn$treatment_clean <- sub("24 h", "1 d", via_grn$treatment_clean)

# re-order x axis so control is first
via_grn$treatment_clean <- factor(via_grn$treatment_clean, levels=unique(via_grn$treatment_clean))


grn.color <- "#F9C205" # green leaves
via.color <- "#A7C9EC" # viability

# Create viability: TRUE if first_regen_d is not NA
via_grn <- via_grn %>%
  mutate(viable = !is.na(first_regen_d))

# Summarize green leaves
green_summary <- via_grn %>%
  group_by(treatment_clean) %>%
  summarise(
    y_value = mean(green_lvs_7daypost, na.rm = TRUE),
    se = sd(green_lvs_7daypost, na.rm = TRUE) / sqrt(sum(!is.na(green_lvs_7daypost))),
    measurement = "green_leaves_d7"
  )


# Summarize viability
viability_summary <- via_grn %>%
  group_by(treatment_clean) %>%
  summarise(
    y_value = 100 * mean(viable),
    se = 100 * sd(viable) / sqrt(n()),
    measurement = "viability"
  )

# Combine into one tidy long dataframe
long_via_grn <- bind_rows(viability_summary, green_summary)

long_via_grn <- long_via_grn %>%
  mutate(
    facet_order = factor(measurement,
                         levels = c("viability", "green_leaves_d7"),
                         labels = c("B_viability", "A_green_leaves"))
  )




# Axis labeller
axis_labeller_fig4 <- function(labels) {
  labels$facet_order <- c(
    A_green_leaves = "Green leaves (%)",
    B_viability = "Regenerating shoots (%)"
  )[labels$facet_order]
  return(labels)
}


# Panel text labels (adjust y as needed)
dat_text_fig4 <- data.frame(
  label = c("Green leaves at day 7", "Viability at day 21"),
  facet_order = c("A_green_leaves", "B_viability"),
  measurement = c("green_leaves_d7", "viability"),
  x = c(7.5, 7.5),
  y = c(55, 106)
)



# Plot
fig4 <- ggplot(long_via_grn, aes(x = treatment_clean, y = y_value, color = measurement)) +
  scale_fill_manual(values = c(
    viability = via.color,
    green_leaves_d7 = grn.color
  )) +
  geom_errorbar(aes(ymin = y_value - se, ymax = y_value + se),
                colour = 'gray40', width = 0.3) +
  geom_point(aes(fill = measurement), size = 3, shape = 21, colour = "black") +
  theme_light(base_size = 11) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(size = 10),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    strip.text.y.left = element_text(
      size = 9, color = "black", angle = 90,
      margin = margin(r = 1)
    ),
    panel.spacing = unit(4, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8)),
    strip.switch.pad.wrap = unit(-2.5, "lines")
  ) +
  ylab(NULL) +
  labs(x = "Length of prehydration treatment (hours or days)") +
  facet_grid2(
    rows = vars(facet_order),
    switch = "y",
    scales = "free",
    labeller = axis_labeller_fig4,
    independent = "x"
  ) +
  geom_text(data = dat_text_fig4,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = 0.5,
            fontface = "bold.italic",
            size = 4) + 
  coord_cartesian(clip = "off") +
  
  # Add A and B to panels
  geom_text(
    data = data.frame(
      label = c("A", "B"),
      measurement = c("green_leaves_d7", "viability"),
      facet_order = c("A_green_leaves", "B_viability"),
      x = c(-Inf, -Inf),
      y = c(Inf, Inf),
      hjust = c(2, 2),   
      vjust = c(0.5, 0)    
    ),
    aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold"
  )


fig4


ggsave(
  "figures/Figure_4.pdf",
  plot = fig4,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

