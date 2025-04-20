# Fig 3
# load packages
library(tidyverse)
library(ggh4x)

# load data
wc_rh <- read.csv("prehy_wc_rh.csv", stringsAsFactors = T)

wc_rh_long <- wc_rh %>%
  mutate(
    prehy_label = case_when(
      is.na(Prehy_time_h) ~ "Full\nturgor",
      Prehy_time_h >= 24 ~ paste0(Prehy_time_h / 24, " d"),
      TRUE ~ paste0(Prehy_time_h, " h")
    )
  )

wc_rh_long <- wc_rh_long %>%
  pivot_longer(cols = c(WC_pct, RH_pct),
               names_to = "measurement",
               values_to = "value") %>%
  mutate(
    measurement = recode(measurement,
                         WC_pct = "water_content",
                         RH_pct = "relative_humidity")
    )
  
rh.color <- "#F9C205" # relative humidity
wc.color <- "#A7C9EC" # water content

# re-order x axis so control is first
wc_rh_long$prehy_label <- factor(wc_rh_long$prehy_label, levels=unique(wc_rh_long$prehy_label))

wc_rh_summary <- wc_rh_long %>%
  group_by(prehy_label, measurement) %>%
  summarise(
    n = sum(!is.na(value)),
    y_value = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Facet strip labels
axis_labeller_rh_wc <- function(labels) {
  labels$measurement <- c(
    relative_humidity = "Relative Humidity (%)",
    water_content = "Water Content (%)"
  )[labels$measurement]
  return(labels)
}

# Positioning for facet text inside plot panels
dat_text_rh_wc <- data.frame(
  label = c("Relative humidity", "Water content"),
  measurement = c("relative_humidity", "water_content"),
  x = c(7.5, 7.5),   # adjust depending on your x-axis values
  y = c(101.5, 360)      # adjust based on expected y-axis ranges
)

p <- ggplot(wc_rh_summary, aes(x = prehy_label, y = y_value, color = measurement)) + 
  geom_line(aes(group = measurement)) +
  scale_color_manual(values = c(
    relative_humidity = rh.color,
    water_content = wc.color
  )) +
  scale_fill_manual(values = c(
    relative_humidity = rh.color,
    water_content = wc.color
  )) +
  geom_errorbar(aes(ymin = y_value - se, ymax = y_value + se), 
                width = 0.4, colour = 'gray40') +
  geom_point(aes(fill = measurement), size = 3, shape = 21, colour = "black") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  theme_light(base_size = 11) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(size = 10),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    strip.text.y.left = element_text(
      size = 10, color = "black", angle = 90, margin = margin(r = 1)
    ),
    panel.spacing = unit(4, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 0)),
    strip.switch.pad.wrap = unit(-2.5, "lines")
  ) +
  ylab(NULL) +
  labs(x = "Length of prehydration treatment (hours or days)") +
  facet_grid2(
    rows = vars(measurement),
    switch = "y",
    scales = "free",
    labeller = axis_labeller_rh_wc,
    independent = "x"
  ) +
  geom_text(data = dat_text_rh_wc, aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, hjust = 0.5, fontface = "bold.italic", size = 4)

p

ggsave(
  "Figure_3.jpg",
  plot = p,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)


