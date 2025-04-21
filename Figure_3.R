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
    relative_humidity = "Relative humidity (%)",
    water_content = "Water content (% DM)"
  )[labels$measurement]
  return(labels)
}

# Positioning for facet text inside plot panels
dat_text_rh_wc <- data.frame(
  label = c("Relative humidity", "Water content"),
  measurement = c("relative_humidity", "water_content"),
  x = c(7.5, 7.5),   # adjust depending on your x-axis values
  y = c(117, 380)      # adjust based on expected y-axis ranges
)

# transform WC between 100 and 300 to create axis gap
squish_trans <- scales::trans_new(
  name = "squished",
  transform = function(y) {
    ifelse(y < 100, y,
           ifelse(y < 300, 100 + (y - 100) * 0.1, 120 + (y - 300)))
  },
  inverse = function(y) {
    ifelse(y < 100, y,
           ifelse(y < 120, 100 + (y - 100) / 0.1, 300 + (y - 120)))
  }
)



fig3 <- ggplot() +
  # Horizontal lines at 80 and 100 for RH
  geom_hline(data = data.frame(measurement = "relative_humidity", yint = 80),
             aes(yintercept = yint),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_hline(data = data.frame(measurement = "relative_humidity", yint = 100),
             aes(yintercept = yint),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  
  # Raw relative humidity points + mean line, no SE
  geom_line(data = wc_rh_summary %>% filter(measurement == "relative_humidity"),
            aes(x = prehy_label, y = y_value, group = measurement),
            color = rh.color) +
  # geom_point(data = wc_rh_long %>% filter(measurement == "relative_humidity"),
  #            aes(x = prehy_label, y = value),
  #            fill = rh.color, size = 1, shape = 21, color = "black") + 
  geom_jitter(data = wc_rh_long %>% filter(measurement == "relative_humidity"),
              aes(x = prehy_label, y = value),
              fill = rh.color, size = 1, shape = 21, color = "black",
              width = 0.1, height = 0) +
  
  # Mean ± SE for water content
  geom_line(data = wc_rh_summary %>% filter(measurement == "water_content"),
            aes(x = prehy_label, y = y_value, group = measurement),
            color = wc.color) +
  
  geom_errorbar(data = wc_rh_summary %>% filter(measurement == "water_content"),
                aes(x = prehy_label, ymin = y_value - se, ymax = y_value + se),
                width = 0.4, color = "gray40") +
  
  geom_point(data = wc_rh_summary %>% filter(measurement == "water_content"),
             aes(x = prehy_label, y = y_value),
             fill = wc.color, size = 3, shape = 21, color = "black") +
  
  # Shared theme, facets, etc.
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  theme_light(base_size = 11) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(size = 10),
    axis.ticks.length = unit(0.25, "cm"),
    legend.position = "none",
    strip.text.y.left = element_text(
      size = 10, color = "black", angle = 90,
      hjust = 0.5,  # center align vertically
      margin = margin(r = 3)
    ),

    panel.spacing = unit(6, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 0)),
    strip.switch.pad.wrap = unit(-2.5, "lines"),
    axis.line.y = element_blank()
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
  facetted_pos_scales(
    y = list(
      relative_humidity = scale_y_continuous(
        limits = c(0, 120),
        breaks = seq(0, 100, by = 20),
        labels = scales::label_number(accuracy = 1)
      ),
      water_content = scale_y_continuous(
        trans = squish_trans,
        limits = c(0, 375),
        breaks = c(seq(0, 100, by = 25), seq(300, 375, by = 25)),
        labels = scales::label_number(accuracy = 1)
      )
    )
  ) +
  geom_text(data = dat_text_rh_wc, aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, hjust = 0.5, fontface = "bold.italic", size = 4) + 
  coord_cartesian(clip = "off") +
  
  # Mask the y-axis between 100–300 for visual break
  # Custom y-axis for WC facet, above break
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -0.2, xend = -0.2, y = 200, yend = 400),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +
  
  # Custom y-axis for WC facet, below break
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -0.2, xend = -0.2, y = -10, yend = 102),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +

  # Triangle break symbol
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -0.2, xend = 0.04, y = 202, yend = 151),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +
  
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -0.2, xend = 0.04, y = 100, yend = 151),
    inherit.aes = FALSE,
    color = "black") +
  
  # Custom y-axis for RH facet
  geom_segment(
    data = data.frame(measurement = "relative_humidity"),
    aes(x = -0.2, xend = -0.2, y = -6, yend = 120),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) 

fig3

ggsave(
  "Figure_3.pdf",
  plot = fig3,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)


