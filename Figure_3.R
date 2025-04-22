# Fig 3
# load packages
library(tidyverse)
library(ggh4x)
library(scales)

# load data, water content and relative humidity
wc <- read.csv("prehy_wc.csv", stringsAsFactors = T)
rh <- read.csv("prehy_rh.csv", stringsAsFactors = T)

# add minutes column to wc
wc <- wc %>%
  mutate(Minutes = case_when(
    !is.na(Prehy_time_h) ~ Prehy_time_h * 60,
    prehy_time_cat == "Full_turgor" ~ 14400
  )) %>%
  mutate(Hours = case_when(
    !is.na(Minutes) ~ Minutes / 60,
  ))

# fill in Minutes in rh
rh <- rh %>%
  mutate(Minutes = case_when(
    !is.na(Hours) ~ Hours * 60,
    TRUE ~ Minutes  # keep existing values in Minutes
  ))

# join
wc_rh <- full_join(wc, rh, by = c("Hours", "Minutes")) %>% dplyr::select(-se)

# convert to long and rename measurement category
wc_rh_long <- wc_rh %>%
  pivot_longer(cols = c(WC_pct, mean.RH),
               names_to = "measurement",
               values_to = "value") %>%
  mutate(
    measurement = recode(measurement,
                         WC_pct = "water_content",
                         mean.RH = "relative_humidity"))

# select and reorganize columns
wc_rh_long <- wc_rh_long %>% dplyr::select(Minutes, Hours, measurement, value)

summary <- wc_rh_long %>%
  group_by(Minutes, measurement) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>% 
  mutate(se = na_if(se, 0)) %>% # se and sd calculated as 0 because only one rep in dataset, 3x
  mutate(sd = na_if(sd, 0))

# add back rh se to summary
rh$measurement <- rep("relative_humidity")
summary <- full_join(summary, rh, by = c("Minutes", "measurement"), suffix = c(".s", ".rh")) %>%
  dplyr::select(-Hours, -mean.RH) %>%
  mutate(se = coalesce(se.s, se.rh)) %>%
  select(-se.s, -se.rh)

# Facet strip labels
axis_labeller_rh_wc <- function(labels) {
  labels$measurement <- c(
    relative_humidity = "Relative humidity (%)",
    water_content = "Water content (% DM)"
  )[labels$measurement]
  return(labels)
}

# transform y axis (WC) between 100 and 300 to create axis gap
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

# custom x axis ticks
custom_ticks <- c(
  "0 h" = 0,
  "1 h" = 60,
  "2 h" = 120,
  "3 h" = 180,
  "4 h" = 240,
  "8 h" = 480,
  "12 h" = 720,
  "16 h" = 960,
  "20 h" = 1200,
  "1 d" = 1440,
  "2 d" = 2880,
  "3 d" = 4320,
  "4 d" = 5760,
  "7 d" = 10080,
  "Full\nturgor" = 14400
)

# transform Minutes into a visually continuous but nonlinear axis, made of segments with different squash factors:
# as follows:
# Segment, (minutes),	Visual scale,	Squash factor
# 0–240,	Linear,	1x
# 240–1440,	Squashed 4x,	1/4 = 0.25x
# 1440–5760,	Squashed 24x,	1/24 ≈ 0.0417x
# 5760–14400,	Squashed 72x,	1/72 ≈ 0.0139x

# define break points and squash factors
breaks <- c(0, 240, 1440, 5760, 14400)
squash_factors <- c(1, 1/4, 1/24, 1/72)

# create custom transformation
piecewise_minutes_trans <- trans_new(
  name = "piecewise_minutes",
  
  transform = function(x) {
    sapply(x, function(val) {
      if (is.na(val)) return(NA_real_)
      if (val <= 240) {
        return(val)
      } else if (val <= 1440) {
        return(240 + (val - 240) * 1/4)
      } else if (val <= 5760) {
        return(240 + (1440 - 240) * 1/4 + (val - 1440) * 1/24)
      } else {
        return(240 + (1440 - 240) * 1/4 + (5760 - 1440) * 1/24 + (val - 5760) * 1/72)
      }
    })
  },
  
  inverse = function(x) {
    sapply(x, function(val) {
      if (is.na(val)) return(NA_real_)
      if (val <= 240) {
        return(val)
      } else if (val <= 240 + (1440 - 240) * 1/4) {
        return(240 + (val - 240) * 4)
      } else if (val <= 240 + (1440 - 240) * 1/4 + (5760 - 1440) * 1/24) {
        return(1440 + (val - (240 + (1440 - 240) * 1/4)) * 24)
      } else {
        return(5760 + (val - (240 + (1440 - 240) * 1/4 + (5760 - 1440) * 1/24)) * 72)
      }
    })
  }
)

# add error bar widths to summary df
# error bar widths are in minutes and then scaled by the inverse of the squash factor to make visually consistent
base.width <- 27
summary <- summary %>%
  mutate(
    left_width = case_when(
      Minutes <= 240 ~ base.width / 2,                           # left half in 1x zone
      Minutes <= 1440 ~ (base.width * 4) / 2,
      Minutes <= 5760 ~ (base.width * 24) / 2,
      Minutes <= 14400 ~ (base.width * 72) / 2,
      TRUE ~ NA_real_
    ),
    right_width = case_when(
      Minutes < 240 ~ base.width / 2,
      Minutes == 240 ~ (base.width * 4) / 2,                     # right half in 4x zone
      Minutes < 1440 ~ (base.width * 4) / 2,
      Minutes == 1440 ~ (base.width * 24) / 2,
      Minutes < 5760 ~ (base.width * 24) / 2,
      Minutes == 5760 ~ (base.width * 72) / 2,
      Minutes <= 14400 ~ (base.width * 72) / 2,
      TRUE ~ NA_real_
    )
  ) 

rh.color <- "#F9C205" # relative humidity
wc.color <- "#A7C9EC" # water content

# Positioning for facet text inside plot panels
dat_text_rh_wc <- data.frame(
  label = c("Relative humidity", "Water content"),
  measurement = c("relative_humidity", "water_content"),
  x = c(960, 960),   # x location
  y = c(117, 382)      # y location
)



#### PLOT ####

fig3 <- ggplot(data = summary, aes(x = Minutes)) +
  
  # Custom y-axis for RH facet
  geom_segment(
    data = data.frame(measurement = "relative_humidity"),
    aes(x = -40, xend = -40, y = -6, yend = 120),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +
  
  # Custom y-axis for WC facet, above break
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -40, xend = -40, y = 200, yend = 390),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +
  
  # Triangular axis break symbol (top)
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -40, xend = -30, y = 202, yend = 151),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +
  
  # Triangular axis break symbol (bottom)
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -40, xend = -30, y = 100, yend = 151),
    inherit.aes = FALSE,
    color = "black") +
  
  # Custom y-axis for WC facet, below break
  geom_segment(
    data = data.frame(measurement = "water_content"),
    aes(x = -40, xend = -40, y = 0, yend = 102),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.5) +

  # Horizontal lines at 80 and 100 for RH
  geom_hline(data = data.frame(measurement = "relative_humidity", yint = 80),
             aes(yintercept = yint),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_hline(data = data.frame(measurement = "relative_humidity", yint = 100),
             aes(yintercept = yint),
             linetype = "dashed", color = "gray70", linewidth = 0.4) +
  # mean 
  geom_line(
    data = summary %>% filter(measurement == "water_content", !is.na(mean)),
    aes(x = Minutes, y = mean, group = 1),
    color = wc.color,
    linewidth = 0.5
  ) +
  geom_line(
    data = summary %>% filter(measurement == "relative_humidity", !is.na(mean)),
    aes(x = Minutes, y = mean, group = 1),
    color = rh.color,
    linewidth = 0.5
  ) +
  
  # add error bars under points
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "gray40") +
  
  # Top horizontal line of error bar
  geom_segment(
    data = summary,
    aes(x = Minutes - left_width, xend = Minutes + right_width,
        y = mean + se, yend = mean + se),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth = 0.5
  ) +
  
  # Bottom horizontal line of error bar
  geom_segment(
    data = summary,
    aes(x = Minutes - left_width, xend = Minutes + right_width,
        y = mean - se, yend = mean - se),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth = 0.5
  ) +

  geom_point(aes(y = mean, fill = measurement),
             size = 3, shape = 21, color = "black") +
  
  # theme, facets, etc.
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
      margin = margin(r = 3)),
    panel.spacing = unit(6, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 0)),
    strip.switch.pad.wrap = unit(-2.5, "lines"),
    axis.line.y = element_blank()) +
  
  scale_color_manual(
    values = c(
      relative_humidity = rh.color,
      water_content = wc.color)) +
  scale_fill_manual(
    values = c(
      relative_humidity = rh.color,
      water_content = wc.color)) +
  
  ylab(NULL) +
  xlab(NULL) +
  facet_grid2(
    rows = vars(measurement),
    switch = "y",
    scales = "free",
    labeller = axis_labeller_rh_wc,
    independent = "x") +
  
  # non-linear custom transform on x 
  scale_x_continuous(
    name = "Length of prehydration treatment (hours or days)",
    trans = piecewise_minutes_trans,
    breaks = unname(custom_ticks),
    labels = names(custom_ticks)) +

  # limits and transformation on y axes
  facetted_pos_scales(
    y = list(
      relative_humidity = scale_y_continuous(
        limits = c(0, 120),
        breaks = seq(0, 100, by = 20),
        labels = scales::label_number(accuracy = 1)
      ),
      water_content = scale_y_continuous(
        trans = squish_trans,
        limits = c(0, 390),
        breaks = c(seq(0, 100, by = 25), seq(300, 390, by = 25)),
        labels = scales::label_number(accuracy = 1)
      )
    )
  ) +
  geom_text(data = dat_text_rh_wc, aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, hjust = 0.5, fontface = "bold.italic", size = 4) + 
  coord_cartesian(clip = "off", expand = FALSE) +
  
  # plot invisible vline to expand the plot to the right a bit
  geom_vline(xintercept = 17280, alpha = 0) +
  
# Add A and B to panels
  geom_text(
    data = data.frame(
      label = c("A", "B"),
      measurement = c("relative_humidity", "water_content"),
      x = c(-Inf, -Inf),
      y = c(Inf, Inf),
      hjust = c(2, 2),   
      vjust = c(0.6, -.25)    
    ),
    aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold"
  )

fig3

ggsave(
  "Figure_3.pdf",
  plot = fig3,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)


