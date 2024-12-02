# prehy paper new Fig 5
#

# install packages
install.packages('tidyverse')
library(tidyverse)

# load in data
pr <- read.csv("prehy_regen_means.csv")
head(pr)

# dataset modified so se is all in one column
pr_l <- read.csv("prehy_regen_means_longer.csv")
head(pr_l)

# figure displaying first regen day and regen pts d21 ~ prehydration time
# re-order x axis so control is first
pr_l$treatment <- factor(pr_l$treatment, levels=unique(pr_l$treatment))
# change colors 
pink <- "#EA33F7"
green <- "#75FC4C"
blue <- "#0100F0"
# remove background using theme() code below - done
# get rid of legend and instead put words on fig like fig 3 - done
# make 2 y axes but basically just duplicate y axis
pr_l %>%
  ggplot(aes(x = treatment, y = y_value, colour = measurement)) + 
  geom_line(aes(group = measurement)) + 
  geom_errorbar(aes(ymin = y_value - se, ymax = y_value + se, colour = 'black')) +
  geom_point(size=5, shape = 1, colour = "black") + 
  geom_point(size=4.2)+
  scale_color_manual(values=c(first_regen_day = green, regen_pts_d21 = blue)) +
  annotate("text", x=9, y=9, label= "day of first regeneration", size=4.5) +
  annotate("text", x=9, y=1.5, label= "# regeneration points at day 21", size=4.5) +
  # Custom the Y scales:
  scale_y_continuous(
    # Features of the first axis
    name = "Day",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~.*1, name="Regeneration points")) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        axis.title = element_text(face='bold', size=12),
        axis.text = element_text(face='bold', size=11),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    panel.border       = element_blank()) +
  theme(legend.position="none") +
  labs(x = "Prehydration time, hours or days") 

## save as hi rez
ggsave(
  "prehy_Fig5.jpg",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

##
# alternative Fig. 5 display as multi-panel plot
# use + facet_wrap(~variable, scales = "free")
# getting close
pr_l %>%
  ggplot(aes(x = treatment, y = y_value, colour = measurement)) + 
  facet_wrap(~measurement, scales = "free", ncol=1) +
  geom_line(aes(group = measurement)) + 
  geom_errorbar(aes(ymin = y_value - se, ymax = y_value + se, colour = 'black')) +
  geom_point(size=5, shape = 1, colour = "black") + 
  geom_point(size=4.2) +
  scale_color_manual(values=c(first_regen_day = green, regen_pts_d21 = blue)) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        axis.title = element_text(face='bold', size=12),
        axis.text = element_text(face='bold', size=11),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(legend.position="none") +
  labs(x = "Prehydration time, hours or days") 
##
