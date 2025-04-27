# Analyses for Coe et al. 
# "Influence of prehydration events on revival of the dryland moss Syntrichia caninervis desiccated for 17 years: does water content matter?"

# install packages
install.packages('tidyverse')
library(tidyverse)
install.packages('FSA')
library(FSA) # for dunn test
install.packages("easynls")
library(easynls)

# analyses for water content as a function of prehydration time n=3
# load in data
wc <- read.csv("data/prehy_wc.csv")

# observe data 
plot(wc$prehy_time_cat, wc$WC_pct, ylim=c(0,110))

#isolate numerical data for analyses
wc_num <- wc[1:42,]
plot(wc_num$Prehy_time_h, wc_num$WC_pct)

#set up plot window to see nls results best
par(cex.lab = 0.8, cex.axis = 0.8, cex=0.8)

# fit with linear plateau model
wc_num%>%
  select(Prehy_time_h,WC_pct) %>%
  nlsplot(., model=3) #linear plateau r2=0.93

# cunduct series of anovas to determine where inflection point is
wc_num_1 <- wc_num %>%
  filter(Prehy_time_h > 19) #modify this number as needed 
  
summary(aov(WC_pct~prehy_time_cat, data=wc_num_1))
TukeyHSD(aov(WC_pct~prehy_time_cat, data=wc_num_1))
  # test result: not different >19h... (16 was lower).. so 20+hours same

# analyses for regeneration metrics as a function of prehydration time
## day of first regeneration, # regeneration points at day 21, % green leaves at day 7
# load in data
p <- read.csv("data/prehy.csv") 
pr <- read.csv("data/prehy_regen_means.csv")

#normality check for green leaves 7d post
shapiro.test(p$green_lvs_7daypost) #not normal
shapiro.test(pr$percent_green_lvs_7dpost) #normal (when considered as means)

pr %>%
  ggplot(aes(x=treatment_h,y=percent_green_lvs_7dpost))+
  geom_point()
# analysis use quadratic model
pr%>%
  select(treatment_h,percent_green_lvs_7dpost) %>%
  nlsplot(., model=2) #quadratic r2=0.63

kruskal.test(green_lvs_7daypost~treatment, data=p)
# result x2=31.14, p=0.003
dunnTest(green_lvs_7daypost ~ treatment, data = p, method="bonferroni")
# result 3d dif from control

#using nonlinear models and means for other response vars
install.packages('drc')
install.packages('nlme')
library(drc)
install.packages('devtools')
library(devtools)
install_github("onofriandreapg/aomisc")
library(aomisc)

#regen pts d21
shapiro.test(pr$regen_pts_d21) #normal
plot(pr$regen_pts_d21~pr$treatment_h)

# fit with asymtotic regression model
model <- drm(regen_pts_d21 ~ treatment_h, fct = DRC.asymReg(),
              data=pr)
summary(model) # P < 0.0001

# first regen day
plot(pr$first_regen_day~pr$treatment_h)
# fit with exp decay model
model2 <- drm(first_regen_day ~ treatment_h, fct = DRC.expoDecay(),
             data = pr)
summary(model2) # p<0.0001

##