#==============================================================
# File description
#==============================================================
# contents: 
#  - resource access over time
# code author: Joao Souto-Maior
# last updated: December, 2025  

#==============================================================
# Set up 
#==============================================================

rm(list=ls())
source("header_basic.R")
source("header_process-ABM.R")

#==============================================================
# Resource access over time
#==============================================================

# load data
dat <- BehaviorSpace_load("../data/model-hwh resource-access-table.csv") %>%
  rename(step = X.step.) %>%
  group_by(step) %>%
  summarise(
    mean_black = mean(pct_adopted_b, na.rm = TRUE),
    mean_white = mean(pct_adopted_w, na.rm = TRUE),
    .groups = "drop") %>% 
  pivot_longer(cols = c("mean_black", "mean_white"),
               names_to = "race",
               values_to = "pct") %>%
  mutate(race = case_when(
    race == "mean_black" ~ "Black",
    race == "mean_white" ~ "White"))

# ggplot
p <- dat %>%
  ggplot() +
  geom_smooth(aes(x = step,
                  y = pct,
                  color = race),
              span = 0.5, 
              size = 0.5,
              se = F) +
  scale_y_continuous("percentage of not-initially-resourced agents\nwith access to the network-based resource",
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0)) +
  scale_x_continuous("time step",
                     #limits=c(0,0.5),
                     expand = c(0,0)) +
  scale_color_manual(values = my2cols) +
  ggtheme_facet +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank()) 
p

# save
ggsave("figures/fig_resource-access-over-time.png", width = 9, height = 10, units = "cm", dpi = 1000)
