#==============================================================
# File description
#==============================================================
# contents: 
#  - simulated outputs from the network-formation model
# code author: Joao Souto-Maior
# last updated: October, 2025  

#==============================================================
# Set up 
#==============================================================

rm(list=ls())
source("header_basic.R")
source("header_process-ABM.R")

#==============================================================
# Empirically informed racial homophily for a 50:50 school
#==============================================================

# parameters from 
# Currarini, S., Jackson, M. O., & Pin, P. (2010). 
#   Identifying the roles of race-based choice and chance in high school friendship network formation. 
#   Proceedings of the National Academy of Sciences, 107(11), 4857-4861.
#   https://www.pnas.org/doi/pdf/10.1073/pnas.0911793107
pct_group = 0.5
IH_data = 0.032 + 2.15 * pct_group - 2.35 *pct_group ^ 2
IH_data # = 0.52

#==============================================================
# Simulated homophily by group (by school composition)
#==============================================================

dat <- BehaviorSpace_load("../data/model-hwh network_formation-table.csv") %>%
  mutate(target_IH = net_homophily) %>%
  pivot_longer(cols = c("sim_homophily_b", "sim_homophily_w"),
               names_to = "race",
               values_to = "sim_IH") %>%
  mutate(race = case_when(race == "sim_homophily_b" ~ "Black",
                          race == "sim_homophily_w" ~ "White"))

plot <- dat %>%
  mutate(pct_w = paste0("S = ", pct_w)) %>%
  ggplot(aes(x = target_IH,
             y = sim_IH,
             color = race)) +
  # geom_point(alpha = 0.05, # --> removed for better visualization;
  #            size = 0.5,   #     points show small variance
  #            shape = 19) +
  geom_smooth(method = 'loess',
              formula = y~x, 
              alpha = 0.5,
              se = F,
              linewidth = 0.5) +
  scale_y_continuous(expression("simulated inbreeding racial homophily"), 
                     limits = c(0, 1)) +
  scale_x_continuous(expression("target inbreeding racial homophily, " * alpha[racial-homophily]), 
                     expand = c(0.02,0.02)) +
  scale_color_manual(values = my2cols) +
  facet_wrap(~pct_w, ncol = 3) +
  ggtheme_facet +
  theme(legend.title = element_blank())
plot

ggsave("figures/fig_net-IH-by-composition.png", width = 15, height = 15, units = "cm", dpi = 1000)

#==============================================================
# Simulated degree by school composition and by group
#==============================================================

dat <- BehaviorSpace_load("../data/model-hwh network_formation-table.csv") %>%
  mutate(target_IH = net_homophily) %>%
  pivot_longer(cols = c("n_ties_b", "n_ties_w"),
               names_to = "race",
               values_to = "sim_IH") %>%
  mutate(race = case_when(race == "n_ties_b" ~ "Black",
                          race == "n_ties_w" ~ "White"))

plot <- dat %>%
  ggplot(aes(x = pct_w,
             y = sim_IH,
             color = race)) +
  # geom_point(alpha = 0.05, # --> removed for better visualization;
  #            size = 0.5,   #     points show small variance
  #            shape = 19) +
  geom_smooth(method = 'loess',
              formula = y~x, 
              alpha = 0.5,
              se = F,
              linewidth = 0.5) +
  scale_y_continuous(expression("simulated average number of ties"), 
                     limit = c(2, 10)) +
  scale_x_continuous(expression("share of White agents, S"),
                     expand = c(0.02,0.02),
                     limit = c(0, 1)) +
  scale_color_manual(values = my2cols) +
  ggtheme_facet +
  theme(legend.title = element_blank())
plot

ggsave("figures/fig_net-ties-by-composition.png", width = 9, height = 10, units = "cm", dpi = 1000)

#==============================================================
# Simulated degree by IH and by group
#==============================================================

plot <- dat %>%
  ggplot(aes(x = target_IH,
             y = sim_IH,
             color = race)) +
  # geom_point(alpha = 0.05, # --> removed for better visualization;
  #            size = 0.5,   #     points show small variance
  #            shape = 19) +
  geom_smooth(method = 'loess',
              formula = y~x, 
              alpha = 0.5,
              se = F,
              linewidth = 0.5) +
  scale_y_continuous(expression("simulated average number of ties"), 
                     expand = c(0.02,0.02),
                     limit = c(2, 10)) +
  scale_x_continuous(expression("inbreeding racial homophily, " * alpha[racial-homophily]),
                     expand = c(0.02,0.02),
                     limit = c(0, 1)) +
  scale_color_manual(values = my2cols) +
  ggtheme_facet +
  theme(legend.title = element_blank())
plot

ggsave("figures/fig_net-ties-by-IH.png", width = 9, height = 10, units = "cm", dpi = 1000)
