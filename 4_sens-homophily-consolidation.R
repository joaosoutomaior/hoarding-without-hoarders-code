#==============================================================
# File description
#==============================================================
# contents: 
#  - conditions for the emergence of opportunity hoarding
# code author: Joao Souto-Maior
# last updated: December, 2025  

#==============================================================
# Set up 
#==============================================================

rm(list=ls())
source("header_basic.R")
source("header_process-ABM.R")

#==============================================================
# Compute outcome for each simulation run
#==============================================================

# load data and define sample
dat <- read_csv("../data/sens-homophily-consolidation/results.csv")%>%
  filter(no_initial_net_access_i == "true")

# adjust variables
dat <- as_tibble(dat) %>% 
  mutate(run_id = as.numeric(run_id),
         race_i = factor(race_i, levels = c("White", "Black")),
         ses_percentile_i = as.numeric(ses_percentile_i))

# unique run ids
run_ids <- sort(unique(dat$run_id))

# define results vector
output <- rep(NA_real_, length(run_ids))
names(output) <- as.numeric(run_ids)

# loop per run
for (k in seq_along(run_ids)) {
  d <- dat[dat$run_id == run_ids[k], , drop = FALSE]
  
  # run the model
  m <- glm(e_i ~ race_i + ses_percentile_i,
           family = poisson(link = "log"),
           data = d)
  
  # extract logRR
  output[k] <- unname(coef(m)[["race_iBlack"]])
}

# convert to tibble
dat_output <- enframe(output, name = "run_id", value = "value") %>%
  mutate(run_id = as.numeric(run_id))

# get summary of results
dat_base <- dat %>%
  select(run_id, bw_ses_diff, net_homophily) %>%
  dplyr::distinct()

# merge by run_id
dat_plot <- left_join(dat_output, dat_base, by = "run_id")

#==============================================================
# Plot results
#==============================================================

# define factor variables  
dat_plot <- dat_plot %>%
  group_by(bw_ses_diff, net_homophily, value) %>%
  summarise(RATIO = value, 
            .groups = "drop") 

# ggplot
p <- dat_plot %>%
  mutate(bw_ses_diff = factor(bw_ses_diff,
                              levels = c("0","0.25","0.5","0.75"))) %>%
  ggplot() +
  geom_smooth(aes(x = net_homophily,
                  y = RATIO,
                  color = bw_ses_diff),
              span = 1, 
              size = 0.5,
              se = FALSE) +
  scale_y_continuous(expression("simulated racial penalties, " * theta[1]),
                     expand = c(0,0),
                     limits = c(-2.5, 1)) +
  scale_color_viridis_d(option = "magma",
                        direction = -1, # -1 reverses the order
                        begin = 0.4, # use a subset of the palette
                        end = 0.9,
                        alpha = 1)  +
  scale_x_continuous(expression("network racial segregation, " * alpha["racial-homophily"]),
                     expand = c(0,0)) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = "dashed",
             size = 0.5) +
  geom_hline(yintercept = -0.225, # --> from main result
             color = graycolor,
             linetype = "dashed",
             size = 0.5) +
  ggtheme_facet +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title.position = "top"
  )+
  labs(color = expression("standardized mean difference in SES composite across race groups, " * alpha["ses-gap"]))
p

# save
ggsave("figures/fig_sens-homophily-consolidation.png", width = 9, height = 10, units = "cm", dpi = 1000)

#==============================================================
# Plot results showing variation
#==============================================================

# Calculate mean and SD for each parameter combination
binned_sd <- dat_plot %>%
  mutate(bw_ses_diff = factor(bw_ses_diff,
                              levels = c("0","0.25","0.5","0.75")),
         homophily_bin = cut(net_homophily, breaks = 11)) %>% # --> one for each value \in {0, 0.1, ..., 1}
  group_by(bw_ses_diff, homophily_bin) %>%
  summarize(net_homophily = mean(net_homophily),
            mean_ratio = mean(RATIO),
            sd_ratio = sd(RATIO),
            .groups = "drop")

# ggplot
p <- dat_plot %>%
  mutate(bw_ses_diff = factor(bw_ses_diff,
                              levels = c("0","0.25","0.5","0.75"))) %>%
  ggplot() +
  geom_smooth(aes(x = net_homophily,
                  y = RATIO,
                  color = bw_ses_diff,
                  fill = bw_ses_diff),
              span = 1, 
              size = 0.5,
              alpha = 1,                 
              se = FALSE) +              
  # adding bands showing 0.5 SD from the mean
   geom_ribbon(data = binned_sd,
              aes(x = net_homophily,
                  ymin = mean_ratio - 0.5 * sd_ratio,
                  ymax = mean_ratio + 0.5 * sd_ratio,
                  fill = bw_ses_diff),
             alpha = 0.15) +
  # (alternatively) adding CIS: those are quite narrow
  # geom_smooth(span = 1, 
  #            size = 0.5,
  #            level = 0.99,
  #            alpha = 0.15,       
  #            se = TRUE) +
  scale_y_continuous(expression("simulated racial penalties, " * theta[1]),
                     expand = c(0,0),
                     limits = c(-2.5, 1)) +
  scale_color_viridis_d(option = "magma",
                        direction = -1,
                        begin = 0.4,
                        end = 0.9,
                        alpha = 1) +
  scale_fill_viridis_d(option = "magma",
                       direction = -1,
                       begin = 0.4,
                       end = 0.9,
                       guide = "none") +
  scale_x_continuous(expression("network racial segregation, " * alpha["racial-homophily"]),
                     expand = c(0,0)) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = "dashed",
             size = 0.5) +
  geom_hline(yintercept = -0.225, # --> from main result
             color = graycolor,
             linetype = "dashed",
             size = 0.5) +
  ggtheme_facet +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title.position = "top"
  ) +
  guides(fill = "none") +
  labs(color = expression("standardized mean difference in SES composite across race groups, " * alpha["ses-gap"]))
p

# save
ggsave("figures/fig_sens-homophily-consolidation-with-variation.png", width = 9, height = 10, units = "cm", dpi = 1000)
