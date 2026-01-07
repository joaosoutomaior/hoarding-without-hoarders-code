#==============================================================
# File description
#==============================================================
# contents: 
#  - sensitivity for n
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
dat <- read_csv("../data/sens-n/results.csv") %>%
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
           family = poisson(link = "log"), # to get log risk ratio (logRR)
           data = d)
  
  # extract logRR
  output[k] <- unname(coef(m)[["race_iBlack"]])
}

# convert to tibble
dat_output <- enframe(output, name = "run_id", value = "value") %>%
  mutate(run_id = as.numeric(run_id))

# get summary of results
dat_base <- dat %>%
  select(run_id, net_diffusion, n) %>%
  dplyr::distinct()

# merge by run_id
dat_plot <- left_join(dat_output, dat_base, by = "run_id")

#==============================================================
# Plot results
#==============================================================

# define factor variables
dat_plot <- dat_plot %>%
  mutate(net_diffusion = as.factor(net_diffusion),
         n = case_when(
           n == 1000 ~ "N = 1,000",
           n == 2000 ~ "N = 2,000",
           n == 3000 ~ "N = 3,000",
           TRUE ~ as.character(n)  # catch any other values
         ),
         n = factor(n, levels = c("N = 1,000", "N = 2,000", "N = 3,000")))

# compute means
mu <- dat_plot %>%
  group_by(net_diffusion, n) %>%
  summarise(group_mean = mean(value, na.rm = TRUE), 
            .groups = "drop")

# ggplot
p <- ggplot() +
  geom_density(data = dat_plot,
               aes(x = value,
                   color = net_diffusion,
                   fill = net_diffusion),
               size = 0.5,
               bw = 0.1,
               alpha = 0.2) +
  geom_vline(data = mu,
             aes(xintercept = group_mean,
                 color = net_diffusion),
             size = 0.5,
             linetype = "dashed") +
  scale_y_continuous("density",
                     limits=c(0,5),
                     expand = c(0,0)) +
  scale_x_continuous(expression("simulated racial penalties, " * theta[1]),
                     limits=c(-0.999,0.999),
                     expand = c(0,0)) +
  scale_color_manual(values = my2cols) +
  scale_fill_manual(values = my2cols) +
  facet_wrap(~n, nrow = 1) +
  ggtheme_facet +
  theme(
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title.position = "left") +
  labs(color = expression(beta["net-diffusion"])) +
  guides(fill = "none")
p

# save
ggsave("figures/fig_sens-n.png", width = 14, height = 8, units = "cm", dpi = 1000)
