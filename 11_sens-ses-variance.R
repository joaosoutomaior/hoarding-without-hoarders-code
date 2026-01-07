#==============================================================
# File description
#==============================================================
# contents: 
#  - sensitivity for variance in SES variable
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

# Define folder path and file names
dat <- read_csv("../data/sens-ses-variance/results.csv") %>%
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
  select(run_id, net_diffusion, ses_variance) %>%
  dplyr::distinct()

# merge by run_id
dat_plot <- left_join(dat_output, dat_base, by = "run_id")

#==============================================================
# Plot results
#==============================================================

# prepare factor variable
dat_plot <- dat_plot %>%
  mutate(net_diffusion = as.factor(net_diffusion),
         ses_variance = as.factor(ses_variance )) 

# compute means
mu <- dat_plot %>%
  group_by(net_diffusion, ses_variance) %>%
  summarise(group_mean = mean(value, na.rm = TRUE), 
            .groups = "drop")

# labeller function
sigma_labeller <- as_labeller(function(x) {
  parse(text = paste0("sigma^2 == ", x))
}, default = label_parsed)

# ggplot
p <- ggplot() +
  geom_density(data = dat_plot,
               aes(x = value,
                   color = net_diffusion,
                   fill = net_diffusion),
               size = 0.5,
               bw = 0.1,
               alpha = 0.3) +
  geom_vline(data = mu,
             aes(xintercept = group_mean,
                 color = net_diffusion),
             size = 0.5,
             linetype = "dashed") +
  scale_y_continuous("density",
                     limits=c(0,5),
                     expand = c(0,0)) +
  scale_x_continuous(expression("simulated racial penalties, " * theta[1]),
                     limits=c(-1.5,1),
                     expand = c(0,0)) +
  scale_color_manual(values = my2cols) +
  scale_fill_manual(values = my2cols) +
  geom_vline(xintercept = 0,
             color = "black",
             linetype = "dashed",
             size = 0.5) +
  facet_wrap(~ses_variance, nrow = 1, labeller = sigma_labeller) +
  ggtheme_facet +
  theme(
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title.position = "left") +
  labs(color = expression(beta["net-diffusion"])) +
  guides(fill = "none")
p

# save
ggsave("figures/fig_sens-ses-variance.png", width = 17, height = 8, units = "cm", dpi = 1000)
