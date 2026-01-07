#==============================================================
# File description
#==============================================================
# contents: 
#  - sensitivity for racial composition
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
dat <- read_csv("../data/sens-racial-composition/results.csv") %>%
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
  select(run_id, pct_w) %>%
  dplyr::distinct()

# merge by run_id
dat_plot <- left_join(dat_output, dat_base, by = "run_id")

#==============================================================
# Racial composition
#==============================================================
  
# prepare plot
dat_plot <- dat_plot %>%
  group_by(pct_w) %>%
  summarise(RATIO = value, .groups = "drop")

# ggplot
p <- dat_plot %>%
  ggplot() +
  geom_smooth(aes(x = pct_w,
                  y = RATIO),
              span = 1, 
              size = 0.5,
              se = FALSE,
              method = "loess",
              color = "red") +
  scale_y_continuous(expression("simulated racial penalties, " * theta[1]),
                     expand = c(0,0),
                     limits = c(-1,0.5)) +
  scale_x_continuous(expression("share of White agents, S"),
    expand = c(0,0)
  ) +
  geom_hline(yintercept = 0,
             color = "black",
             linetype = "dashed",
             size = 0.5) +
  ggtheme_facet +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title.position = "top"
  )
p

# save
ggsave("figures/fig_sens-racial-composition.png", width = 9, height = 9, units = "cm", dpi = 1000)
