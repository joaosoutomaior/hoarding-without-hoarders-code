#==============================================================
# File description
#==============================================================
# contents: 
#   - general packages and functions
# code author: Joao Souto-Maior
# last updated: January, 2025

#==============================================================
# Packages
#==============================================================

setwd("C:/Users/joaom/Documents/Research projects/hoarding-without-hoarders/hoarding-without-hoarders-code")
library(tidyverse)
library(data.table)
library(kableExtra)
library(broom.mixed)
library(lme4)
require(lmerTest)
library(RColorBrewer)
library(texreg)
library(viridis)
library(purrr)
library(broom)

#==============================================================
# Colors (from Dark2)
#==============================================================

# paletteer_d("RColorBrewer::Dark2")
 c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF", "#66A61EFF", "#E6AB02FF", "#A6761DFF", "#666666FF")

dark2_green = "#1B9E77FF"
dark2_purple = "#7570B3FF" 
dark2_orange = "#D95F02FF"
dark2_yellow = "#E6AB02FF"
dark2_pink = "#E7298AFF"

my2cols = c(dark2_purple, dark2_green)
my2colsb = c(dark2_purple, dark2_pink)

grayfill = "#E0E0E0"
graycolor ="#797D7F"

#==============================================================
# Some functions for data description
#==============================================================

freq_table <- function(x){
  cnt <- table(x, useNA = "always")
  tbl <- cbind(cnt, paste0("(",round(prop.table(cnt)*100,2), "%)"))
  colnames(tbl) <- c('Count','Percentage')
  tbl
}

vars_NA <- function(x) any(is.na(x))
vars_no_NA <- function(x) any(!is.na(x))

summarize_group <- function(x) {
  id_count <- table(x)  # counts of unique ids
  return(list(
    UniqGroups=length(id_count),
    minPerGroup=min(id_count),
    meanPerGroup=mean(id_count),
    maxPerGroup=max(id_count)))
}

pct_missing_variable <- function(dataset){
  dataset %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = paste0(round(100 * pct_missing, 2), " %")) %>%
    as_tibble()
}

pct_given_value <- function(dataset, x){
  dataset[dataset != x] <- 0
  dataset[dataset == x] <- 1
  dataset %>% 
    mutate_if(is.character, as.numeric) %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(pct_missing = mean(value, na.rm = T)) %>%
    mutate(pct_missing = 100 * pct_missing) %>%
    as_tibble()
}

summary_of_data <- function(dataset){
  dataset %>% 
    gather(Variable, value) %>%
    mutate(value = as.numeric(value)) %>%
    group_by(Variable) %>%
    summarise(Mean = mean(value, na.rm = T),
              S.d. = sd(value, na.rm = T),
              Min. = min(value, na.rm = T),
              Max. = max(value, na.rm = T),
              pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = round(100 * pct_missing, 1),
           Mean = round(Mean, 1),
           S.d. = round(S.d., 1),
           Max. = round(Max., 1),
           Min. = round(Min., 1)) %>%
    rename("% missing" = pct_missing)
}

mean_of_data <- function(dataset){
  dataset %>% 
    gather(Variable, value) %>%
    group_by(Variable) %>%
    summarise(Mean = mean(value, na.rm = T)) %>%
    mutate(Mean = round(Mean, 2)) %>%
    pivot_wider(names_from = Variable, values_from = Mean)
}

agg <- function(dat, dat_var, final_name, dat_ID, ID_name){
  temp_data <- aggregate(dat_var, by = list(dat_ID), FUN = sum, na.rm = TRUE)
  temp_data <- temp_data %>%
    setNames(c(ID_name, final_name))
  new <- inner_join(dat, temp_data, by = ID_name)
  return(new)
}

agg_mean <- function(dat, dat_var, final_name, dat_ID, ID_name){
  temp_data <- aggregate(dat_var, by = list(dat_ID), FUN = mean, na.rm = TRUE)
  temp_data <- temp_data %>%
    setNames(c(ID_name, final_name))
  new <- inner_join(dat, temp_data, by = ID_name)
  return(new)
}

#==============================================================
# My ggplot theme
#==============================================================

ggtheme_facet =  theme_minimal(base_size = 7) +
  theme(plot.background = element_rect(fill = "white", 
                                       color = "white"), ##F6FCF8"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(),
        plot.title = element_text(),
        plot.caption = element_text(vjust = 0.5, 
                                    hjust = 1),
        axis.title.x = element_text(margin = margin(15, 15, 15, 15)),
        axis.title.y = element_text(margin = margin(15, 15, 15, 15)),
        panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    size = 0.5),
        plot.margin = unit(c(.5,.5,.5,.5),"cm"),
        strip.text.x = element_text(size = 7, 
                                    color = "black", 
                                    face = "bold"),
        strip.background = element_rect(color="black", 
                                        fill = grayfill, 
                                        size=1,
                                        linetype="solid"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title.position = "left",
        legend.title.align = 0.5,
        legend.title = element_text(size = 6, 
                                    color = "black")) 