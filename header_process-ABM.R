#==============================================================
# File description
#==============================================================
# contents: 
#   - some basic functions to process output from Netlogo's behaviorspace
# code author: Joao Souto-Maior
# last updated: January, 2022

#==============================================================
# Process data
#==============================================================

BehaviorSpace_load <- function(file_path){
  data <- as_tibble(read.csv(file_path, 
                             skip = 6, 
                             header = TRUE))
  data
}

BehaviorSpace_get_data <- function(dataset,
                                   x, 
                                   y,
                                   model_type){
  
  data <- dataset
  
  data <- data %>%
    rename(y_var = y,
           x_var = x)
  
  dat_means <- data %>%
    select(y_var, x_var) %>% 
    group_by(x_var) %>% 
    summarise_each(funs(mean(. ,na.rm=TRUE))) %>%
    rename_all(paste0, ".mean")
  
  data <- merge(data, dat_means, 
                by.x = "x_var", 
                by.y = "x_var.mean") %>%
    pivot_longer(names_to = "obs.type", 
               values_to = "values", 
               cols = starts_with("y")) %>%
    mutate(model = model_type) %>%
    select(x_var, values, obs.type, model)
  
  data
}
