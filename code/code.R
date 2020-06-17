#-----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(ranger)
#-----------------------------------------------------------

data <- dials::Chicago
dim(data)

data %>%  
  ggplot(aes(x = ridership)) +
  geom_density(fill = "#ff6767", alpha = 1) +
  labs(x = "Target Variable", y = "Density") +
  theme_classic(18) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave("presentation/img/density.png", width = 8.5, height = 7, 
       bg = "transparent")

  
data %>%  
  ggplot(aes(x = ridership)) +
  labs(x = "Target Variable", y = "Density") +
  geom_rect(xmin = 1, xmax = 8.5,   ymin = 0, ymax = 0.065, 
            size = 1.5,
            alpha = 0, colour = '#f5c04a', fill = "#f5c04a") +
  geom_rect(xmin = 12.5, xmax = 17.5,   ymin = 0, ymax = 0.095, 
            size = 1.5,
            alpha = 0, colour = '#f5c04a', fill = "#f5c04a") +
  geom_rect(xmin = 18, xmax = 24,   ymin = 0, ymax = 0.095, 
            size = 1.5,
            alpha = 0, colour = '#f5c04a', fill = "#f5c04a") +
  geom_density(fill = "#ff6767", alpha = 1) +
  theme_classic(18) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
  
ggsave("presentation/img/density_boxes.png", width = 8.5, height = 7, 
       bg = "transparent")

#-----------------------------------------------------------
# Step 1. Train (75%) and test (25%) splits ------

data_tibble <- rep(list(data), 10) %>% 
  enframe(name = 'index', value = 'data') %>% 
  mutate(train_test = purrr::map(data, initial_split, prop = 3/4))


#-----------------------------------------------------------
# Step 2. Creating our model list ------

models <- list(
  tree = list(mtry = ncol(data) - 1, trees = 1, reg.factor = 1, 
              depth = FALSE),
  bagging = list(mtry = ncol(data) - 1, trees = 100, reg.factor = 1, depth = FALSE), 
  bagging_reg = list(mtry = ncol(data) - 1, trees = 100, reg.factor = 0.7, depth = FALSE),
  bagging_reg_dep = list(mtry = ncol(data) - 1, trees = 100, reg.factor = 0.7, depth = TRUE), 
  forest = list(mtry = sqrt(ncol(data) - 1), trees = 100, reg.factor = 1, depth = FALSE),
  reg_forest =  list(mtry = (ncol(data) - 1)/2, trees = 100, reg.factor = 0.7, depth = FALSE),
  reg_forest_dep =  list(mtry = (ncol(data) - 1)/2, trees = 100, reg.factor = 0.7, depth = TRUE),
  reg_forest2 =  list(mtry = (ncol(data) - 1)/2, trees = 100, reg.factor = 0.2, depth = FALSE), 
  reg_forest_dep2 =  list(mtry = (ncol(data) - 1)/2, trees = 100, reg.factor = 0.2, depth = TRUE)) %>% 
  enframe(name = 'model', value = 'parameters')


# Combining everything  ------
data_tibble <- data_tibble %>% 
  crossing(models) %>% 
  arrange(model)

#-----------------------------------------------------------
# Step 3. Building our modelling function ------

modelling <- function(train, 
                      mtry = NULL, 
                      trees = NULL, 
                      reg.factor = 1, 
                      depth = FALSE, 
                      formula = ridership ~ ., 
                      mode = "regression") {
  
  
  model_setup <-   
    parsnip::rand_forest(mode = mode, mtry = mtry, trees = trees) %>% 
    parsnip::set_engine("ranger", 
                        regularization.factor = reg.factor, 
                        regularization.usedepth = depth)
  
  #  Random Forest Model Specification (regression)
  #  
  #  Main Arguments:
  #   mtry = mtry
  #   trees = trees
  #  
  #  Engine-Specific Arguments:
  #   regularization.factor = reg.factor
  #   regularization.usedepth = depth
  #  
  #  Computational engine: ranger 
  
  us_hol <- 
    timeDate::listHolidays() %>% 
    str_subset("(^US)|(Easter)")
  
  # Recipe 
  rec <- recipe(formula, data = train) %>% 
    step_holiday(date, holidays = us_hol) %>%  # Include US holidays
    step_date(date) %>% 
    step_rm(date) 

  # Preparation
  preps <- prep(rec, verbose = FALSE)

  # Final fit!   
  fit <- model_setup %>% parsnip::fit(formula, data = juice(preps))

  return(fit)
  
}

#-----------------------------------------------------------
# Step 4. Training all models (70) at once -- might be slow (!)

start_time <- Sys.time()

training_models <- data_tibble %>% 
  mutate(
    all_parameters = 
      map2(parameters, map(train_test, training), 
           ~list_modify(.x, train = .y)))  %>% 
  mutate(model_trained = invoke_map(modelling, all_parameters))

end_time <- Sys.time() 

end_time - start_time  # 6.48 mins

# # Alternative: furrr::future_invoke_map()
# library(furrr)
# plan(multiprocess)
# 
# start_time <- Sys.time()
# training_models_parallel <- data_tibble %>% 
#   mutate(
#     all_parameters = 
#       map2(parameters, map(train_test, training), 
#            ~list_modify(.x, train = .y)), 
#     model_trained = future_invoke_map(modelling, all_parameters))
# 
# end_time <- Sys.time()
# end_time - start_time  # 14.39 secs
#-----------------------------------------------------------
# Evaluation functions

rmse <- function(model, test, formula = ridership ~ ., 
                 us_hol = timeDate::listHolidays() %>% str_subset("(^US)|(Easter)")){
  
  rec <- recipe(formula, data = test) %>% 
    step_holiday(date, holidays = us_hol) %>%  # Include US holidays
    step_date(date) %>% 
    step_rm(date) 
  
  preps <- prep(rec, verbose = FALSE)
  pp <- predict(model, juice(preps))
  sqrt(mean((pp$.pred - test$ridership)^2))
}

n_variables <- function(model){
  length(unique((unlist(model$fit$forest$split.varIDs))))
}

#-----------------------------------------------------------
# Step 5. Evaluating models
results <- training_models %>% 
  mutate(
    rmse = map2_dbl(.x = model_trained,
                   .y = map(train_test, testing), 
                   ~rmse(model = .x, test = .y)), 
    n_variables = map_int(model_trained, n_variables),
    rsquared = map_dbl(model_trained, ~{.x$fit$r.squared})) 


results %>% 
  select(model, rmse, n_variables, rsquared) %>% 
  group_by(model) %>% 
  summarise_all(mean) %>% 
  arrange(desc(n_variables))

results %>% 
  ggplot(aes(x = rmse, y = model)) +
  geom_density_ridges(scale = 4, fill = "#f5c04a") +
  labs(y = "Densities", x = "RMSE") +
  theme_classic(18) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave("presentation/img/results.png", width = 8.5, height = 7, 
       bg = "transparent")
#-----------------------------------------------------------
#-----------------------------------------------------------
