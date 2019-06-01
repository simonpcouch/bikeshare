# Fitting a Model to `bikeshare`

# This file serves to fit a model to the dataset resulting from
# join.R and make a paper-ready table out of it

# Load libraries and data
library(tidyverse)
library(knitr)
library(xtable)
library(scales)
load("data/bikeshare.Rda")

bikeshare_variables <- bikeshare %>%
  # Select only variables that contain demographic information
  # relevant to master statuses from 2010 or later
         # education
  select(frac_coll_plus2010, 
         # income/SES
         poor_share2010, 
         # race
         nonwhite_share2010, 
         # controls
         popdensity2010, 
         job_density_2013, 
         county_has_bikeshare,
         # response variable
         num_stations, num_bikes) %>%
  filter(county_has_bikeshare) %>%
  select(-county_has_bikeshare)

# Scale all of the variables so that coefficients are more easily comparable
# But retain the number of bikeshare stations and bikes as a count
bikeshare_variables <- data.frame(map_dfc(bikeshare_variables[,1:5],
                                          rescale,
                                          to = c(0, 1)),
                                  bikeshare_variables[,6:7])

# we now want each row to be a count, with another indicator
# column giving whether that row is a free bikes or stations count
bikeshare_model_df <- bikeshare_variables %>%
  gather(key = "type", value = "count",
         c("num_stations", "num_bikes"))


# fit the number of bikes and stations using all variables
# as well as the interaction with type
bikeshare_model <- glm(count ~ . + .*type,
                       family = "poisson",
                       data = bikeshare_model_df,
                       maxit = 100)

new_var_names <- c("Intercept", 
                   "% College-Educated",
                   "% in Poverty",  
                   "% Nonwhite",
                   "Population Density", 
                   "Job Density", 
                   "Docking Type")

# Make a dataframe of values representing the model
model_df <- data_frame("Predictor" = c(new_var_names, 
                                       map2_chr(new_var_names[3:length(new_var_names)-1],
                                                rep(" * Docking Type", 
                                                    length(new_var_names)-2),
                                                paste)),
                       "Coefficient" = bikeshare_model[["coefficients"]],
                       "exp(Coefficient)" = exp(bikeshare_model[["coefficients"]]),
                       "p-Value" = coef(summary(bikeshare_model))[,4])

model_df <- model_df %>%
  mutate("Significance" = case_when(`p-Value` < .05 & `p-Value` >= .01  ~ "*",
                                    `p-Value` < .01 & `p-Value` >= .001 ~ "**",
                                    `p-Value` < .001 ~ "***"))

model_df$`p-Value` <- plyr::round_any(model_df$`p-Value`, .001)

model_df$`p-Value`[model_df$`p-Value` < .001] <- "< .001"
