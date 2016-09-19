
# PURPOSE -----------------------------------------------------------------

#  Run the simulation based on user input
#  Output a dataframe and ggplot
#  Try purrr to iterate

# SETUP -------------------------------------------------------------------
rm(list = ls())
#  setwd("C:/Users/mammykins/Documents/GitHub/rebmaindeR/")
setwd("C:/Users/mammykins/Documents/GitHub/maintenance/")

#getwd()

# LIBRARY -----------------------------------------------------------------
library(tidyverse)
library(markovchain)

# USER INPUT --------------------------------------------------------------

build_type_of_interest <- "built_total"
timesteps <- 30  #  forecast horizon, numer of time steps to simulate

# GET RELEVANT DATA AND REBMAINDER FUNCTION -------------------------------------------------------

source("00_getdata.R")
source("rebmainder_all_in_one.R")  #  Call the r
#  levels(pdsp_data$Building_Type)  #  build_type_of_interest potential input

# TRANSITION MATRIX -------------------------------------------------------
# tm_data <- tm_data_with_na  #  uncomment if you want non-zero na rates, mroe realistic for long timeframe
source("tm_create_and_tidy.R")

# OTHER USER INPUTS ------------------------------------------------------------

R <- 1500e6 #  £m invested in rebuilding the buildings at time t
main_cash <- 1500e6  #  £m maintenance cash
revenue <- 1234e6  #  £m revenue of the school
fixed_main_cost <- 1288e6  #  £m Fixed maitanence cost
infl_rate <- 1.13  #  inflation rate, should be a vector
# infl_rate <- c(1.00, 1.02, 1.04, 1.06, 1.08, 1.10, 1.12, 1.14, 1.16, 1.18,
#                1.20, 1.22, 1.24, 1.26, 1.28, 1.30, 1.32, 1.34, 1.37, 1.39, 
#                1.41, 1.43, 1.45, 1.47, 1.49, 1.51, 1.53, 1.55, 1.57, 1.59)  #  30 years forecast from 

# INITIAL STATE --------------------------------------------------------------
initial_state <- pdsp_data %>%
  filter(Building_Type == build_type_of_interest) %>%
  select(-Building_Type, -ends_with("Count")) %>%
  as.numeric()  #  Drop building type, remove columns and names

# PARAMETERS (DEFAULTS SET; OPTIONAL USER INPUT) --------------------------------------------------------------
#M <- main_cash / infl_rate + revenue - fixed_main_cost  #  Excel sandpit model
M <- (main_cash + revenue - fixed_main_cost) / infl_rate # amount of money invested in maintaining the buildings at time t
r <- 2000 # rebuild rate/unit cost of bringing a building back to new condition

######-note-######-----------unit maintenance cost should be a matrix consisting different building type-------------

cost_b <- 179 # unit maintenance cost of bringing B condition buildings back to condition A
cost_c <- 801 # unit maintenance cost of bringing C condition buildings back to condition A 
cost_d <- 2246 # unit maintenance cost of bringing D condition buildings back to condition A 

x_b <- 0.00  # proportion of maintenance fund allocated to building type B 
x_c <- 0.81  # proportion of maintenance fund allocated to building type C 
x_d <- 0.19  # proportion of maintenance fund allocated to building type D 

y_b <- 0.15  # proportion of rebuilding fund allocated to building type B 
y_c <- 0.45  # proportion of rebuilding fund allocated to building type C 
y_d <- 0.40  # proportion of rebuilding fund allocated to building type D 

dis_m <- 0.5  # discounted factor for maintenance cost
dis_r <- 0.17 # discounted factor for rebuild cost

# VECTORISATION -----------------------------------------------------------
 
revenue <- rep(revenue, timesteps)
fixed_main_cost <- rep(fixed_main_cost, timesteps)
main_cash <- rep(main_cash, timesteps)
M <- (main_cash + revenue - fixed_main_cost) / infl_rate
# amount of money invested in maintaining the buildings at time t
R <- rep(R, timesteps) / infl_rate
# Rebuilding fund should also be deflated

# USE FUNCTION ------------------------------------------------------------

condition_df <- rebmainder(starting_state = initial_state, timesteps,
                           rebuild_investment = R,
                           maintenance_investment = M,
                           rebuild_cost_back_to_new = r,
                           discount_rate_rebuild = dis_r, discount_rate_maintain = dis_m, 
                           prop_maint_b = x_b, prop_maint_c = x_c, prop_maint_d = x_d,
                           prop_rebuild_b = y_b, prop_rebuild_c = y_c, prop_rebuild_d = y_d,
                           cost_b_to_a = cost_b, cost_c_to_a = cost_c, cost_d_to_a = cost_d, transition_matrix = dtmc)

# GGPLOT COLOURS and DATA RESHAPE ------------------------------------------------------------------
colour_blind_mode <- TRUE
source("plot_condition_output.R")

# SAVE AS -----------------------------------------------------------------
#g <- plot1
#g <- gridExtra::arrangeGrob(plot1, plot2, plot3, nrow = 3) #generates g
#ggsave(file = "whatever.pdf", g) #saves g

