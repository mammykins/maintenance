
# PURPOSE -----------------------------------------------------------------

#  User inputs all the parameters and necessary inputs to run the hybrid simulation
#  Which runs the maintenance and rebuild functions on the current state
#  and then deteriorates to provide the next step


# SETUP -------------------------------------------------------------------
rm(list = ls())
#setwd("./maintenance")
#getwd()

# LIBRARY -----------------------------------------------------------------
library(dplyr)
library(markovchain)
library(purrr)

# DATA --------------------------------------------------------------------
source("01_rebuild_maintenance.R")

# USER INPUT --------------------------------------------------------------

build_type_of_interest <- "built_pre_1919"
timesteps <- 10  #  forecast horizon, numer of time steps to simulate

# GET RELEVANT DATA
source("00_getdata.R")
source("02_det_model_setup.R")

# OTHER INPUTS

R <- 1500e6 #  £m invested in rebuilding the buildings at time t
main_cash <- 1500e6  #  £m maintenance cash
revenue <- 1234e6  #  £m revenue of the school
fixed_main_cost <- 1288e6  #  £m Fixed maitanence cost
infl_rate <- 1.13  #  inflation rate, should be a vector

# INITIAL STATE --------------------------------------------------------------
initial_state <- pdsp_data %>%
  filter(Building_Type == build_type_of_interest) %>%
  select(-Building_Type, -ends_with("Count")) %>%
  as.numeric()  #  Drop building type, remove columns and names

# PARAMETERS --------------------------------------------------------------
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

# ONE TIME STEP HYBRID ----------------------------------------------------
# TEST
current_state <- initial_state

#  det_model(current_state = rebmain_state_fun(), transition_mat = dtmc)

# SIMULATION --------------------------------------------------------------
hybrid_sim <- function(start_state = initial_state, t = timesteps, ...) { 
  #  input the initial state from n, a, b, c, d, e as the current state at time zero
  #  create an empty matrix to hold timesteps + 1 rows and 6 columns
  #  first row is initial state
  mat_state <- matrix(0, nrow = (t + 1), ncol = 6)
  #print(mat_state)
  mat_state[1, ] <- start_state
  #print(mat_state)
  
for (i in 1:(t)) {
  
  h <- mat_state[i, 6]*r # £ to rebuild all current decommissioned 
  #  need to update iteratively, hence inclusion here
  
  #  Check lexical scoping, R searches within the function
  if (R > h) {  #  £ cost of bringing e to a
    #  money surplus for rebuilding b, c, d after rebuilding all e
    alf_1 <- 1  
    alf_2 <- 0
  }  else {
    #  when there is no surplus after attempting rebuild of all e 
    alf_1 <- 0
    alf_2 <- 1
  }
  #print(R)
  #print(h)
  
  #  this provides input for rebmain function and det model
  current_state <- mat_state[i, 1:6]
  rebmain_state <- rebmain_state_fun(initial_state = current_state)
  #  this inputs into det model which outputs next state at i + 1
  mat_state[(i + 1), 1:6] <-      det_model(current_state = rebmain_state,
                                       transition_mat = dtmc)
  #print(mat_state)
  }  #  end of for
  #  let's return an output that looks like our previous condition_df
  condition_df <- data.frame( "timesteps" = seq(0, timesteps, 1),
                              "n" = mat_state[, 1],
                              "a" = mat_state[, 2], "b" = mat_state[, 3],
                              "c" = mat_state[, 4], "d" = mat_state[, 5],
                              "e" = mat_state[, 6])
  return(condition_df)
}  #  end of function
  
