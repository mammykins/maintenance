
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

# DATA --------------------------------------------------------------------
source("01_rebuild_maintenance.R")

# USER INPUT --------------------------------------------------------------

build_type_of_interest <- "built_pre_1919"
timesteps <- 1  #  forecast horizon, numer of time steps to simulate

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

# FUNCTIONS ---------------------------------------------------------------

#  UPDATE CURRENT STATES to aid iteration
current_state <- initial_state
h <- current_state[6]*r # £ to rebuild all current decommissioned type building at time zero. 
#  need to update iteratively or incorporate into each

if (R > current_state[6]*r) {  #  £ cost of bringing e to a
  #  money surplus for rebuilding b, c, d after rebuilding all e
  alf_1 <- 1  
  alf_2 <- 0
}  else {
  #  when there is no surplus after attempting rebuild of all e 
  alf_1 <- 0
  alf_2 <- 1
}


# ONE TIME STEP HYBRID ----------------------------------------------------
# TEST
rebmain_state <- c(
  n_rebmain(),
  a_rebmain(),
  b_rebmain(),
  c_rebmain(),
  d_rebmain(),
  e_rebmain()
)

det_model(rebmain_state, dtmc)

# SIMULATION --------------------------------------------------------------
# Dont run yet
condition_df <- data.frame( "timestep" = numeric(),
                            "n" = numeric(),
                            "a" = numeric(), "b" = numeric(),
                            "c" = numeric(), "d" = numeric(),
                            "e" = numeric(),
                            stringsAsFactors = FALSE)
for (i in 0:timesteps) {
  newrow <- as.list(c(i, round(as.numeric(initial_state * dtmc ^ i), 0)))
  condition_df[nrow(condition_df) + 1, ] <- newrow
}

condition_df <- condition_df %>%
  mutate(total = n + a + b + c + d + e) 
