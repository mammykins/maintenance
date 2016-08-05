
# PURPOSE -----------------------------------------------------------------

#  To develop functions for the decision model
#  Rebuild and maintenance
#  We need initial state, then we need functions to describe change through one timeperiod
#  So that we can iterate

# LIBRARY -----------------------------------------------------------------
library(dplyr)

# DATA --------------------------------------------------------------------
source("00_getdata.R")

# USER INPUT --------------------------------------------------------------

build_type_of_interest <- "built_pre_1919"
t <- 1  #  forecast horizon, numer of time steps to simulate
R <- 1500e6 #  £m invested in rebuilding the buildings at time t
main_cash <- 1500e6  #  £m maintenance cash
revenue <- 1234e6  #  £m revenue of the school
fixed_main_cost <- 1288e6  #  £m Fixed maitanence cost
infl_rate <- 1  #  inflation rate, should be a vector

# INITIAL STATE --------------------------------------------------------------
initial_state <- pdsp_data %>%
  filter(Building_Type == build_type_of_interest) %>%
  select(-Building_Type, -ends_with("Count")) %>%
  as.numeric()  #  Drop building type, remove columns and names

# PARAMETERS --------------------------------------------------------------

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

h <- initial_state[6]*r # £ to rebuild all current decommissioned type building at time zero. 

######-note-#######-----------h should be product of r and current numebr of e but not initial_state[6]----------------------------

# FUNCTIONS ---------------------------------------------------------------

#  need to update iteratively

  if (R > h) {
    #  money surplus for rebuilding b,c,d after rebuilding all decomissioned buildings
    alf_1 <- 1  
    alf_2 <- 0
  }  else {
    #  when there is no surplus after attempting rebuild of all decomissioned buildings 
    alf_1 <- 0
    alf_2 <- 1
  }

#  UPDATE CURRENT STATES
# WARNING DELETE THIS ASSIGNATION AFTER TESTING
current_state <- initial_state
#  see Binary programming.doc by Bolun Wang or her thesis for full details
#  should add warning and error messages given more time

n_rebmain <- function(rebuild_monies = R, rebuild_cost = r,
                      current_n = current_state[1] ) { 
  (rebuild_monies / rebuild_cost) + current_n
}

# A
a_rebmain <- function(maint_monies = M,
                       x1 = x_b, x2 = x_c, x3 = x_d,
                       cb = cost_b, cc = cost_c, cd = cost_d,
                       current_a = current_state[2]) {
  current_a + 
    ((x1 * maint_monies) / cb) + 
    ((x2 * maint_monies) / cc) + 
    ((x3 * maint_monies) / cd)
}


# B
b_rebmain <- function(alf_1. = alf_1,
                      alf_2. = alf_2, 
                      b = current_state[3],
                      y1 = y_b,
                      R. = R,
                      h. = h,
                      x1 = x_b,
                      M. = M,
                      cb = cost_b) {
  
  (alf_1. * (b - (y1 * (R. - h.))/ r )) +
  (alf_2. * (b - ((x1 * M.) / cb)))
}


# C
c_rebmain <- function(alf_1. = alf_1,
                      alf_2. = alf_2,
                      c = current_state[4],
                      y2 = y_c,
                      R. = R,
                      h. = h,
                      x2 = x_c,
                      M. = M,
                      cc = cost_c) {
  
  (alf_1. * (c - y2 * (R. - h.)/ r )) +
  (alf_2. * (c - ((x2 * M.)/ cc)))
}

# D
d_rebmain <- function(alf_1. = alf_1,
                      alf_2. = alf_2,
                      d= current_state[5],
                      y3 = y_d,
                      R. = R,
                      h. =h,
                      x3 = x_d,
                      M. = M,
                      cd = cost_d) {
  
  (alf_1. * (d - y3 * (R. - h.)/ r )) + 
  (alf_2. * (d - ((x3 * M.)/ cd)))
}

# E
e_rebmain <- function(alf_1. = alf_1,
                      alf_2. = alf_2,
                      e = current_state[6],
                      R. = R,
                      r. = r) {
  (alf_1. * e) + (alf_2. * (R. /r.))
}
