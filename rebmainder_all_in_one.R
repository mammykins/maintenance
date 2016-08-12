
# PURPOSE -----------------------------------------------------------------

#  We need initial state, then we need functions to describe change through one timeperiod
#  of rebuilding and maintenance then deterioration
#  So that we can iterate through timesteps
#  Let's make a big function to do all this
#  We'll build it up from smaller parts as a package later

#  see Binary programming.doc by Bolun Wang or her thesis for full details
#  should add unit tests warning and error messages given more time

# GOOD CODING -------------------------------------------------------------

#  functions verbs, arguments nouns
#  use common prefix for function names
#  make the functions pipeable


# LIBRARY -----------------------------------------------------------------

library(dplyr)  #  check this is called as we pipe and mutate

# FUNCTION REBUILD, MAINTENANCE THEN DETERIORATE --------------------------------------------------------------
#  This function is too long, it should be broken down into smaller functions?
#  Smaller chunks enhance readibility, testing and the QA process.
rebmainder <- function(starting_state, timesteps,
                       rebuild_investment, maintenance_investment,
                       rebuild_cost_back_to_new,
                       discount_rate_rebuild,
                       discount_rate_maintain, 
                       prop_maint_b, prop_maint_c, prop_maint_d,
                       prop_rebuild_b, prop_rebuild_c, prop_rebuild_d,
                       cost_b_to_a, cost_c_to_a, cost_d_to_a,
                       transition_matrix = dtmc,
                       ...) { 
  #  Test that inputs are sensible
  if (length(starting_state) != 6) {
    stop("The initial or starting state should be a 6 length vector; new build, a, b, c, d, decommisioned",
         call. = FALSE)
  }
  
  if (sum(c(prop_maint_b, prop_maint_c, prop_maint_d)) != 1) {
    stop("The proportion of maintenance funding allocation must sum to one.", call. = FALSE)
  }
  
  if (sum(c(prop_rebuild_b, prop_rebuild_c, prop_rebuild_d)) != 1) {
    stop("The proportion of rebuilding funding allocation must sum to one.", call. = FALSE)
  }  
  
  #  input the initial state from n, a, b, c, d, e as the current state at time zero
  #  create an empty matrix to hold timesteps + 1 rows and 6 columns
  #  first row is initial state
  
  mat_state <- matrix(0, nrow = (timesteps + 1), ncol = 6)  #  output
  #print(mat_state)
  mat_state[1, ] <- starting_state  #  time zero state
  #print(mat_state)
  
  for (i in 1:timesteps) {
    
    h <- mat_state[i, 6]*rebuild_cost_back_to_new # £ to rebuild all current decommissioned 
    #  need to update iteratively, hence inclusion here
    
    #  Check lexical scoping, R searches within the function
    if (rebuild_investment > h) {  #  £ cost of bringing e to a
      #  money surplus for rebuilding b, c, d after rebuilding all e
      alf_1 <- 1  
      alf_2 <- 0
    }  else {
      #  when there is no surplus after attempting rebuild all of e 
      alf_1 <- 0
      alf_2 <- 1
    }
    #print(R)
    #print(h)
    
    #  this provides input for rebmain function and det model
    current_state <- mat_state[i, 1:6]
    
    ###########################################################################################
    #REBMAIN
    #  this takes current state and rebuilds and maintains it
    #  this is used as input into det model which outputs next state at i + 1
    
    rebmain_state <- c(
     n = (rebuild_investment / (rebuild_cost_back_to_new / discount_rate_rebuild)) + current_state[1],
     
     a =   current_state[2] + 
       ((prop_maint_b * maintenance_investment) / (cost_b_to_a / discount_rate_maintain)) + 
       ((prop_maint_c * maintenance_investment) / (cost_c_to_a / discount_rate_maintain)) + 
       ((prop_maint_d * maintenance_investment) / (cost_d_to_a / discount_rate_maintain)),
     
     b =   (alf_1 * (current_state[3] - (prop_rebuild_b * (rebuild_investment - h))/ (rebuild_cost_back_to_new / discount_rate_rebuild) )) +
       (alf_2 * current_state[3]) - (prop_maint_b * maintenance_investment / (cost_b_to_a / discount_rate_maintain)),
     
     c = (alf_1 * (current_state[4] - (prop_rebuild_c * (rebuild_investment - h))/ (rebuild_cost_back_to_new / discount_rate_rebuild) )) +
       (alf_2 * current_state[4]) - (prop_maint_c * maintenance_investment / (cost_c_to_a / discount_rate_maintain)),
     
     d =   (alf_1 * (current_state[5] - (prop_rebuild_d * (rebuild_investment - h))/ (rebuild_cost_back_to_new / discount_rate_rebuild) )) + 
       (alf_2 * current_state[5]) - (prop_maint_d * maintenance_investment / (cost_d_to_a / discount_rate_maintain)),
     
     e = (alf_1 * current_state[6]) + (alf_2 * (maintenance_investment /(rebuild_cost_back_to_new / discount_rate_rebuild)))
    )
    
    #  TEST for negative numbers due to over-investment, does not stop but warns the user
    if (any(rebmain_state < 0)) {
      warning("Inefficient investment; too much money is being assigned to rebuild, thus reducing GIFA below zero for at least one state.", call. = FALSE)
    } 
    rebmain_state[rebmain_state < 0] <- 0  #  if negative GIFA replace with zero
    ###########################################################################################
    #DETERIORATION
    mat_state[(i + 1), 1:6] <- (rebmain_state * transition_matrix)[1:6]
    #print(mat_state)
  }  #  end of for
  #  let's return an output that looks like our previous condition_df
  condition_df <- data.frame( "timestep" = seq(0, timesteps, 1),
                              "n" = mat_state[, 1],
                              "a" = mat_state[, 2], "b" = mat_state[, 3],
                              "c" = mat_state[, 4], "d" = mat_state[, 5],
                              "e" = mat_state[, 6])
  
  df <- condition_df %>%
    mutate(total = n + a + b + c + d + e)  #  sanity check
  
  return(df)
}  #  end of function



