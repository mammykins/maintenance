
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
#

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
    
    h <- mat_state[i, 6]*rebuild_cost_back_to_new # ? to rebuild all current decommissioned 
    #  need to update iteratively, hence inclusion here
    
    #  Check lexical scoping, R searches within the function
    if (rebuild_investment > h) {  #  ? cost of bringing e to a
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
    #  See page 16 of Bolun Wang's thesis for details
    
    #GIFA N------------------------------
    # Cast an upper ceiling on GIFA N: increase in GIFA N should not exceed sum of GIFA B, C, D, E
    max_to_rebuild <-  (current_state[3] + current_state[4] + current_state[5] + current_state[6])
    
    ifelse(((rebuild_investment[i] / (rebuild_cost_back_to_new / discount_rate_rebuild)) > max_to_rebuild),
           n <- max_to_rebuild + current_state[1], #  yes
           n <- (rebuild_investment[i] / (rebuild_cost_back_to_new / discount_rate_rebuild)) + current_state[1] # no
           )
    
    #GIFA A------------------------------
    #  Cast an upper ceiling on GIFA A: increase in GIFA A should not exceed sum of GIFA B, C, D
    max_to_main <- (current_state[3] + current_state[4] + current_state[5])
    
    #  Ideal amount to be maintained
    a0 <- ((prop_maint_b * maintenance_investment[i]) / (cost_b_to_a / discount_rate_maintain)) + 
      ((prop_maint_c * maintenance_investment[i]) / (cost_c_to_a / discount_rate_maintain)) + 
      ((prop_maint_d * maintenance_investment[i]) / (cost_d_to_a / discount_rate_maintain))
    
    ifelse(a0 > max_to_main,  #  test
           a <- max_to_main + current_state[2],  #  yes
           a <- a0 + current_state[2])  #  no
    
    #GIFA B------------------------------
    b_r <-  (alf_1 * (current_state[3] - (prop_rebuild_b * (rebuild_investment[i] - h))/ (rebuild_cost_back_to_new / discount_rate_rebuild) )) +
      (alf_2 * current_state[3])
    
    if (b_r < 0) {
      warning("Inefficient investment; too much money is being assigned to rebuliding B, thus reducing GIFA below zero.", call. = FALSE)
      n <- n + b_r
      b_r <- 0
      }
    
    b_m <- b_r - (prop_maint_b * maintenance_investment[i] / (cost_b_to_a / discount_rate_maintain))
    if ( b_m < 0 ) {
      warning("Inefficient investment; too much money is being assigned to maintaining B, thus reducing GIFA below zero.", call. = FALSE)
      a = a + b_m
      b_m <- 0
    }
    
    #GIFA C------------------------------
    c_r = (alf_1 * (current_state[4] - (prop_rebuild_c * (rebuild_investment[i] - h))/ (rebuild_cost_back_to_new / discount_rate_rebuild) )) +
      (alf_2 * current_state[4]) 
    
    if ( c_r < 0 ) {
      warning("Inefficient investment; too much money is being assigned to rebuliding C, thus reducing GIFA below zero.", call. = FALSE)
      n = n + c_r
      c_r <- 0
    }
    c_m = c_r - (prop_maint_c * maintenance_investment[i] / (cost_c_to_a / discount_rate_maintain))
    if ( c_m < 0 ) {
      warning("Inefficient investment; too much money is being assigned to maintaining C, thus reducing GIFA below zero.", call. = FALSE)
      a = a + c_m
      c_m <- 0
    }
    
    #GIFA D------------------------------
    
    d_r = (alf_1 * (current_state[5] - (prop_rebuild_d * (rebuild_investment[i] - h))/ (rebuild_cost_back_to_new / discount_rate_rebuild) )) + 
      (alf_2 * current_state[5]) 
    
    if ( d_r < 0 ) {
      warning("Inefficient investment; too much money is being assigned to rebuliding D, thus reducing GIFA below zero.", call. = FALSE)
      n = n + d_r
      d_r <- 0
    }
    
    d_m = d_r - (prop_maint_d * maintenance_investment[i] / (cost_d_to_a / discount_rate_maintain))
    if ( d_m < 0 ) {
      warning("Inefficient investment; too much money is being assigned to maintaining D, thus reducing GIFA below zero.", call. = FALSE)
      a = a + d_m
      d_m <- 0
    }
    
    #GIFA E------------------------------
    
    e = alf_2 * (current_state[6] - (rebuild_investment[i] /(rebuild_cost_back_to_new / discount_rate_rebuild)))
    if ( e < 0 ) {
      warning("Inefficient investment; too much money is being assigned to rebuliding E, thus reducing GIFA below zero.", call. = FALSE)
      n = n + e
      e <- 0
    }
    
    rebmain_state <- c(n , a , b_m , c_m , d_m , e)
    
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



