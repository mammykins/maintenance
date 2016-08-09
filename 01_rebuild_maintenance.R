
# PURPOSE -----------------------------------------------------------------

#  To develop functions for the decision model
#  Rebuild and maintenance
#  We need initial state, then we need functions to describe change through one timeperiod
#  So that we can iterate


#  see Binary programming.doc by Bolun Wang or her thesis for full details
#  should add unit tests warning and error messages given more time

n_rebmain <- function(rebuild_monies = R, rebuild_cost = r,
                      current_n = current_state[1], dr = dis_r ) { 
  (rebuild_monies / (rebuild_cost / dr)) + current_n
}

# A
a_rebmain <- function(maint_monies = M,
                      x1 = x_b, x2 = x_c, x3 = x_d,
                      cb = cost_b, cc = cost_c, cd = cost_d,
                      current_a = current_state[2], dm = dis_m) {
  current_a + 
    ((x1 * maint_monies) / (cb / dm)) + 
    ((x2 * maint_monies) / (cc / dm)) + 
    ((x3 * maint_monies) / (cd / dm))
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
                      cb = cost_b,
                      dm = dis_m,
                      dr = dis_r) {
  
  (alf_1. * (b - (y1 * (R. - h.))/ (r / dr) )) +
    (alf_2. * b) - (x1 * M. / (cb / dm))
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
                      cc = cost_c,
                      dm = dis_m,
                      dr = dis_r) {
  
  (alf_1. * (c - y2 * (R. - h.)/ (r / dr) )) +
    (alf_2. * c) - (x2 * M./ (cc / dm))
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
                      cd = cost_d,
                      dm = dis_m,
                      dr = dis_r) {
  
  (alf_1. * (d - y3 * (R. - h.)/ (r / dr) )) + 
    (alf_2. * d) - (x3 * M./ (cd / dm))
}

# E
e_rebmain <- function(alf_1. = alf_1,
                      alf_2. = alf_2,
                      e = current_state[6],
                      R. = R,
                      r. = r,
                      dr = dis_r) {
  (alf_1. * e) + (alf_2. * (R. /(r. / dr)))  #  is this right? or should alf_2*R before divide
}


#  Combine all to create a vector of initial state reb_main'ed
rebmain_state_fun <- function(initial_state = current_state, ...) {
  #  take the current state and rebuild and maintain
  #  given the relevant policy parameters and costs inputs
#input
current_state <- initial_state
current_n <- initial_state[1]
current_a <- initial_state[2]
b <- initial_state[3]
c <- initial_state[4]
d <- initial_state[5]
e <- initial_state[6]
  
output <-  c(
  n_rebmain(),
  a_rebmain(),
  b_rebmain(),
  c_rebmain(),
  d_rebmain(),
  e_rebmain()
)
return(output)
#  the output needs to feed into the det model
}