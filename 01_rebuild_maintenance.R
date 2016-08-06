
# PURPOSE -----------------------------------------------------------------

#  To develop functions for the decision model
#  Rebuild and maintenance
#  We need initial state, then we need functions to describe change through one timeperiod
#  So that we can iterate


#  see Binary programming.doc by Bolun Wang or her thesis for full details
#  should add unit tests warning and error messages given more time

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
  (alf_2. * b) - (x1 * M. / cb)
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
  (alf_2. * c) - (x2 * M./ cc)
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
  (alf_2. * d) - (x3 * M./ cd)
}

# E
e_rebmain <- function(alf_1. = alf_1,
                      alf_2. = alf_2,
                      e = current_state[6],
                      R. = R,
                      r. = r) {
  (alf_1. * e) + (alf_2. * (R. /r.))  #  is this right? or should alf_2*R before divide
}

