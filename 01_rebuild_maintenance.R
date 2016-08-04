
# PURPOSE -----------------------------------------------------------------

#  To develop functions for the decision model
#  Rebuild and maintenance
#  We need initial state, then we need functions to describe change through one timeperiod

# LIBRARY -----------------------------------------------------------------
library(dplyr)

# DATA --------------------------------------------------------------------
source("00_getdata.R")

# USER INPUT --------------------------------------------------------------
i_want_gifa <- FALSE  #  if FALSE then we want Count
build_type_of_interest <- "built_pre_1919"
h <- 1  #  forecast horizon, numer of time steps to simulate


# PARAMETERS --------------------------------------------------------------



# INITIAL STATE --------------------------------------------------------------
# GIFA or COUNT of records?
if (i_want_gifa == TRUE) {
  initial_state <- filter(pdsp_data, Building_Type == build_type_of_interest) %>%
    select(-Building_Type, -contains("Count")) %>%
    as.numeric()  #  Drop building type, remove columns and names
}  else {
  initial_state <- c(0,
                     filter(pdsp_data, Building_Type == build_type_of_interest) %>%
                       select(-Building_Type, -contains("GIFA")) %>%
                       as.numeric(),
                     0)  #  stick zero either end for N and E for Count
}


# FUNCTIONS ---------------------------------------------------------------


