
# PURPOSE -----------------------------------------------------------------

#  This script should act as a package precursor by developing and testing the functions
#  necessary for he maintenace and rebuild, this happen first then the deterioration can
#  be applied simultaneously and after.


# LIBRARY -----------------------------------------------------------------
library(dplyr)
library(markovchain)

# DATA --------------------------------------------------------------------
source("00_getdata.R")

# USER INPUT --------------------------------------------------------------
i_want_gifa <- FALSE
build_type_of_interest <- "built_pre_1919"
h <- 1  #  forecast horizon, numer of time steps to simulate 

# CREATE TRANSITION MATRIX ------------------------------------------------

na <- filter(tm_data, Building_Type == build_type_of_interest) %>%
  select(na)
ab <- filter(tm_data, Building_Type == build_type_of_interest) %>%
  select(ab)
bc <- filter(tm_data, Building_Type == build_type_of_interest) %>%
  select(bc)
cd <- filter(tm_data, Building_Type == build_type_of_interest) %>%
  select(cd)
de <- filter(tm_data, Building_Type == build_type_of_interest) %>%
  select(de)
ee <- filter(tm_data, Building_Type == build_type_of_interest) %>%
  select(ee)

# PROCESSING --------------------------------------------------------------
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

#  get the numbers into the appropriate matrix format

tm_deterioration <- matrix(c(1 - na[[1]], na[[1]], nb, nc, nd, ne,
                             an, 1 - ab[[1]], ab[[1]], ac, ad, ae,
                             bn, ba, 1 - bc[[1]], bc[[1]], bd, be,
                             cn, ca, cb, 1 - cd[[1]], cd[[1]], ce,
                             dn, da, db, dc, 1 - de[[1]], de[[1]],
                             en, ea, eb, ec, ee[[1]], 1 - ee[[1]]),
                           nrow = 6, byrow = TRUE) #define the transition matrix


# CREATE Discrete time Markov Chain object --------------------------------
dtmc <- new("markovchain", transitionMatrix = tm_deterioration,
            states = c("n", "a", "b", "c", "d", "e"),
            name = paste(build_type_of_interest))

# SIMULATION --------------------------------------------------------------
timesteps <- h

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