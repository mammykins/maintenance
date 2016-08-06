
# PURPOSE -----------------------------------------------------------------

#  This script should act as a package precursor by developing and testing the functions
#  necessary for he maintenace and rebuild, this happen first then the deterioration can
#  be applied simultaneously and after.

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


# DET FUNCTION ------------------------------------------------------------

det_model <- function(current_state, transition_mat) {
  (current_state * transition_mat)[1:6]  #  extract values and ignore names
}


# TIDY ENVIRONMENT --------------------------------------------------------
# clear junk from "00_getdata.R", as incorporated into dtmc

rm(
  list = c(paste0("a", letters[1:5]),
           paste0("b", letters[1:5]),
           paste0("c", letters[1:5]),
           paste0("d", letters[1:5]),
           paste0("e", letters[1:5]),
           paste0(letters[1:5], "n"),
           paste0("n", letters[1:5]),
           "nn")
           )
