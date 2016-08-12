
# PURPOSE -----------------------------------------------------------------

#  Run the simulation based on user input
#  Output a dataframe and ggplot
#  Try purrr to iterate

# SETUP -------------------------------------------------------------------
rm(list = ls())
#  setwd("C:/Users/mammykins/Documents/GitHub/rebmaindeR/")
setwd("C:/Users/mammykins/Documents/GitHub/maintenance/")

#getwd()

# LIBRARY -----------------------------------------------------------------
library(dplyr)
library(markovchain)
library(purrr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(GGally)
library(gridExtra)

# USER INPUT --------------------------------------------------------------

build_type_of_interest <- "built_total"
timesteps <- 10  #  forecast horizon, numer of time steps to simulate

# GET RELEVANT DATA AND REBMAINDER FUNCTION -------------------------------------------------------

source("00_getdata.R")
source("rebmainder_all_in_one.R")  #  Call the r
#  levels(pdsp_data$Building_Type)  #  build_type_of_interest potential input

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

# TIDY ENVIRONMENT --------------------------------------------------------
# clear junk from 00_getdata.R, as incorporated into dtmc

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

# OTHER USER INPUTS ------------------------------------------------------------

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

# PARAMETERS (OPTIONAL USER INPUT) --------------------------------------------------------------
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

# USE FUNCTION ------------------------------------------------------------

condition_df <- rebmainder(starting_state = initial_state, timesteps = 10,
                           rebuild_investment = R,
                           maintenance_investment = M,
                           rebuild_cost_back_to_new = r,
                           discount_rate_rebuild = dis_r, discount_rate_maintain = dis_m, 
                           prop_maint_b = x_b, prop_maint_c = x_c, prop_maint_d = x_d,
                           prop_rebuild_b = y_b, prop_rebuild_c = y_c, prop_rebuild_d = y_d,
                           cost_b_to_a = cost_b, cost_c_to_a = cost_c, cost_d_to_a = cost_d, transition_matrix = dtmc)

# GGPLOT COLOURS and DATA RESHAPE ------------------------------------------------------------------
colours <- brewer.pal(7, "Set1")  #  including total, 7 colours required
colours_cb <- c("#8c510a", "#d8b365", "#f6e8c3", "#A65628",
                "#c7eae5", "#5ab4ac", "#01665e")

#  We could do with reshaping the data to make it easier to handle
#  See here: http://www.r-bloggers.com/the-reshape-function/
long_condition_df <- reshape(condition_df, varying = 2:8, v.names = "Count",
                             timevar = "condition", times = names(condition_df)[2:8],
                             idvar = "Condition ID",
                             direction = "long") %>%
  filter(condition != "total")  #  Let's remove total as its uninformative

# PLOT DETAILS ------------------------------------------------------------
#  need a sensible syntactic title tailored to the user input, remove underscores
relevant_title <- paste("Predicted deterioration of the school estate",
                        "\n", build_type_of_interest) %>%
  gsub(pattern = "_", replacement = " ")

p1 <- ggplot() + geom_line(aes(y = Count, x = timestep, colour = condition), size = 1.5,
                           data = long_condition_df, stat = "identity") +
  ggtitle(relevant_title) + labs(x = "Timestep", y = "GIFA") +
  scale_colour_manual(values = colours_cb, name = "Condition",
                      labels = c("A", "B", "C", "D", "E", "New", "Total")) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_text())

#p1 + theme_bw()

plot1 <- p1 + theme_tufte() + theme(plot.title = element_text(size = 10, face = "bold"))

# GGSAVE OR OUTPUT ----------------------------------------------------------
grid.arrange(plot1, ncol = 1)

#merge all three plots within one grid (and visualize this)
#grid.arrange(plot1, plot2, plot3, nrow = 3) #arranges plots within grid

#save
#g <- arrangeGrob(plot1, plot2, plot3, nrow = 3) #generates g
#ggsave(file = "whatever.pdf", g) #saves g

