
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

# USER INPUT --------------------------------------------------------------

build_type_of_interest <- "built_pre_1919"
timesteps <- 10  #  forecast horizon, numer of time steps to simulate

# GET RELEVANT DATA -------------------------------------------------------

source("00_getdata.R")
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

# OTHER INPUTS ------------------------------------------------------------

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

# SIMULATION --------------------------------------------------------------
#  This function is too long, it should be borken down into smaller functions
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

p1 <- ggplot() + geom_line(aes(y = Count, x = timestep, colour = condition), size = 2,
                           data = long_condition_df, stat = "identity") +
  ggtitle(relevant_title) + labs(x = "Timestep", y = "Count") +
  scale_colour_manual(values = colours_cb, name = "Condition",
                      labels = c("A", "B", "C", "D", "E", "New", "Total")) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_text())

#p1 + theme_bw()

plot1 <- p1 + theme_tufte() + theme(plot.title = element_text(size = 10, face = "bold"))

# GGSAVE ----------------------------------------------------------
grid.arrange(plot1, ncol = 2)

#merge all three plots within one grid (and visualize this)
#grid.arrange(plot1, plot2, plot3, nrow = 3) #arranges plots within grid

#save
#g <- arrangeGrob(plot1, plot2, plot3, nrow = 3) #generates g
#ggsave(file = "whatever.pdf", g) #saves g

