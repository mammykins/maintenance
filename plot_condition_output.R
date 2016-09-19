# GGPLOT COLOURS and DATA RESHAPE ------------------------------------------------------------------
#library(ggthemes)
#library(RColorBrewer)
#library(GGally)
#library(gridExtra)

ifelse(test = colour_blind_mode == TRUE,
       yes = colours <- c("#8c510a", "#d8b365", "#f6e8c3", "#A65628",
                          "#c7eae5", "#5ab4ac", "#01665e"),
       no = colours <- RColorBrewer::brewer.pal(7, "Set1")  #  including total, 7 colours required
)


#  We could do with reshaping the data to make it easier to handle
#  See here: http://www.r-bloggers.com/the-reshape-function/
long_condition_df <-  reshape(condition_df, varying = 2:8, v.names = "Count",
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
  #ggtitle(relevant_title) +
  labs(x = "Timestep", y = "GIFA") +
  scale_colour_manual(values = colours, name = "Condition",
                      labels = c("A", "B", "C", "D", "E", "New", "Total")) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_text())

#p1 + theme_bw()

plot1 <- p1 + ggthemes::theme_tufte() + theme(plot.title = element_text(size = 10, face = "bold"))

# GGSAVE OR OUTPUT ----------------------------------------------------------
gridExtra::grid.arrange(plot1, ncol = 1)