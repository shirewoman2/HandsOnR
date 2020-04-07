# Graphing II: Modifying the appearance of graphs

# This script builds on the basic graphs we made in Graphing I and modifies
# their appearance in a variety of ways.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)


# Adjusting the appearance of the graph ----------------------------------

# 14 We're going to be adding components to this graph for each of the next few
# questions, so I'm going to store the basic building blocks of the graph as
# their own object G rather than copying and pasting everything each time.
G <- ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix, 
                           shape = Matrix)) +
      geom_point() + geom_line() +
      scale_shape_manual(values = c(17, 8, 15)) +
      scale_color_manual(values = c("red", "blue", "green"))
G

# 15
G <- G + ylab("Peak area ratio") + xlab("Metformin nominal mass (ng)")
G      


# 16
G <- G + labs(color = "Blank plasma source", 
              shape = "Blank plasma source")
G

# 17a
G + xlim(c(0, 5)) + ylim(c(0, 2.5))

# 17b
G + coord_cartesian(xlim = c(0, 5), ylim = c(0, 2.5))

# 18a
Donations <- Candidates %>% filter(amount > 0)
ggplot(Donations, aes(x = party, y = amount)) +
      geom_boxplot() +
      scale_y_log10()

# 18b
ggplot(Donations, aes(x = party, y = amount)) +
      geom_boxplot() +
      coord_trans(y = "log10")

# 19a
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point() +
      stat_smooth(method = lm)

# 19b
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix)) +
      geom_point() +
      stat_smooth(method = lm)

# 20
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix)) +
      geom_vline(xintercept = 1, color = "red", linetype = "dotted") +
      geom_point() +
      stat_smooth(method = lm) +
      annotate("text", x = 5, y = 10, 
               label = paste("r =", 
                             round(cor(Metformin$MET.mass, Metformin$MET.PAR), 2)),
               color = "blue")

# 21
ggplot(Students, aes(x = Sleep.hr, y = VampTV.hr, color = Cookie)) +
      geom_point() +
      facet_grid(Gender ~ Department)

