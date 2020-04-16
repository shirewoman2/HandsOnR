# Graphing II: Modifying the appearance of graphs

# This script builds on the basic graphs we made in Graphing I and modifies
# their appearance in a variety of ways.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)
library(ggpubr)

# Loading in the Metformin data.frame, adjusting the column names, and adding
# the two calculated columns.
data(Metformin)
# Renaming to give easier-to-use variables
names(Metformin) <- c("SampleID", "Matrix", "MET.wk", "VolWk", "MET.peak", 
                      "d6MET.peak")

# Making those two extra columns that we'll need.
Metformin <- Metformin %>% 
      mutate(MET.mass = MET.wk * VolWk,
             MET.PAR = MET.peak / d6MET.peak)


# Adjusting the appearance of the graph ----------------------------------


# 1
windows() # This puts your graphs in a new window. 

ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point()

# 2
G <- ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, 
                           color = Matrix, shape = Matrix)) +
      geom_point()
G

# 3
G <- G + scale_shape_manual(values = c(17, 8, 18)) + 
      scale_color_manual(values = c("firebrick", "dodgerblue3", "forestgreen"))
G

# 4
G <- G + ylab("Peak area ratio") + xlab("Metformin nominal mass (ng)")
G      


# 5
G <- G + labs(color = "Blank plasma source", 
              shape = "Blank plasma source")
G

# 6a
G + xlim(c(0, 5)) + ylim(c(0, 2.5))

# 6b
G + coord_cartesian(xlim = c(0, 5), ylim = c(0, 2.5))

# 7a
data(Candidates)
Donations <- Candidates %>% filter(amount > 0)

ggplot(Donations, aes(x = party, y = amount)) +
      geom_boxplot() +
      scale_y_log10()

# 7b
ggplot(Donations, aes(x = party, y = amount)) +
      geom_boxplot() +
      coord_trans(y = "log10")

# 8a
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point() +
      stat_smooth(method = lm)

# 8b
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix)) +
      geom_point() +
      stat_smooth(method = lm)

# 9
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_vline(xintercept = 1, color = "red", linetype = "dotted") +
      geom_point() +
      stat_smooth(method = lm) +
      annotate("text", x = 5, y = 10, 
               label = paste("r =", 
                             round(cor(Metformin$MET.mass, Metformin$MET.PAR), 2)),
               color = "blue")

# 10
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix, 
                      group = 1)) +
      geom_vline(xintercept = 1, color = "red", linetype = "dotted") +
      geom_point() +
      stat_smooth(method = lm) +
      annotate("text", x = 5, y = 10, 
               label = paste("r =", 
                             round(cor(Metformin$MET.mass, Metformin$MET.PAR), 2)),
               color = "blue")


# Making graphing preferences consistent -----------------------------------

# 11
G <- ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, 
                           color = Matrix, shape = Matrix)) +
      geom_point()

theme_set(theme_bw()); G

theme_set(theme_linedraw()); G

theme_set(theme_classic()); G

theme_set(theme_dark()); G

# Just for fun, a few extras from the package ggthemes
library(ggthemes)
theme_set(theme_tufte()); G
theme_set(theme_economist()); G
theme_set(theme_fivethirtyeight()); G
theme_set(theme_few()); G
theme_set(theme_wsj()); G


# Let's return to the original theme for now. 
theme_set(theme_gray())

# 12
data(Students)
head(Students)

DeptColor <- c("Pceut" = "purple", "MedChem" = "gold")

# 12a
A <- ggplot(Students, aes(x = Sleep.hr, y = VampTV.hr, color = Department)) +
      geom_point() +
      scale_color_manual(values = DeptColor)
A

# 12b 
B <- ggplot(Students, aes(y = Sleep.hr, fill = Department)) +
      geom_boxplot() +
      scale_fill_manual(values = DeptColor)
B

# Arranging multiple graphs together ----------------------------------------

# 13a 
A + facet_wrap(Gender ~ Cookie)

# 13b
A + facet_grid(Gender ~ Cookie)


# 14
B + facet_wrap(~ Gender)

# Another option:
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point() +
      facet_wrap(~ Matrix)

# And another:
ggplot(Donations %>% filter(party %in% c("DEMOCRAT", "REPUBLICAN",
                                         "INDEPENDENT")), 
       aes(x = party, y = amount)) +
      geom_boxplot() +
      scale_y_log10() +
      facet_wrap(office ~ primary_general)



# 15
ggarrange(A, B, common.legend = TRUE, legend = "bottom", 
          align = "h")

# Note what happens when we switch the order:
ggarrange(B, A, common.legend = TRUE, legend = "bottom", 
          align = "h")


# 16
ggsave("MedChem and Pceut sleep graphs.png", 
       height = 4, width = 6, dpi = 600)


