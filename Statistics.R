# Statistics

# This script offers examples of how to do some of the most-common statistical
# tests.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)


# Summary statistics -------------------------------------------------------

# 1
summary(Candidates$amount)

# 2
mean(Candidates$amount)
sd(Candidates$amount)

# 3
?mean_sd
mean_sd(Candidates$amount, calcMedian = TRUE, reportn = TRUE)

# 4
Candidates %>% group_by(party) %>% 
      summarize(Contributions = mean_sd(amount, calcMedian = TRUE, 
                                        reportn = TRUE))


# Student's t tests and ANOVAs ------------------------------------------

# 1
DandR <- Candidates %>% filter(party %in% c("DEMOCRAT", "REPUBLICAN"))
(MyTest <- t.test(amount ~ party, data = DandR))
# Putting parentheses around the ENTIRE command prints that new object you've
# created

# 2
MyTest$p.value

# 3
str(MyTest)
MyTest$estimate


# 4
DRL <- Candidates %>% filter(party %in% c("DEMOCRAT", "REPUBLICAN",
                                          "LIBERTARIAN"))
Party.aov <- aov(amount ~ party, data = DRL)
summary(Party.aov)

# 5
TukeyHSD(Party.aov)

# 6 (bonus)
tukeyStar(DRL, groupColumn = "party", valueColumn = "amount")
# This is pretty hard to see b/c the values range so much. Let's try log
# transforming the y axis.
tukeyStar(DRL, groupColumn = "party", valueColumn = "amount") +
      scale_y_log10()
# When we cover graphing, I'll explain more about how this works. For now, just
# know that the graph we made with tukeyStar() is a ggplot2 graph and we can
# thus work with it just like we work with other ggplot2 graphics. Don't know
# what that means? Don't worry; we'll cover it in the graphing sessions.


# Linear and nonlinear regression ------------------------------------------






