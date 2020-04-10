# Graphing I: Intro to ggplot2

# This script goes over the basics of graphing with the package ggplot2.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# Loading and tidying data ------------------------------------------------

data(Metformin)
names(Metformin) <- c("SampleID", "Matrix", "MET.wk", "VolWk", "MET.peak",  "d6MET.peak")

Metformin <- Metformin %>% 
      mutate(MET.mass = VolWk * MET.wk, 
             MET.PAR = MET.peak / d6MET.peak)

# 1
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point()

# 2
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix)) +
      geom_point() + geom_line()


# 3
METmean <- Metformin %>% 
      group_by(MET.mass) %>% 
      summarize(MET.PAR.mean = mean(MET.PAR),
                MET.PAR.sd = sd(MET.PAR))

# 4
ggplot(METmean, aes(x = MET.mass, y = MET.PAR.mean)) +
      geom_point() + geom_line() +
      geom_errorbar(aes(ymin = MET.PAR.mean - MET.PAR.sd, 
                        ymax = MET.PAR.mean + MET.PAR.sd))


# Histograms ----------------------------------------------------------------

# 5. Using the Students data.frame, make a histogram to show the distribution of
# hours watching trashy vampire TV.
data(Students)
str(Students)

# 6
ggplot(Students, aes(x = VampTV.hr)) +
      geom_histogram(bins = 10, color = "white")

# 7
ggplot(Students, aes(x = VampTV.hr)) +
      geom_histogram(bins = 10, color = "white") +
      facet_wrap(. ~ Department)

ggplot(Students, aes(x = VampTV.hr)) +
      geom_histogram(bins = 10, color = "white") +
      facet_grid(Gender ~ Cookie)

# boxplot --------------------------------------------------------------

# 9
ggplot(Students, aes(y = Sleep.hr)) +
      geom_boxplot()

# 10 & 11
ggplot(Students, aes(y = Sleep.hr, x = Cookie, fill = Cookie)) +
      geom_boxplot()


# 12

# Change the order of the x-axis to molasses spice, chocolate chip, oatmeal raisin, peanut butter rather than the default
ggplot(Students %>% 
             mutate(Cookie = factor(Cookie, 
                                    levels  = c("molasses spice", "chocolate chip",
                                                "oatmeal raisin", "peanut butter"))), 
       aes(y = Sleep.hr, x = Cookie, fill = Cookie)) +
      geom_boxplot()












