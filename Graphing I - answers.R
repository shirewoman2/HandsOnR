# Graphing I: Intro to ggplot2

# This script goes over the basics of graphing with the package ggplot2.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# Scatter plots ----------------------------------------------------

# Data we'll use:
data(Metformin)
names(Metformin)

# Renaming to give easier-to-use variables
names(Metformin) <- c("SampleID", "Matrix", "MET.wk", "VolWk", "MET.peak", 
                      "d6MET.peak")

# Making those two extra columns that we'll need.
Metformin <- Metformin %>% 
      mutate(MET.mass = MET.wk * VolWk,
             MET.PAR = MET.peak / d6MET.peak)

# 1
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point()

# 2
ggplot(Metformin, aes(x = MET.mass, y = MET.PAR, color = Matrix)) +
      geom_point() + geom_line()

## Error bars
# 3
METmeans <- Metformin %>% group_by(MET.mass) %>% 
      summarize(MET.PAR.mean = mean(MET.PAR),
                MET.PAR.sd = sd(MET.PAR))

# 4
ggplot(METmeans, aes(x = MET.mass, y = MET.PAR.mean)) +
      geom_point() + geom_line() +
      geom_errorbar(aes(ymax = MET.PAR.mean + MET.PAR.sd,
                        ymin = MET.PAR.mean - MET.PAR.sd),
                    width = 0.25)



# Histograms ----------------------------------------------------------

data(Students)
str(Students)

# 5
ggplot(Students, aes(x = VampTV.hr)) +
      geom_histogram()

# 6
ggplot(Students, aes(x = VampTV.hr)) +
      geom_histogram(bins = 10)


# 7
ggplot(Students, aes(x = VampTV.hr)) +
      geom_histogram() +
      facet_wrap(~ Department)

# 8 
data(Candidates)
SmallDonors <- Candidates %>% filter(amount < 1000 & amount > 0)
ggplot(SmallDonors, aes(x = amount, fill = party)) +
      geom_histogram(bins = 20) + 
      facet_wrap(~ party)



# Boxplots -----------------------------------------------------------

# 9
ggplot(Students, aes(y = Sleep.hr)) +
      geom_boxplot()

# 10
ggplot(Students, aes(x = Cookie, y = Sleep.hr)) +
      geom_boxplot()

# 11
ggplot(Students, aes(x = Cookie, y = Sleep.hr, fill = Cookie)) +
      geom_boxplot()

# 12
Students <- Students %>% 
      mutate(Cookie = factor(Cookie, levels = c("molasses spice", 
                                                "chocolate chip", 
                                                "oatmeal raisin", 
                                                "peanut butter")))
ggplot(Students, aes(x = Cookie, y = Sleep.hr, fill = Cookie)) +
      geom_boxplot()


# 13
ggplot(SmallDonors, aes(y = amount, x = jurisdiction_type)) +
      geom_boxplot()



# Optional advanced material --------------------------------------------

MyFit <- stdCurve(Metformin, normPeak = "MET.PAR", nominal = "MET.mass", 
                  poly = "2nd")
FittedCurve <- MyFit[["Curve.DF"]]

ggplot(Metformin, aes(x = MET.mass, y = MET.PAR)) +
      geom_point() +
      geom_line(data = FittedCurve)

names(MyFit)

str(MyFit$Fit)
# or, equivalently
str(MyFit[["Fit"]])
# These are the results of a nonlinear regression. Try this: 
summary(MyFit$Fit)
# We'll cover how to extract and interpret the typical output from a nonlinear
# regression in the statistics class, but the column "Estimate" contains the
# estimates of the betas for the fit, and the column titled "Pr(>|t|)" is the
# associated p value.


# This function has already made a scatter plot for you:
MyFit$CurvePlot

# You can modify this just as you modify other ggplot2 graphs:
MyFit$CurvePlot + xlab("Metformin nominal mass (ng)") +
      ylab("MET/d6-MET peak area ratio")





