# Concentration-time data

# This script makes fake concentration-time data to play with. 

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# Generating fake data -------------------------------------------------------

# Let's say we have 50 subjects in a PK study, say that each subject got drug A
# and drug B, and say they got it both IV and SC, so there were 4 study days.

# Let's make drug A and drug B have different elimination rates. Let's also make
# the two dosing routes -- IV and SC -- have different volumes of distribution.
StudyDesign <- tibble(Drug = c("A", "B", "A", "B"),
                      Dose = 250, # mg
                      DoseRoute = c("IV", "IV", "SC", "SC"))

PKparams_mean <- tibble(Drug = rep(c("A", "B"), 2), 
                        DoseRoute = rep(c("IV", "SC"), each = 2),
                        k_meanpop = c(rep(c(0.02, 0.04), 2)), # hr^-1
                        Vd_meanpop = c(12, 12, 15, 15))

# We're going to be adding some random noise to each observation, so we'll set
# the seed so that we all get the same results every time we do this.
set.seed(206)

# Setting up our subjects and adding some inter-subject variability to our PK
# parameters. Vd and k will vary between subjects, and then there will be some
# noise that we'll add in a bit for each of the concentration measurements due
# to limitations of the assay, imperfect timing of blood draws, etc.
Subjects <- expand.grid(Drug = c("A", "B"), 
                        DoseRoute = c("IV", "SC"),
                        SubjectID = c(101:150),
                        stringsAsFactors = FALSE) %>% 
      left_join(PKparams_mean) %>% 
      mutate(k = rnorm(nrow(.), 1, 0.1), 
             Vd = rnorm(nrow(.), 1, 0.1))

# # Checking that that's a reasonable amount of variability
# Subjects %>% group_by(Drug, DoseRoute) %>% 
#       summarize(Meank = mean_sd(k, calcRange = TRUE), 
#                 MeanVd = mean_sd(Vd, calcRange = TRUE))

# Now, let's use those starting data to make up some concentration-time data. At
# each study day, they had blood draws at the times listed.
Times <- expand.grid(Drug = c("A", "B"), 
                     DoseRoute = c("IV", "SC"),
                     SubjectID = c(101:150), 
                     TimeHr = c(0, 0.5, 1, 2, 4, 8, 10, 12, 24), # hr
                     stringsAsFactors = FALSE) 

# Calculating the concentration at each point: C = D/V * exp(-kt))
ConcTime <- Subjects %>% 
      left_join(Times) %>% left_join(StudyDesign) %>% 
      mutate(Concentration = 
                   Dose/Vd * exp(-k * TimeHr) * rnorm(nrow(.), 1, 0.2)) # units become ng/mL

# # Checking that this looks reasonable      
# ggplot(ConcTime %>% filter(SubjectID == 101), 
#        aes(x = TimeHr, y = Concentration, color = DoseRoute, shape = Drug)) +
#       geom_point() + geom_line() +
#       scale_y_log10()
#       
# 
# # Looking at all the data together
# ggplot(ConcTime, aes(x = TimeHr, y = Concentration, color = SubjectID,
#                      shape = Drug)) +
#       geom_point(alpha = 0.5) + 
#       scale_y_log10() +
#       facet_grid(DoseRoute ~ Drug)

# Let's add some meta data to make things interesting. 
Meta <- Subjects %>% select(SubjectID) %>%
      mutate(Sex = sample(c("female", "male"), nrow(.), replace = TRUE),
             Age = rnorm(nrow(.), 35, 5),
             Occupation = sample(c("scientist", "politician", "truck driver",
                                   "nurse", "police officer", "student"), 
                                 nrow(.), replace = TRUE))

ConcTime <- ConcTime %>% left_join(Meta)


