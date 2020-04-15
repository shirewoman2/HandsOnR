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

# Let's add some meta data to make things interesting. Really, it would be even
# more interesting if some of the PK parameters depended on these traits, but
# I'm not going to do that for now.
Subjects <- tibble(SubjectID = 101:150,
                   Sex = "male") %>% 
      mutate(Age = rnorm(nrow(.), 35, 5),
             Occupation = sample(c("scientist", "politician", "truck driver",
                                   "nurse", "police officer", "student"), 
                                 nrow(.), replace = TRUE))
Subjects$Sex[sample(1:nrow(Subjects), 10)] <- "female"
# Making men over represented.

# We're going to be adding some random noise to each observation, so we'll set
# the seed so that we all get the same results every time we do this.
set.seed(206)

# Setting up our subjects and adding some inter-subject variability to our PK
# parameters. Vd and k will vary between subjects, and then there will be some
# noise that we'll add in a bit for each of the concentration measurements due
# to limitations of the assay, imperfect timing of blood draws, etc.
StudyDays <- expand.grid(Drug = c("A", "B"), 
                         DoseRoute = c("IV", "SC"),
                         SubjectID = c(101:150),
                         stringsAsFactors = FALSE) %>% 
      left_join(PKparams_mean) %>% 
      mutate(k = 10^(rnorm(nrow(.), 1, 0.1))/10 * 
                   k_meanpop,
             # trying to make a skew distribution without having *too* much
             # variation, which is challenging!
             Vd = 10^(rnorm(nrow(.), 1, 0.1))/10 * 
                   Vd_meanpop)

# Checking that that's a reasonable amount of variability
StudyDays %>% group_by(Drug, DoseRoute) %>%
      summarize(Min_k = min(k), Max_k = max(k),
                Min_Vd = min(Vd), Max_Vd = max(Vd))

ParamCheck <- StudyDays %>%
      gather(key = Param, value = Value, -Drug, -DoseRoute,
             -SubjectID, -k_meanpop, -Vd_meanpop)

ggplot(ParamCheck %>% filter(Param == "k"),
       aes(x = Value, fill = DoseRoute)) +
      geom_histogram(color = "white", bins = 15) +
      facet_wrap(DoseRoute ~ Drug, scales = "free") +
      ggtitle("k")

ggplot(ParamCheck %>% filter(Param == "Vd"),
       aes(x = Value, fill = DoseRoute)) +
      geom_histogram(color = "white", bins = 15) +
      facet_wrap(DoseRoute ~ Drug, scales = "free") +
      ggtitle("Vd")

# Now, let's use those starting data to make up some concentration-time data. At
# each study day, they had blood draws at the times listed.
Times <- expand.grid(Drug = c("A", "B"), 
                     DoseRoute = c("IV", "SC"),
                     SubjectID = c(101:150), 
                     TimeHr = c(0, 0.5, 1, 2, 4, 8, 10, 12, 24), # hr
                     stringsAsFactors = FALSE) 

# Calculating the concentration at each point: C = D/V * exp(-kt))
ConcTime <- StudyDays %>% left_join(Subjects) %>% 
      left_join(Times) %>% left_join(StudyDesign) %>% 
      mutate(Concentration = 
                   Dose/Vd * exp(-k * TimeHr) * # units become ng/mL
                   rnorm(nrow(.), 1, 0.025)) # adding variability to measurements 

# Checking that fake data look reasonable ------------------------------------
windows()
ggplot(ConcTime %>% filter(SubjectID == 101),
       aes(x = TimeHr, y = Concentration, color = DoseRoute, shape = Drug)) +
      geom_point() + geom_line()


# Looking at all the data together
ConcTime_mean <- ConcTime %>%
      group_by(Drug, DoseRoute, TimeHr) %>%
      summarize(MeanConc = mean(Concentration),
                SDConc = sd(Concentration))

ggplot(ConcTime_mean, aes(x = TimeHr, y = MeanConc,
                          ymin = MeanConc - SDConc,
                          ymax = MeanConc + SDConc,
                          color = DoseRoute, shape = Drug)) +
      geom_point() + geom_line() +
      geom_errorbar() +
      scale_y_log10() +
      facet_grid(DoseRoute ~ Drug)


# graphs -------------------------------------------------------------

ggplot(ConcTime, aes(x = TimeHr, y = Concentration, 
                     color = Drug)) +
      geom_point(alpha = 0.1, 
                 position = position_jitter(width = 0.15, height = 0)) +
      geom_line(data = ConcTime_mean, aes(x = TimeHr, y = MeanConc), 
                size = 1.5) +
      scale_y_log10() +
      scale_color_manual(values = c(rgb(15, 111, 198, maxColorValue=255),
                                    rgb(124, 202, 98, maxColorValue=255))) +
      facet_grid(. ~DoseRoute) +
      xlab("Time (hr)") + ylab("Plasma drug concentration (ng/mL)") +
      scale_x_continuous(breaks = seq(0, 24, 4)) +
      geom_hline(yintercept = 2, color = "gray40", linetype = "dotted") +
      annotate("text", x = 1, y = 2, label = "LLOQ", vjust = -0.5, color = "gray40")
ggsave("Concentration-time data example.png", width = 8, height = 4, dpi = 600)


# This is just to show an example of a violin plot and how to incorporate
# symbols and subscripts into axis titles; pretend that we're plotting AUC data
# instead of Vd. 
ggplot(StudyDays %>% left_join(Subjects) %>% 
             mutate(Sex = recode(Sex, "female" = "female, n = 10",
                                 "male" = "male, n = 40")),
       aes(x = DoseRoute, y = Vd, fill = Sex)) +
      geom_violin(color = "white", 
                  scale = "count") +
      scale_fill_manual(values = c(rgb(6, 39, 71, maxColorValue=255),
                                   rgb(102, 132, 170, maxColorValue=255))) +
      ylab(expression(AUC[0~to~infinity]~(mg/mL%*%min))) +
      xlab("Dose administration route")
ggsave("Violin plot example.png", width = 6, height = 4, dpi = 600)


# Saving the data for use elsewhere -------------------------------------

save(ConcTime, file = "Concentration-time data to play with.RData")


