# Statistics

# This script summarizes some examples of statistical tests you can do with R.

# Housekeeping --------------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)


# Summary statistics ----------------------------------------------------

# 1 With the Candidates data.frame, look at the summary statistics of the amount
# people donated to King County candidates in 2018.

data(Candidates)
names(Candidates)

summary(Candidates$amount)

# 2. Calculate the mean and the standard deviation of contributions made to King
# County candidates in 2018.

mean(Candidates$amount)
sd(Candidates$amount)

# 3
mean_sd(Candidates$amount)

# 4 Again with attention to sig figs, do the same calculation broken down by
# party. Use tidyverse syntax.

Candidates %>% group_by(party) %>% 
      summarize(MeanSD = mean_sd(amount))


# Student's t test ----------------------------------------------------------

# 5 Perform a Student's t test to see whether Republicans or Democrats
# donated more on average with each donation. Call the t test object "MyTTest".
# Keep in mind that donations should be positive!

Donations <- Candidates %>% filter(party %in% c("DEMOCRAT", "REPUBLICAN") &
                                         amount > 0)
      
      
Stuff <- c(LETTERS[1:10])
Stuff

"E" == Stuff
"E" %in% Stuff

MyTTest <- t.test(amount ~ party, data = Donations)
MyTTest

# 6 Get just the p value for that t test.
MyTTest$p.value

# 7. Figure out what other stuff is contained in MyTest and get, say, the
# estimated means for the two groups from MyTest.

str(MyTTest)
MyTTest$conf.int

ggplot(Donations, aes(x = amount, fill = party)) +
      geom_histogram(bins = 15) +
      facet_wrap(~ party)



# ANOVA -------------------------------------------------------------

# 8. Now that we've done a t test, let's expand and do an ANOVA with basically
# the same question. Let's limit the parties we include to Democrats,
# Republicans, non partisans just so we're not talking about tons of
# comparisons. Make a new object, "Party.aov", that is the result of an ANOVA
# asking, "Are there any differences between political parties in terms of
# average donation?"

Donations <- Candidates %>% filter(party %in% c("DEMOCRAT", "REPUBLICAN", 
                                                "NON PARTISAN") &
                                         amount > 0)
MyAOV <- aov(amount ~ party, data = Donations)
MyAOV

# 9
summary(MyAOV)


# 10. Looks like there definitely is a difference! (See the column "Pr(>F)" for
# the p value for differences based on party.) Which of the parties differ from
# each other? Do a Tukey post-hoc test on Party.aov to see.
TukeyHSD(MyAOV)

# Linear regression ----------------------------------------------------

# 12. Using the standard curve data ExStdCurve, perform a linear regression of
# the MET/d6-MET peak area ratio given the nominal mass of metformin added. Call
# the output of that linear model "MyLM".

data(ExStdCurve)
head(ExStdCurve)

MyLM <- lm(MET.peakarearatio ~ MET.nominalmass, data = ExStdCurve)
summary(MyLM)

# 13a slope
names(MyLM$coefficients)

# 13b intercept

# 13c p value
summary(MyLM)$coef[2,"Pr(>|t|)"]

# 13d 
plot(MyLM)


# 14 In the Students dataset, some of those students are watching an awful
# lot of trashy vampire TV and some are not doing nearly enough sleeping. Here's
# a graph of those two variables:
data(Students)
head(Students)

ggplot(Students, aes(x = VampTV.hr, y = Sleep.hr)) +
      geom_point() +
      stat_smooth(method = lm)

VampTrouble <- lm(Sleep.hr ~ VampTV.hr, data = Students)
summary(VampTrouble)

# 15
VampTrouble <- lm(Sleep.hr ~ VampTV.hr + Gender, data = Students)
summary(VampTrouble)


# 16
VampTrouble <- lm(Sleep.hr ~ VampTV.hr + Gender + VampTV.hr * Gender, data = Students)
summary(VampTrouble)


# Nonlinear regression ------------------------------------------------------

# 17. Let's return to the ExStdCurve data.frame. We saw that those residuals
# don't look great, so let's try a nonlinear model where we fit a 2nd order
# polynomial to the data. Let's use the equation

# MET.peakarearatio ~ A * MET.nominalmass^2 + B * MET.nominalmass + C. For
# starting values, try list(A = 0, B = 1, C = 0)
names(ExStdCurve)


MyNLM <- nls(MET.peakarearatio ~ A*MET.nominalmass^2 +
                   B*MET.nominalmass + C, 
             data=ExStdCurve, start=list(A = 0, B = 1, C = 0))
summary(MyNLM)

# 18
summary(MyNLM)$coef[1, 4]


# 19. Using the standard curve helper function I wrote for myself, do a
# nonlinear regression of ExStdCurve, fitting a 2nd-order polynomial to the
# data. 

MyNLM2 <- stdCurve(ExStdCurve,
                   normPeak = "MET.peakarearatio",
                   nominal = "MET.nominalmass",
                   poly = "2nd", 
                   weights = ExStdCurve$Weight.1overx)
names(MyNLM2)
summary(MyNLM2$Fit)

MyNLM2$CurvePlot










