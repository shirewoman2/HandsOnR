# Statistics 

# This script demonstrates how to do some statistical tests in R.

# Housekeeping -------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# Summary statistics ----------------------------------------------------

data(Candidates)

# 1
summary(Candidates$amount)

# 2
mean(Candidates$amount, na.rm = TRUE)

sd(Candidates$amount, na.rm = TRUE)

# 3
?mean_sd

mean_sd(Candidates$amount, calcMedian = TRUE, reportn = TRUE)

# 4
Candidates %>% group_by(party) %>% 
      summarize(MeanSD = mean_sd(amount, calcMedian = TRUE, reportn = TRUE))


# Student's t test ------------------------------------------------------

# 5
DvsR <- Candidates %>% filter(amount > 0, 
                              party %in% c("DEMOCRAT", "REPUBLICAN"))

MyTTest <- t.test(amount ~ party, data = DvsR)
MyTTest

# 6
MyTTest$p.value


# 7
str(MyTTest)
MyTTest$estimate


# ANOVA ----------------------------------------------------------------

# 8
Donations <- Candidates %>% filter(amount > 0, 
                                   party %in% c("DEMOCRAT", "REPUBLICAN",
                                                "NON PARTISAN"))
Party.aov <- aov(amount ~ party, data = Donations)


# 9
summary(Party.aov)


# 10
TukeyHSD(Party.aov)

# 11
tukeyStar(Donations, groupColumn = "party", valueColumn = "amount")
# This would probably be easier to see if the data were log transformed. Let's
# do that.
tukeyStar(Donations, groupColumn = "party", valueColumn = "amount") +
      scale_y_log10()
# It *does* mess up the look of the comparison bars, but you get the idea. :-)


# Linear regression ---------------------------------------------------------

data(ExStdCurve)
head(ExStdCurve)

# 12
MyLM <- lm(MET.peakarearatio ~ MET.nominalmass, data = ExStdCurve)
summary(MyLM)

str(MyLM)

# I don't know why this is the case (it's probably the result of so many authors
# contributing to R over time), but there are several ways you can see the
# coefficients (alone or with standard error, t value and p value) of a linear
# regression.
MyLM$coefficients
coef(MyLM)
summary(MyLM)$coef
summary(MyLM)$coefficients

# 13a
MyLM$coefficients[2]

# 13b
MyLM$coefficients[1]

# 13c
summary(MyLM)$coef[2,"Pr(>|t|)"]

# 13d
MyLM$residuals

plot(MyLM)

# 14
ggplot(Students, aes(x = VampTV.hr, y = Sleep.hr)) +
      geom_point() +
      stat_smooth(method = lm)

VampTrouble <- lm(Sleep.hr ~ VampTV.hr, data = Students)
summary(VampTrouble)

# 15
ggplot(Students, aes(x = VampTV.hr, y = Sleep.hr, color = Gender)) +
      geom_point() +
      stat_smooth(method = lm)

VampTrouble2 <- lm(Sleep.hr ~ VampTV.hr + Gender, data = Students)
summary(VampTrouble2)
# Gender is not significant at the 95% confidence level.

# 16
VampTrouble3 <- lm(Sleep.hr ~ VampTV.hr + Gender + VampTV.hr * Gender,
                   data = Students)
summary(VampTrouble3)
# Nope. It doesn't look like we have sufficient evidence that the slope differs
# between the sexes. It may in fact differ, but we lack the evidence to show
# that with these data. Nota bene: You can't interpret the statistical
# significance of the other betas when there's an interaction term.


# Nonlinear regression -----------------------------------------------

# 17
ggplot(ExStdCurve, aes(x = MET.nominalmass, y = MET.peakarearatio)) +
      geom_point()

MyNLM <- nls(MET.peakarearatio ~ A*MET.nominalmass^2 + B*MET.nominalmass + C,
             data = ExStdCurve, 
             start=list(A = 0, B = 1, C = 0))
summary(MyNLM)

# 18
summary(MyNLM)$coef
summary(MyNLM)$coef[1, 4]
summary(MyNLM)$coef["A", "Estimate"]

# 19 
MyFit <- stdCurve(ExStdCurve, normPeak = "MET.peakarearatio",
                  nominal = "MET.nominalmass", poly = "2nd")

summary(MyFit$Fit)

# 19 challenge
?stdCurve
names(MyFit)
head(MyFit$CurvePlot)
# This is the data included behind the scenes in a ggplot2 graph. Try just
# running this:
MyFit$CurvePlot

head(MyFit$Curve.DF)
# These are the points used for drawing the fitted line in the graph

head(MyFit$Data)
# These are the data that were used to perform the regression. 