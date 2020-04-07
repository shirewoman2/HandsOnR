# Calculations with dataframes - 20200407

# This script is for playing around with data.frames.

# Housekeeping -----------------------------------------------------------

library(tidyverse)
library(LaurasHelpers)

# Structures and classes of objects ---------------------------------------

data(iris)
str(iris)

unique(iris$Species)

data(Students)
head(Students)
unique(Students$Cookie)

as.numeric(unique(iris$Species))

iris$Species %>% unique() %>% as.numeric()

as.numeric(Students$Gender)

class(iris$Species)
class(Students$Gender)

# Sort on the column "Gender" in the Students data.frame and make the order 1. "M", 2. "F".
Students <- Students %>% 
      mutate(Gender = factor(Gender, levels = c("M", "F"))) %>% 
      arrange(Gender)
      
Students <- mutate(Students, Gender = factor(Gender, levels = c("M", "F")))
Students <- arrange(Students, Gender)


Students$Gender <- factor(Students$Gender, levels = c("M", "F"))
# Students <- sort(Students, Students$Gender)

nrow(Students)

# What are the ranges of values for the hours of sleep in the Students data.frame?
range(Students$Sleep.hr)
range(Students[, "Sleep.hr"])


# Mathematical calculations on data.frames -----------------------------------

# 1. For the Students data.frame, let's assume that they spread out their TV
# watching evenly over the week. Make a new column "VampTV.hr.perday" that is
# VampTV.hr divided by 7.

Students <- Students %>% 
      mutate(VampTV.hr.perday = VampTV.hr / 7)

# 2. Let's assume that people need about 5 hours per day to just take care of
# things like eating, bathing, doing laundry, working out, etc. Let's make a
# separate variable called BasicLivingTime and set it equal to 5.

BasicLivingTime = 5


# 3. Now that we know how much time per day students are spending on basic
# living, trashy vampire TV watching, and sleeping, let's see how much time they
# each have left for working in the lab! How would you do that?
Students <- Students %>% 
      mutate(LabTime.perday = 24 - Sleep.hr - VampTV.hr.perday - BasicLivingTime)

sum(Students$LabTime.perday)


# Regular expressions: Matching patterns in text -----------------------------

# With the Students data.frame, find all the students whose name ends in "a"
# using RegEx.
Students$Name[str_detect(Students$Name, "a$")]

Students$Name[str_detect(Students$Name, "a$|e$")]


# 3. With the Candidates data.frame, what are all the possible values for the
# column "jurisdiction"?
data(Candidates)
str(Candidates)

unique(Candidates$jurisdiction)


# 4. Make a new column in the Candidates data.frame called "WhichHouse" and, if
# the jurisdiction contains the word "SENATE" extract that word and put it in
# the column "WhichHouse", and if it contains "HOUSE", put that in the column
# "WhichHouse".
Candidates <- Candidates %>% 
      mutate(WhichHouse = str_extract(jurisdiction, "SENATE|HOUSE"))

House <- Candidates %>% 
      filter(WhichHouse == "HOUSE")

# 5. For some reason (I honestly don't know it), one of the entries in
# jurisdiction, "CITY OF BOTHELL *", includes that asterisk. Count how many
# instances there are in which "jurisdiction" contains the special character
# "*".
Candidates <- Candidates %>% 
      mutate(LookForAsterisk = str_extract(jurisdiction, "\\*"))


# Subsetting with Boolean operators  ----------------------------------------

# 1. Make a new data.frame that is all the students in the Pharmaceutics
# Department. 

# 2. Make a new data.frame that is all the students who spent more
# than 3 hours watching trashy vampire TV


MyNum <- c(5, 2, NA, 10)
MyNum[MyNum > 2]
MyNum[which(MyNum > 2)]












