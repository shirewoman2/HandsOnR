# Calculations with data.frames

# This script is for practicing some common manipulations of data and
# data.frames and for seeing how to do some statistics.


# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)


# II. Structures and classes of objects -----------------------------------------

# 1
data(iris)
str(iris)
attributes(iris)

# 2 
unique(iris$Specsies)

# 3
data(Students)
unique(Students$Cookie)

# 4
as.numeric(iris$Species)
as.numeric(Students$Gender)
# Note that iris$Species is factor data whereas Students$Gender is character
# data.


# 5
# Using tidyverse syntax here:
Students <- Students %>% 
      mutate(Gender = factor(Gender, levels = c("M", "F"))) %>% 
      arrange(Gender)

# 6
nrow(Students)

# 7
range(Students$Sleep.hr)

# III. Mathematical calculations on data.frames ---------------------------------------

# 1
Students <- Students %>% 
      mutate(VampTV.hr.perday = VampTV.hr / 7)

# 2
BasicLivingTime <- 5

# 3
Students <- Students %>% 
      mutate(LabTime.perday = 24 - Sleep.hr - VampTV.hr.perday - BasicLivingTime)

# 4
sum(Students$LabTime.perday)

# IV. RegEx: Matching patterns in text ----------------------------------------------

# 1
Students$Name[str_detect(Students$Name, "a$")]

# 2
Students$Name[str_detect(Students$Name, "a$|e$")]

# 3
data(Candidates)
str(Candidates)
unique(Candidates$jurisdiction)

# 4
# Using tidyverse syntax:
Candidates <- Candidates %>% 
      mutate(WhichHouse = str_extract(jurisdiction, "HOUSE|SENATE"))
# Checking that it worked...
unique(Candidates$WhichHouse)

# 5
length(which(str_detect(Candidates$jurisdiction, "\\*")))

# 6
Students <- Students %>% 
      mutate(Cookie = sub("raisin", "disgusting brown shriveled grape", 
                          Cookie))

# 7
# Here's how I'd probably do it:
Candidates %>% 
      filter(str_detect(contributor_name, "NGUYEN")) %>% 
      pull(contributor_name) %>% unique() %>% length()
# "pull" selects only that column and then converts the data from a data.frame
# into a vector. 


# Here's another way:
Candidates %>% 
      filter(str_detect(contributor_name, "NGUYEN")) %>% 
      summarize(Number = n_distinct(contributor_name))


# And another way, this time using base R and stringr functions:
length(unique(Candidates$contributor_name[
      str_detect(Candidates$contributor_name, "NGUYEN")]))


# V: Subsetting with Boolean operators -------------------------------------------

# 1
Pceut <- Students %>% filter(Department == "Pceut")

# 2
Trash <- Students %>% filter(VampTV.hr > 3)

# 3
Cookie <- Students %>% filter(Cookie %in% c("molasses spice", "chocolate chip"))

# 4 (Note that I'm not creating a new object here; I'm just listing the contents
# of this data.frame)
Students %>% filter(Cookie == "molasses spice" | 
                          Department == "MedChem")

# 5
Students %>% filter(Sleep.hr >= 7.5)


# 6
MyNum <- c(5, 2, NA, 10)
MyNum[MyNum > 2]
MyNum[which(MyNum > 2)]



