# Joining, sorting, reshaping, and summarizing data.frames

# This script is meant to demonstrate ways for manipulating data.frames.

# Housekeeping ------------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# Joining and sorting data.frames ------------------------------------------------

# 1
data(Students)
str(Students)

data(Pets)
str(Pets)

# 2
Students <- Students %>% left_join(Pets)

# 3
which(duplicated(Students$Name))
# This lists the 2nd instance. I find using this in a base R command helps me
# see what's going on.
Students[which(duplicated(Students$Name)), ]
# Again, this is only the 2nd instance, but it tells me that Eddie and Mackenzie
# are listed twice. Let's try making the data.frame unique and see if that
# solves things.
Students <- Students %>% unique()
which(duplicated(Students$Name))

# 4
Students <- Students %>% arrange(Number.of.pets, Department, Name)


# 5
Students <- Students %>% 
      bind_rows(data.frame(Name = c("Aiden", "Elliott"), Department = "Pceut", 
                           Gender = "M", Cookie = "chocolate chip", 
                           Sleep.hr = c(11, 10), Number.of.pets = 3))

# Summarizing data.frames ---------------------------------------------------

# 6
data(Candidates)
str(Candidates)

Candidates %>%
      filter(amount > 0) %>% 
      summarize(NumDonations = n())


# 7
Candidates %>%
      filter(amount > 0) %>% 
      group_by(party) %>% 
      summarize(NumDonations = n(),
                Mean = mean(amount),
                Median = median(amount),
                # 8
                Total = sum(amount),
                # 9
                MeanSD = mean_sd(amount, reportn = TRUE))

# Reshaping data.frames ---------------------------------------------------

# 10
data(Cotinine)
head(Cotinine)


# 11
COT_wide <- Cotinine %>% 
      # We're only going to look at peak area, so let's remove peak height and
      # retention time.
      select(-Height, -RT) %>% 
      spread(key = Analyte, # the "key" column is the character data that 
             # contains the names of the new columns you're about to make
             value = Area) %>% # the "value" column is the column that contains 
             # the data you want to be listed in those new columns
      mutate(COT.PAR = COT / d3COT)

# Challenge: What happens if you don't remove peak height and retention time?
COT_wide_v2 <- Cotinine_original %>% 
      spread(key = Analyte, value = Area)
# Answer: Since we still have Height and RT in our data.frame, it will try to
# match those values when it spreads the column Area. Since COT will have one
# set of values for Area, Height, and RT and d3COT will have another set of
# values for Area, Height, and RT, it will mean that your rows will be
# mismatched. You'll have "NA" for d3COT whenever you have a value for COT and
# vice versa.


# 12
PrePost <- data.frame(SubjectID = rep(101:200, 2),
                      StudyDay = rep(c("pre", "post"), each = 100), 
                      CD4count = c(rnorm(100, 200, 100),
                                   rnorm(100, 450, 50)),
                      stringsAsFactors = FALSE)

# Making it wide
PrePost_wide <- PrePost %>% 
      spread(key = StudyDay, value = CD4count) %>% 
      mutate(CD4_diff = post - pre)

# 13
PrePost_long <- PrePost_wide %>% 
      select(-CD4_diff) %>% 
      gather(key = StudyDay, value = CD4count, 
             -SubjectID)

ggplot(PrePost_long, aes(x = StudyDay, y = CD4count, color = SubjectID,
                         group = SubjectID)) +
      geom_point() + geom_line() +
      scale_x_discrete(limits = c("pre", "post"))


#14
# Checking that everything matches. It does have to be listed in the same order
# for this to work; that's why we're including the "arrange" function.
all(arrange(PrePost, SubjectID, StudyDay) == 
          arrange(PrePost_long, SubjectID, StudyDay))







