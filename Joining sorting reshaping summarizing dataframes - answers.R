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
Cotinine <- Cotinine %>% 
      # We're only going to look at peak area, so let's remove peak height and
      # retention time.
      select(-Height, -RT) %>% 
      spread(key = Analyte, # the "key" column is the character data that 
             # contains the names of the new columns you're about to make
             value = Area) %>% # the "value" column is the column that contains 
             # the data you want to be listed in those new columns
      mutate(COT.PAR = COT / d3COT)


# 12
PrePost <- data.frame(SubjectID = rep(101:200, 2),
                      StudyDay = rep(c("pre", "post"), each = 100), 
                      CD4count = c(rnorm(100, 200, 100),
                                   rnorm(100, 450, 50)))

# Making it wide
PrePost <- PrePost %>% 
      spread(key = StudyDay, value = CD4count) %>% 
      mutate(CD4_diff = post - pre)

# 13
PrePost <- PrePost %>% 
      select(-CD4_diff) %>% 
      gather(key = StudyDay, value = CD4count, 
             -SubjectID)


PrePost <- PrePost %>% 
      mutate(StudyDay = factor(StudyDay, levels = c("pre", "post")))

ggplot(PrePost, aes(x = StudyDay, y = CD4count, color = SubjectID,
                    group = SubjectID)) +
      geom_point() + geom_line()





