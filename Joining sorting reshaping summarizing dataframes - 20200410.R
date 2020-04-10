# Joining, sorting, reshaping, and summarizing data.frames

# This script is meant to demonstrate ways for manipulating data.frames.

# Housekeeping ------------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# Joining and sorting data.frames ------------------------------------------------

# 1
data(Students)
head(Students)

data(Pets)
str(Pets)

# 2
# Join the two by the student name. 
Students <- left_join(Students, Pets)

which(duplicated(Students$Name))

Students <- Students %>% unique()
which(duplicated(Students$Name))


# 4
Students <- Students %>% 
    arrange(Number.of.pets, Department, Name)  

Students <- Students %>% 
      arrange(desc(Number.of.pets), Department, Name)  


names(Students)
Students <- Students %>% select(Name, Gender, Number.of.pets, 
                                Department, Cookie, VampTV.hr, Sleep.hr)
Students %>% select(-Number.of.pets)

# 5
Students <- Students %>% 
      bind_rows(data.frame(Name = c("Aiden", "Elliott"), Department = "Pceut", 
                           Gender = "M", Cookie = "chocolate chip", 
                           Sleep.hr = c(11, 10), Number.of.pets = 3)
      )

# Arrange Students by Cookie and then Name?
Students <- Students %>% arrange(Cookie, Name)

# Summarizing data.frames ------------------------------------------------

# 6. Using the Candidates data, count the number of donations (reminder:
# donations would need to be positive amounts of money) made.
data(Candidates)
str(Candidates)

summary(Candidates$amount)

Candidates %>% filter(amount > 0) %>% nrow()

# 7 Grouping the data by party, calculate the mean and median donation. 
Candidates %>% group_by(party) %>% 
      summarize(MeanDonation = mean(amount, na.rm = TRUE),
                MedDonation = median(amount, na.rm = TRUE))

# What is the mean and median donation broken down by party AND by jurisdiction_type?
Candidates %>% group_by(party, jurisdiction_type) %>% 
      summarize(MeanDonation = mean(amount, na.rm = TRUE),
                MedDonation = median(amount, na.rm = TRUE))

# 8. Calculate the total amount of donations each party received in King County in 2018.
Candidates %>% group_by(party) %>% 
      summarize(Total = sum(amount))

# 9. Calculate the mean +/- standard deviation of the amount of the donation by
# party and number of observations all at once.
Candidates %>% group_by(party) %>% 
      summarize(MeanSD = mean_sd(amount, reportn = TRUE))



# Reshaping data.frames ------------------------------------------------------

data(Cotinine)
str(Cotinine)

# 11
Cotinine <- Cotinine %>% 
      select(-RT, -Height) %>% 
      spread(key = Analyte, value = Area) %>% 
      mutate(COT.PAR = COT / d3COT)


# 12
PrePost <- data.frame(SubjectID = rep(101:200, 2),
                      StudyDay = rep(c("pre", "post"), each = 100), 
                      CD4count = c(rnorm(100, 200, 100),
                                   rnorm(100, 450, 50)))
# Making it wide
PrePost_wide <- PrePost %>% 
      spread(key = StudyDay, value = CD4count) %>% 
      mutate(CD4_diff = post - pre)

# 13. Now, let's do the reverse. This is a bit silly, but let's undo what we did
# in step 12. Let's take that PrePost_wide, make from it a data.frame called
# "PrePost_long", remove the difference column we just made, and then make the
# wide data.frame long again. It should be identical to the original data.frame.

names(PrePost_wide)

PrePost_long <- PrePost_wide %>% 
      select(-CD4_diff) %>% 
      gather(key = StudyDay, value = CD4count, -SubjectID)


ggplot(PrePost_long, aes(x = StudyDay, y = CD4count, color = SubjectID,
                         group = SubjectID)) +
      geom_point() + geom_line() + 
      scale_x_discrete(limits = c("pre", "post"))

# 14
all(
      arrange(PrePost, SubjectID, StudyDay) ==
            arrange(PrePost_long, SubjectID, StudyDay)
)


# Examples of "all"
A <- 1:4
B <- 1:4
A
B

A == B

all(A == B)


A <- c(1, 5, 3, 4)
A

all(A == B)

# another one that can be useful: any
any(A == B)



