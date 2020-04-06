# R kindergarten

# This script is for practicing basic R concepts.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

# II: Math --------------------------------------------------------------

68/9
12^5
log10(5.2)
log(5.2)
round(10.3689, 1)
signif(10.3689, 4)


# III: Basic object creation and manipulation ---------------------------------

Dave <- 4
Mike <- c(39, 2, 6, 24, 99)
Dave * Mike


Emily <- seq(0, 200, 4)
Emily / Dave
Emily[23]


# Options for making "Gabe":
Gabe <- data.frame(Colors = c("red",  "blue", "green", "purple"), 
                   Numbers = c(1, 2, 3, 4), 
                   Letters = c("A", "B", "C", "D"))
# or:
Gabe <- data.frame(Colors = c("red",  "blue", "green", "purple"), 
                   Numbers = 1:4, 
                   Letters = LETTERS[1:4])

str(Gabe)


# IV: Importing data ----------------------------------------------------

# See the heading "Data for working out the examples" for the 4 possible ways to
# import data here. I'll go over each.

## Option 1
# I've got Dropbox installed on my computer, so I can call on that file with its
# path and file name.
Students <- read.csv("C:/Users/Laura Shireman/Dropbox/AAPS R class/Students.csv")
# Note that the slashes are FORWARD slashes, not the default back slashes
# Windows uses.

## Option 2
# This is the same as option 1 except that I've downloaded the files from Github
# instead of getting them from Dropbox. Instructions are the same.

## Option 3
# I'm going to use a function to do this. Here's the function:
read.gitcsv <- function(url, stringsAsFactors = FALSE){
      URL <- sub("github.com", "raw.githubusercontent.com", url)
      URL <- sub("blob/master", "master", URL)
      Out <- read.csv(URL, stringsAsFactors = FALSE)
      return(Out)
}
# This is a homemade function, and we'll go over how to create these later!

Students <- read.gitcsv("https://github.com/shirewoman2/HandsOnR/blob/master/Students.csv")


## Option 4
library(LaurasHelpers)
data(Students)
# After this step, students will be listed as a "promise".
head(Students)
# It doesn't have to be "head" that you do to the object to make it more than a
# promise; it can be anything!



# V: Subsetting with Boolean operators -------------------------------------------

# a
Pceut <- Students %>% filter(Department == "Pceut")

# b
Trash <- Students %>% filter(VampTV.hr > 3)

# c
Cookie <- Students %>% filter(Cookie %in% c("molasses spice", "chocolate chip"))

# d (Note that I'm not creating a new object here; I'm just listing the contents
# of this data.frame)
Students %>% filter(Cookie == "molasses spice" | 
                                  Department == "MedChem")

# e
Students %>% filter(Sleep.hr >= 7.5)


# f
MyNum <- c(5, 2, NA, 10)
MyNum[MyNum > 2]
MyNum[which(MyNum > 2)]


# g
data(Candidates)
# One option, using base R syntax combined with stringr (str_detect comes from
# the package stringr)
length(unique(Candidates$contributor_name[
      str_detect(Candidates$contributor_name, "NGUYEN")]))

# Another idea, using the tidyverse syntax "pull", which pulls from a data.frame
# only the column you specify.
Candidates %>% filter(str_detect(contributor_name, "NGUYEN")) %>% 
      pull %>% length

# And another tidyverse-based option using the "summarize" and "n" functions
Candidates %>% filter(str_detect(contributor_name, "NGUYEN")) %>% 
      summarize(N = n())


# VI: Calculations with data.frames -----------------------------------

data(Metformin)

# I'm going to use tidyverse syntax here because it's easier to read and to
# type. Base R syntax also works fine if you're more comfortable with that.

# First, though, let's check the names of this data.frame.
names(Metformin)
# Yuck. Those are long and they have a lot of periods in them (the source file
# had spaces or symbols where those periods are). Let's rename all of them.
names(Metformin) <- c("SampleID", "Matrix", "MET.wk", "VolWk", "MET.area", 
                      "d6MET.area")

# Now that will be easier to work with. 

# 2
Metformin <- Metformin %>% 
      mutate(MET.mass = MET.wk * VolWk)

# 3
Metformin <- Metformin %>% 
      mutate(MET.PAR = MET.area / d6MET.area)

# 4 
CurveB <- Metformin %>% 
      filter(Matrix == "Blank plasma B")

# 5 
Metformin <- Metformin %>% 
      mutate(Matrix = factor(Matrix, levels = c("Blank plasma B", 
                                               "Blank plasma C", 
                                               "Blank plasma A")))

# 6
Metformin <- Metformin %>% 
      arrange(Matrix, MET.mass)

# 7
Metformin <- Metformin %>% 
      select(Matrix, SampleID, MET.mass, MET.PAR)

# 8
write.csv(Metformin, "Metformin revised.csv", row.names = FALSE)

# NOTE: We could have done many of the steps here all at one go instead of line
# by line. Here's how that could look:
data(Metformin)
names(Metformin) <- c("SampleID", "Matrix", "MET.wk", "VolWk", "MET.area", 
                      "d6MET.area")

Metformin <- Metformin %>% 
      mutate(MET.mass = MET.wk * VolWk, 
             MET.PAR = MET.area / d6MET.area, 
             Matrix = factor(Matrix, levels = c("Blank plasma B", 
                                                "Blank plasma C", 
                                                "Blank plasma A"))) %>% 
      arrange(Matrix, MET.mass) %>% 
      select(Matrix, SampleID, MET.mass, MET.PAR)

