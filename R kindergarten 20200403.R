# R kindergarten

# This script is for practicing basic R concepts.

# Housekeeping -----------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)


# New section ==========================================================

68/9
12^5
log10(5.2)
log(5.2)


# III: Basic object creation and manipulation ---------------------------------

Dave <- 4
Mike <- c(39, 2, 6, 24, 99)

Emily <- seq(from = 0, to = 200, by = 4)
Emily <- seq(0, 200, 4)
Emily <- seq(to = 200, by = 4, from = 0)

Emily[23]

Gabe <- data.frame(Colors = c("red", "blue", "green", "purple"),
                   Numbers = c(1, 2, 3, 4), 
                   Letters = c("A", "B", "C", "E"), 
                   stringsAsFactors = FALSE)
Gabe

str(Gabe)

# Importing data ----------------------------------------------------

data(Students)
str(Students)

getwd()

MyPath <- "C:/Users"
setwd("C:/Users/Laura Shireman/OneDrive/Documents/Software training files/Hands-on R training sessions")

Students <- read.csv("Students.csv",  stringsAsFactors = FALSE)

# Subsetting with Boolean operators ---------------------------------------

Students[4, 5]
Students[4, "VampTV.hr"]

Students$VampTV.hr

# Make a new data.frame that is all the students in the Pharmaceutics Department.
Pceut <- Students[Students$Department == "Pceut", ]

# Make a new data.frame that is all the students who spent more than 3 hours
# watching trashy vampire TV
VampTV <- Students %>% filter(VampTV.hr > 3) 

# Make a new data.frame that is all the students whose favorite cookie is
# molasses spice or chocolate chip.
DF <- Students %>% filter(Cookie %in% c("molasses spice", "chocolate chip"))

# Make a new data.frame that is all the students whose favorite cookie is
# molasses spice or whose department is MedChem
DF <- Students %>% filter(Cookie %in% c("molasses spice") | 
                                Department == "MedChem")


MyNum <- c(5, 2, NA, 10)
MyNum[MyNum > 2]
MyNum[which(MyNum > 2)]



# Calculations with data.frames -------------------------------------------

NewPath <- "C:/Users/Laura Shireman/OneDrive/Documents/Software training files/Hands-on R training sessions"
setwd(NewPath)

StdCurve <- read.csv("standard curves.csv", stringsAsFactors = FALSE)
names(StdCurve) <- c("SampleID", "Matrix", "MET.wk", "VolWk", "MET.peak",
                     "d6MET.peak")

# Given the volume of the standard solution added and the concentration of the
# standard solution, calculate the nominal mass added to each sample in a
# standard curve by multiplying columns.
StdCurve <- StdCurve %>% 
      mutate(MET.nommass = VolWk * MET.wk)


# Calculate the peak area ratio of metformin/d6-metformin.
StdCurve <- StdCurve %>% 
      mutate(MET.PAR = MET.peak / d6MET.peak)

# Make a data.frame called CurveB that is only the rows in the "standard curves"
# file where the matrix is "Blank plasma B". (That capital "B" matters!)
CurveB <- StdCurve %>% 
      filter(Matrix == "Blank plasma B")


# In the data.frame StdCurve, make the column "Matrix" a factor with the levels
# in this order: Blank plasma B, Blank plasma C, Blank plasma A.
StdCurve <- StdCurve %>%
      mutate(Matrix = factor(Matrix, levels = c("Blank plasma B",
                                                "Blank plasma C",
                                                "Blank plasma A")))


# Sort the data.frame by matrix and then by nominal mass added.
StdCurve <- StdCurve %>% arrange(Matrix, MET.nommass) 

# Make the order of the columns be the following: Matrix, SampleID, nominal mass
# of metformin added (ng), Metformin peak area ratio (MET/d6-MET). Omit the
# other columns.
StdCurve <- StdCurve %>% 
      select(Matrix, SampleID, MET.nommass, MET.PAR)

# Save the data.frame as a csv file or an xlsx file. 
write.csv(StdCurve, file = "My standard curve.csv", row.names = FALSE)




