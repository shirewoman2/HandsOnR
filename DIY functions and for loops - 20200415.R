# DIY function and for loops

# This script shows examples for making your own functions and also for using 
# for loops. 

# Housekeeping ------------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)

setwd("C:/Users/Laura Shireman/OneDrive/Documents/Software training files/Hands-on R training sessions")
load("Concentration-time data to play with.RData")


# DIY functions ---------------------------------------------------------

round2 <- function(x){
      round(x, digits = 2)
}


round2(54.234325)
round2(1.345)

# 1. Write a function called "add5" that adds 5 to whatever input is supplied.

add5 <- function(x){
      x + 5
}

add5(60)
add5("cat")


# 2 

sortName <- function(DF){
      return(DF %>% arrange(Name))
}

data(Students)
sortName(Students)

data(ExStdCurve)
names(ExStdCurve)

sortName(ExStdCurve)


# 3

sortName <- function(DF){

      if("Name" %in% names(DF)){
            return(DF %>% arrange(Name))
      } else {
            stop("One column in the data.frame must be called 'Name' for this function to work.")
            
      }
      
      
            
}

sortName(Students)
sortName(ExStdCurve)


# 4. Modify your add5 function to print an error statement if the input is not
# numeric data. Check what happens when you supply non-numeric data.
add5 <- function(x){
      if(is.numeric(x)){
            x + 5      
      } else {
            stop("Input must be numeric.")
      }
}


# 5. Let's create a new function called "pstar" that returns a "*" when a given
# p value is < 0.05 and "not significant" otherwise.

pstar <- function(pvalue){
      if(pvalue <= 0.05){
            return("\\*")
      } else {
            return("not significant")
      }
}
pstar(0.01)
pstar(0.0001)
pstar(0.06)

# 6. Let's give our users a little more freedom. Maybe they're defining
# significance at the 90% confidence level, not the 95% confidence levels like
# we did above. However, since we're going to define significance as being at
# the 95% confidence interval most of the time, let's make the default be that
# they get the star at p = 0.05. You'll need to add a parameter to the function
# to do this.

pstar <- function(pvalue, sig.level = 0.05){
      if(pvalue <= sig.level){
            return("\\*")
      } else {
            return("not significant")
      }
}

pstar(0.1, 0.05)
pstar(0.1, 0.10)

pstar(0.1)
pstar(0.1, 0.10)


# Subtopic: if ... else vs ifelse ---------------------------------------


# 7. Make a control structure so that if x < 5, you make a new data.frame,
# "Sleep", in which you sort the Students data.frame by Cookie and also only
# keep observations where students have had at least 7 hours of sleep. If x >= 5,
# make a new data.frame "Sleep" in which you sort the Students data.frame by
# Department and keep all observations.

x <- 20 # Try changing this number
if(x < 5){
      Sleep <- Students %>% arrange(Cookie) %>% 
            filter(Sleep.hr >= 7)
} else {
      Sleep <- Students %>% arrange(Department)
}


# 8. Make a control structure to fix mistakes in data entry. Say that you just
# found out that Ryan secretly adores vampire TV and didn't watch 5.5 hours of
# vampire TV last week but 55. Using tidyverse syntax (mutate), change the
# values in the column VampTV.hr to be 55 if the name of the student is Ryan.
# If the name is not Ryan, leave the values what they were.
Students <- Students %>% 
      mutate(VampTV.hr = ifelse(Name == "Ryan", 
                                55, VampTV.hr))


# Graphing with functions for consistency -----------------------------------

# 9. There's a file in the Dropbox and github folders called "Concentration-time
# data to play with.RData". Load that into the workspace. In this fake study,
# subjects received drug A and drug B and received them IV and SC on a total of
# 4 study days.

# We did this at the top of the sript. 


# 10. Make a function called "plotConc" where the input is the subject ID and
# the output is a concentration-time curve. Here's the code I would use for
# making a graph of just subject 101 as a template:
ggplot(ConcTime %>% filter(SubjectID == "101"), 
       aes(x = TimeHr, y = Concentration, color = DoseRoute)) +
      geom_point() + geom_line() +
      facet_wrap(~ Drug) +
      ggtitle("101")

plotConc <- function(ID){
      ggplot(ConcTime %>% filter(SubjectID == ID), 
             aes(x = TimeHr, y = Concentration, color = DoseRoute)) +
            geom_point() + geom_line() +
            facet_wrap(~ Drug) +
            ggtitle(ID)
      
      
}

plotConc(125)

# 11. Check your function by plotting subjects 135 and 140.


# 12. Your PI just waltzed into the lab an hour before your poster has to be
# sent to the printers and said, "Make the y axis label be "Concentration
# (ng/mL)" and make the x axis label "Time (hr)" and make the points larger.
# Change your function to do that.

plotConc <- function(ID){
      ggplot(ConcTime %>% filter(SubjectID == ID), 
             aes(x = TimeHr, y = Concentration, color = DoseRoute)) +
            geom_point(size = 3) + geom_line() +
            facet_wrap(~ Drug) +
            ggtitle(ID) +
            xlab("Concentration (ng/mL)") +
            ylab("Time (hr)")
      
      
}

plotConc(125)















