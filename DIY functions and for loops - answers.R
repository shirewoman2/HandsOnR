# DIY functions and for loops

# This script shows examples of functions you make yourself and how to do
# repetitive tasks easily with "for" loops.

# Housekeeping -------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)


# DIY functions -------------------------------------------------------

# 1
add5 <- function(x){
      x + 5
}


# 2
sortName <- function(DF){
      arrange(DF, Name)
}

data(Students)
sortName(Students)

# 3
data(ExStdCurve)
sortName(ExStdCurve)

sortName <- function(DF){
      if("Name" %in% names(DF)){
            arrange(DF, Name)
      } else {
            stop("One column in the data.frame must be called 'Name' for this function to work.")
      }
}
sortName(Students)
sortName(ExStdCurve)

# 4
add5 <- function(x){
      if(is.numeric(x)){
            x + 5      
      } else {
            stop("Input must be numeric.")
      }
}


# 5
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

# 6
pstar <- function(pvalue, conf.level = 0.05){
      if(pvalue <= conf.level){
            return("\\*")
      } else {
            return("not significant")
      }
}

pstar(0.01)
pstar(0.06)
pstar(0.1)
pstar(0.1, 0.1)


# If ... else vs. ifelse ---------------------------------------------------

# 7
x <- 20 # Try changing this number
if(x < 5){
      Sleep <- Students %>% arrange(Cookie) %>% 
            filter(Sleep.hr >= 7)
} else {
      Sleep <- Students %>% arrange(Department)
}


# 8
Students <- Students %>% 
      mutate(VampTV.hr = ifelse(Name == "Ryan", 55, VampTV.hr))


# functions for graphing consistently ----------------------------------

# 9
load("Concentration-time data to play with.RData")

ggplot(ConcTime %>% filter(SubjectID == "101"), 
       aes(x = TimeHr, y = Concentration, color = DoseRoute)) +
      geom_point() + geom_line() +
      facet_wrap(~ Drug) +
      ggtitle("101")


# 10

plotConc <- function(ID){
      ggplot(ConcTime %>% filter(SubjectID == ID), 
             aes(x = TimeHr, y = Concentration, color = DoseRoute)) +
            geom_point() + geom_line() +
            facet_wrap(~ Drug) + 
            ggtitle(ID)
}


# 11
plotConc("135")
plotConc("140")


# 12

plotConc <- function(ID){
      ggplot(ConcTime %>% filter(SubjectID == ID), 
             aes(x = TimeHr, y = Concentration, color = DoseRoute)) +
            geom_point(size = 3) + geom_line() +
            facet_wrap(~ Drug) + 
            ggtitle(ID) +
            ylab("Concentration (ng/mL)") +
            xlab("Time (hr)")
      
}

plotConc("101")


# for loops ---------------------------------------------------------------

# 13

for(i in 1:10){
      if(i > 4){
            print("greater") 
      } else {
            print("smaller")
      } 
}


# 14

MyNum <- data.frame(Number = 1:10, 
                    CheckValue = NA)

# 15
for(i in 1:10){
      if(MyNum$Number[i] > 4){
            MyNum$CheckValue[i] <- "greater" 
      } else {
            MyNum$CheckValue[i] <- "smaller"
      } 
}


# 16
for(i in 1:10){
      MyNum$CheckValue[i] <- ifelse(MyNum$Number[i] > 4, "greater", "smaller")  
}



# 17
ConcTime10 <- ConcTime %>% filter(SubjectID %in% 101:110)


# 18
for(i in 101:110){
      plotConc(i)
      ggsave(paste0("Concentration-time plot of subject ", i, ".png"),
             height = 4, width = 6)
      
}







