# Exploring Covid-19 case data

# This script explores Covid-19 cases and deaths using data collected and
# provided by The New York Times, accessed on April 7, 2020.

# Housekeeping --------------------------------------------------------
library(tidyverse)

# Function for reading csv files from their github repository
read.gitcsv <- function(url, stringsAsFactors = FALSE){
      URL <- sub("github.com", "raw.githubusercontent.com", url)
      URL <- sub("blob/master", "master", URL)
      Out <- read.csv(URL, stringsAsFactors = FALSE)
      return(Out)
}

# Loading and tidying data ---------------------------------------------------

C19_counties <- read.gitcsv("https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
C19_states <- read.gitcsv("https://github.com/nytimes/covid-19-data/blob/master/us-states.csv")

# Checking the structure of the data. 
str(C19_counties)

# We need to convert the "date" column from character to Date format. 
C19_counties <- C19_counties %>% 
      mutate(date = as.Date(date))

C19_states <- C19_states %>% 
      mutate(date = as.Date(date))

# Adding population data from the US Census Bureau
setwd("C:/Users/Laura Shireman/OneDrive/Documents/Software training files/Hands-on R training sessions")

WApop <- read.csv("co-est2019-annres-53.csv")
str(WApop)

# Tidying and converting to long format
WApop <- WApop %>% rename(county = matches("County")) %>% 
      gather(key = Year, value = Population, -county) %>% 
      mutate(Year = sub("X", "", Year),
             Year = as.numeric(Year), 
             # County in the Census Bureau data is formatted differently from
             # the county in the NY Times data. The column name was capitalized
             # originally (I already changed that with the "rename" function),
             # and the county includes " County, Washington" here but not in the Times
             # data. Making them match.
             county = sub(" County, Washington", "", county),
             county = as.factor(county),
             county = relevel(county, "Washington (entire state)"))

# These are kind of interesting on their own, but really, we want to join the
# most-recent population estimates with the Covid-19 data. Doing that.
C19_counties_WA <- C19_counties %>%
      filter(state == "Washington") %>% 
      left_join(WApop %>% filter(Year == 2019) %>% select(-Year)) %>% 
      mutate(Case_per100k = cases / (Population/100000), 
             Death_per100k = deaths / (Population/100000))


# County data: Exploratory graphs ----------------------------------------------------------

windows()

ggplot(C19_counties %>% filter(state == "Washington"),
       aes(x = date, y = cases, color = county)) +
      geom_point() + geom_line() +
      scale_y_log10()

ggplot(C19_counties %>%
             filter(state == "Washington", 
                    county %in% c("King", "Kitsap", "Snohomish", "Pierce")),
       aes(x = date, y = cases, color = county)) +
      geom_point() + geom_line() +
      scale_y_log10()

ggplot(C19_counties_WA %>%
             filter(county %in% c("King", "Kitsap", "Snohomish", "Pierce")),
       aes(x = date, y = Case_per100k, color = county)) +
      geom_point() + geom_line() +
      scale_y_log10()




# Just WA population ------------------------------------------------------
ggplot(WApop, aes(x = Year, y = Population, color = County)) +
      geom_point() + geom_line() +
      scale_x_continuous(breaks = seq(2010, 2020, 1))



# Cases and deaths by state -----------------------------------------------

ggplot(C19_states, aes(x = date, y = cases, color = state)) + 
      geom_line()




