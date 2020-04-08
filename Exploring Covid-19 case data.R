# Exploring Covid-19 case data

# This script explores Covid-19 cases and deaths using data collected and
# provided by The New York Times. I last accessed this on April 8, 2020, but the
# website currently is being updated daily, so the data will be up to date
# whenever this script is run.

# Housekeeping --------------------------------------------------------
library(tidyverse)
library(LaurasHelpers)
library(lubridate)

ThemeLaura <- function (base_size = 12, base_family = "") {
      theme_gray(base_size = base_size, base_family = base_family) %+replace%
            theme(
                  panel.background = element_rect(fill="white", color=NA),
                  panel.grid.minor.y = element_line(color = NA),
                  panel.grid.minor.x = element_line(color = NA),
                  panel.grid.major = element_line(colour = NA),
                  plot.background = element_rect(fill="white", colour=NA),
                  panel.border = element_rect(color="black", fill=NA),
                  strip.background = element_rect(color=NA, fill="white"),
                  legend.background = element_rect(color=NA, fill=NA),
                  legend.key = element_rect(color=NA, fill=NA)
            )
}

colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                 "darkorange", "green3", "seagreen3",
                                 "cadetblue", "dodgerblue3", "royalblue4",
                                 "darkorchid4"))

# Function for reading csv files from their github repository
read.gitcsv <- function(url, stringsAsFactors = FALSE){
      URL <- sub("github.com", "raw.githubusercontent.com", url)
      URL <- sub("blob/master", "master", URL)
      Out <- read.csv(URL, stringsAsFactors = FALSE)
      return(Out)
}

Today <- paste0(month(today(), label = T, abbr = F), " ",
                day(today()), ", ", year(today()))

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

# What about the death rate per case? Does that show anything interesting?
C19_counties <- C19_counties %>% 
      mutate(MortRate = deaths / cases)
C19_states <- C19_states %>% 
      mutate(MortRate = deaths / cases)


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

# Getting the latest date for which we have data
LatestDate <- max(C19_states$date)
LatestDate <- paste0(month(LatestDate, label = T, abbr = F), " ",
                     day(LatestDate), ", ", year(LatestDate))


# County data: Exploratory graphs ----------------------------------------------------------

windows()

theme_set(theme_grey())

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

ggplot(C19_counties_WA %>% filter(date == max(date)), 
       aes(x = county, y = MortRate, fill = county)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "none") +
      annotate("text", x = 28, y = Inf, hjust = 0, vjust = 1.25,
               fontface = "italic", size = 2.5,
               label = "The actual number of cases is likely underreported.\nThe number of deaths is likely to rise.") +
      ylab(paste("Number of deaths per case of infection\nas of", 
                 LatestDate)) +
      xlab("Washington State county") +
      ggtitle("Covid-19 apparent case fatality rate by county in Washington State",
              subtitle = paste("Data from https://github.com/nytimes/covid-19-data, accessed on",
              Today))
ggsave("Covid-19 apparent case fatality rate by WA county.png", 
       width = 12, height = 4)


# Just WA population ------------------------------------------------------
ggplot(WApop, aes(x = Year, y = Population, color = county)) +
      geom_point() + geom_line() +
      scale_x_continuous(breaks = seq(2010, 2020, 1))



# Cases and deaths by state -----------------------------------------------

ggplot(C19_states %>% filter(date == max(date)), 
       aes(x = state, y = MortRate, fill = state)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "none") +
      annotate("text", x = 40, y = Inf, hjust = 0, vjust = 1.25,
               fontface = "italic", size = 2.5,
               label = "The actual number of cases is likely underreported.\nThe number of deaths is likely to rise.") +
      ylab(paste("Number of deaths per case of infection\nas of", 
                 LatestDate)) +
      xlab("State") +
      ggtitle("Covid-19 apparent case fatality rate by state",
              subtitle = paste("Data from https://github.com/nytimes/covid-19-data, accessed on",
                               Today))
ggsave("Covid-19 apparent case fatality rate by state.png", 
       width = 14, height = 4)



# Call up my graphing preferences for this next graph... 
theme_set(ThemeLaura())

AllStates <- sort(unique(C19_states$state))
MyCol <- colRainbow(length(AllStates))

ggplot(C19_states, aes(x = date, y = cases, color = state)) + 
      geom_line() +
      scale_color_manual(values = MyCol) +
      geom_line(data = C19_states %>% filter(state == "Washington"),
                size = 3) +
      
      # Emphasizing Washington data
      annotate("text", x = as.Date("2020-03-10"), 
               y = C19_states %>% filter(date == as.Date("2020-03-10") &
                                               state == "Washington") %>% 
                     pull(cases),
               label = "Washington", hjust = 1.2, 
               color = MyCol[which(AllStates == "Washington")]) +
      
      # Emphasizing New York data
      geom_line(data = C19_states %>% filter(state == "New York"),
                size = 3) +
      annotate("text", x = as.Date("2020-03-20"), 
               y = C19_states %>% filter(date == as.Date("2020-03-20") &
                                               state == "New York") %>% 
                     pull(cases),
               label = "New York", hjust = 1.2, 
               color = MyCol[which(AllStates == "New York")]) +
      
      scale_y_log10() +
      xlab("Date") + 
      ylab(paste("Cumulative number of cases as of", 
                 LatestDate)) +
      ggtitle("Cumulative Covid-19 cases by state",
              subtitle = paste("Data from https://github.com/nytimes/covid-19-data, accessed on",
                               Today))
ggsave("Cumulative Covid-19 cases by state.png", 
       width = 12, height = 6)







