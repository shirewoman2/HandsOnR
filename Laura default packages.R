# Laura's default packages and settings
# Also available in a Git repo at https://gitlab.cs.washington.edu/PKLab/GeneralScripts

library(lubridate)
library(gridExtra)
library(tidyverse)
library(xlsx)
library(ggpubr)
library(scales)
library(LaurasHelpers)


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

scale_colour_discrete <- function(...) scale_colour_brewer(..., palette="Set1")
scale_fill_discrete <- function(...) scale_fill_brewer(... , palette="Set1")

# Call up that theme before plotting graphs.
theme_set(ThemeLaura())

if(Sys.info()[["nodename"]] == "SOP-D-2FFVP52"){ # Brunhilde
      GenScriptDir <- "C:/Users/shireman.NETID/Documents/R/General scripts"
}

if(Sys.info()[["nodename"]] == "BUFFY"){
      GenScriptDir <- "C:/Users/Laura Shireman/Documents/R/General scripts"
}

colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                 "darkorange", "green3", "seagreen3",
                                 "cadetblue", "dodgerblue3", "royalblue4",
                                 "darkorchid4"))

blueGreen <- colorRampPalette(c("green3", "seagreen3", "cadetblue", "dodgerblue3",
                                "royalblue4"))



