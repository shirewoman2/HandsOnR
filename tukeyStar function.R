# tukeyStar function

# This function makes a boxplot of data compared with an ANOVA and then
# evaluated with a Tukey post-hoc test. 

# INPUT:
# "data" is the data.frame you're plotting with ggplot2
# "Tukey.df" is the data.frame of results from 
# as.data.frame(TukeyHSD(aov(Y ~ X))[[1]])
# "yval" is the column in "data" that the y axis is mapped to. 
# "xval" is the corollary for the x axis. 

tukeyStar <- function(data, Tukey.df, xval, yval,
                      barsize = 1.5, textsize = 8,
                      includeN = FALSE) {
      
      names(data)[names(data) == yval] <- "Y"
      names(data)[names(data) == xval] <- "X"
      
      if(class(data$X) == "factor"){
            Groups <- levels(data$X)
      } else {
            Groups <- levels(as.factor(data$X))
      }
      data$Xorig <- factor(data$X, levels = Groups)
      rm(Groups)
      
      
      if(includeN){
            Count <- ddply(data, "X", function(x) data.frame(
                  N = paste("n =", nrow(x))))
            data <- join(data, Count, by = "X")
            data$X <- paste0(data$X, "\n", data$N)
      }
      
      # Only drawing segments for comparisons with p adjusted < 0.05
      Tukey.df <- Tukey.df[Tukey.df$"p adj" < 0.05 |
                                 is.nan(Tukey.df$"p adj"), ]
      
      plot <- ggplot(data, aes(x = X, y = Y, fill = X)) +
            geom_boxplot() +
            xlab(xval) + ylab(yval) +
            labs(fill = yval) +
            theme(legend.position = "none")
      
      if(nrow(Tukey.df) > 0){
            Comparisons <- row.names(Tukey.df)
            Tukey.df$Comparison <- row.names(Tukey.df)
            Comparisons.l <- str_split(Comparisons, "-")
            names(Comparisons.l) <- row.names(Tukey.df)
            
            Segments <- data.frame(Comparison = Comparisons,
                                   xstart = NA,
                                   xend = NA, 
                                   ystart = NA, 
                                   yend = NA,
                                   
                                   yendV = NA,
                                   
                                   Star = NA,
                                   StarPosX = NA,
                                   StarPosY = NA)
            
            for(l in 1:length(Comparisons)){
                  Segments$xstart[l] <-
                        which(levels(data$Xorig) == Comparisons.l[[l]][1])
                  Segments$xend[l] <-
                        which(levels(data$Xorig) == Comparisons.l[[l]][2])
                  Segments$ystart[l] <- max(data$Y, na.rm = TRUE) +
                        0.1*max(data$Y, na.rm = TRUE)*l
                  Segments$yend[l] <- max(data$Y, na.rm = TRUE) + 
                        0.1*max(data$Y, na.rm = TRUE)*l
                  
                  # Adding vertical bars
                  Segments$ystartV[l] <- Segments$ystart[l]
                  Segments$yendV[l] <- Segments$ystart[l] * 0.95
                  
                  # Determining number of stars
                  Segments$Star[l] <- 
                        ifelse(is.nan(Tukey.df$"p adj"[l]), "can't calc p val",
                               ifelse(Tukey.df$"p adj"[l] < 0.001, "***",
                                      ifelse(Tukey.df$"p adj"[l] < 0.01, "**", "*")))
                  
                  Segments$StarPosX[l] <- mean(c(Segments$xstart[l], 
                                                 Segments$xend[l]))
                  Segments$StarPosY[l] <- max(data$Y, na.rm = TRUE) +
                        0.11*max(data$Y, na.rm = TRUE)*l
                  
            }
            
            for(l in 1:length(Comparisons)){
                  plot <- plot +             
                        geom_segment(data = Segments[l, ], 
                                     aes(x = xstart, xend = xend,
                                         y = ystart, yend = yend),
                                     size = barsize, inherit.aes = FALSE) +
                        # Left vertical segment
                        geom_segment(data = Segments[l, ],
                                     aes(x = xstart, xend = xstart,
                                         y = ystart, yend = yendV),
                                     size = barsize, inherit.aes = FALSE) +
                        # Right vertical segment
                        geom_segment(data = Segments[l, ],
                                     aes(x = xend, xend = xend,
                                         y = ystart, yend = yendV),
                                     size = barsize, inherit.aes = FALSE) +
                        # Adding star
                        geom_text(data = Segments[l, ],
                                  aes(x = StarPosX, y = StarPosY, 
                                      label = Star), 
                                  size = rel(textsize), inherit.aes = FALSE)
            }
      }
      
      return(plot)
}


# tukeyStar(data = LinCheck[LinCheck$Drug == "LPV" &
#                                 LinCheck$Time.Hr == 24, ],
#           Tukey.df = Linearity.Tukey[["LPV.24"]],
#           xval = "Group", yval = "Conc.DoseNorm") +
#       ylab("Concentration dose normalized")

