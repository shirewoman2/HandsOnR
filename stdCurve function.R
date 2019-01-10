# stdCurve function

# This function fits data for a standard curve. Input:
# 1. the data.frame with the data
# If the data are the raw peak heights or area:
#     2. the column with the raw peak height or area
#     3. the column with the IS peak height or area
# OR if the data are the peak heights or area already
# normalized to the internal standard peak height or area
#     4. the column with the normalized peak height or area
# 5. the column with the known concentrations
# 6. whether to fit the data to a 1st or 2nd order polynomial.
# 7. a vector of weights to use for the regression. If left as NA, 
#    no weighting scheme will be used.
# 8. what column to color the points by. If left as NA, points will all be black.

# Output: 
#   1. a plot of the curve and the fitted line, 
#   2. a list object that is the fitted parameters, and 
#   3. a data.frame of the points used for graphing the fitted line.


stdCurve <- function(DF, rawPeak, rawIS, normPeak = NA, 
                     conc, poly = "1st", weights = NULL, 
                     colorBy = NA) {
      
      if(is.na(normPeak)){
            # Normalized peak height or area is not given, only raw
            names(DF)[names(DF) == rawPeak] <- "rawPeak"
            names(DF)[names(DF) == rawIS] <- "rawIS"
            DF$normPeak <- DF$rawPeak/DF$rawIS
      } else {
            names(DF)[names(DF) == normPeak] <- "normPeak"
      }
      names(DF)[names(DF) == conc] <- "CONC"
      
      MaxConc <- max(DF$CONC, na.rm = TRUE)
      
      if(poly == "1st"){
            Fit <- lm(DF$normPeak ~ DF$CONC, weights = weights)
            beta0 <- summary(Fit)$coef["(Intercept)", "Estimate"]
            beta1 <- summary(Fit)$coef["DF$CONC", "Estimate"]
            
            Curve <- data.frame(CONC = seq(0, MaxConc, length.out = 100))
            Curve$normPeak <- beta1 * Curve$CONC + beta0
      } 
      
      if(poly == "2nd"){
            Fit <- nls(normPeak ~ beta2*CONC^2 + beta1*CONC + beta0,
                       data = DF,
                       start = list(beta2 = 0.01, 
                                    beta1 = summary(lm(
                                          DF$normPeak ~ DF$CONC))$coef[
                                                "DF$CONC", "Estimate"],
                                    beta0 = 0),
                       weights = weights)
            beta0 <- summary(Fit)$coef["beta0", "Estimate"]
            beta1 <- summary(Fit)$coef["beta1", "Estimate"]
            beta2 <- summary(Fit)$coef["beta2", "Estimate"]
            
            Curve <- data.frame(CONC = seq(0, MaxConc, length.out = 100))
            Curve$normPeak <- beta2*Curve$CONC^2 + beta1*Curve$CONC + beta0
      }
      
      if(is.na(colorBy)){
            
            CurvePlot <- ggplot(DF, aes(x = CONC, y = normPeak)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = CONC, y = normPeak))
            
      } else {
            
            names(DF)[names(DF) == colorBy] <- "COLOR"
            
            CurvePlot <- ggplot(DF, aes(x = CONC, y = normPeak, 
                                        color = COLOR)) +
                  geom_point() +
                  geom_line(data = Curve, aes(x = CONC, y = normPeak),
                            inherit.aes = FALSE) +
                  labs(color = colorBy)
            
      }
      
      names(Curve)[names(Curve) == "CONC"] <- conc
      names(Curve)[names(Curve) == "normPeak"] <- normPeak
      
      CurveResults <- list(Fit, CurvePlot, Curve)
      names(CurveResults) <- c("Fit", "CurvePlot", "Curve.DF")
      
      return(CurveResults)
      
}
