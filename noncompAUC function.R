# This function performs noncompartmental analysis of pharmacokinetic
# data using the trapezoid rule.

noncompAUC <- function(DF, concentration = "Concentration",
                       time = "Time") {
      # DF = a data.frame including a column Time and a column of the 
      # concentrations of the analyte of interest. To analyze a data.frame
      # MyData with columns "Time.min" and
      # "Norbup.Conc", use the syntax:
      #     noncompAUC(MyData,"Norbup.Conc", "Time.min")
      
      require(plyr)
      
      names(DF)[names(DF) == concentration] <- "Conc"
      names(DF)[names(DF) == time] <- "Time"
      
      DF$TimePt <- as.factor(DF$Time)
      DFmean <- ddply(DF, "TimePt", function(x) c(
            Time = mean(x$Time, na.rm = TRUE),
            Conc = mean(x$Conc, na.rm = TRUE)
      ))
      
      DFmean <- arrange(DFmean, Time)
      
      # Removing rows where Conc = NaN b/c can't calculate otherwise.
      DFmean <- DFmean[complete.cases(DFmean$Conc), ]
      
      AUC <- sum(0.5*((DFmean$Time[2:length(DFmean$Time)] - 
                             DFmean$Time[1:(length(DFmean$Time)-1)]) * 
                            (DFmean$Conc[2:length(DFmean$Conc)] +
                                   DFmean$Conc[1:(length(DFmean$Conc)-1)])))
      
      return(AUC)
}
