# starSig function

# This function checks whether the p value input is below a certain 
# threshold, rounds to the determined number of sig figs, and marks it with an
# asterisk or asterisks as appropriate to its significance level.

# Input: 
# pval = a vector of p values
# sig = the number of significant digits to include
# starlevels = a vector of the three values to use for significance marked with
# ***, **, or *. 
# Output: character vector of rounded numbers with asterisks, 
# e.g., c("0.032 *", "0.00028 **", "0.97", "0.000084 ***")

starSig <- function(pval, sig = 2, 
                    starlevels = c(0.001, 0.01, 0.05)) {
      star <- ifelse(pval < starlevels[1], "\\*\\*\\*",
                     ifelse(pval < starlevels[2], "\\*\\*",
                            ifelse(pval < starlevels[3], "\\*", "")))
      return(paste0(signif(pval, sig), star))
}

