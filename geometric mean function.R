# geometric mean function

# geometric mean function from Stack Overflow
# ref: https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
# When there are no zeroes in a vector of numbers, then the geometric mean
# is defined as the nth root of the product of all the values 
# AND IS EQUIVALENTLY DEFINED AS 
# exp(1/n * the sum of the logs of each value). 
# However, those two are not equivalent if there are zeroes!
# See notes in my document "Notes on Statistics.docx" in my personal 
# OneDrive folder "Documents/Science references/Summary and reference notes".

# This function gives the option of propagating zeroes. If you want 
# to get a 0 when the vector includes 0 values, that's the definition 
# of the nth root of the product of all values, so that would be 
# zero.propagate = TRUE. 
# Alternatively, if you want to just ignore 0 values, you can choose
# zero.propagate = FALSE. Then, either definition works, and, since 
# R has a built-in sum(...) function but not a built-in product(...) 
# function, you use the definition where the geometric mean =
# exp(1/n * the sum of the logs of each value) and that will give you 
# an actual number if you ignore the zeroes.

# I'm not positive that we want to include the zeroes in the calculation
# of the length of the vector, and this affects the output value, so it 
# does matter. In the StackOverflow answer cited above, there are a 
# few comments on this. I *think* that zeroes should be excluded
# from the length if they're excluded from the rest of the calculation,
# so I'm excluding them. However, I hasten to note that that's not
# the choice the author made. 

# Note that the geometric mean is undefined for negative numbers.

gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){
      
      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }
      
      if(zero.propagate){
            if(any(x == 0, na.rm = TRUE)){
                  return(0)
            }
      } else { # If you don't want to propagate zeroes, then remove them from the vector.
            x <- x[x > 0]
      }
      
      return(exp(mean(log(x), na.rm = na.rm)))
      
}


# Per Wikipedia's entry on the geometric standard deviation, the geometric
# standard deviation is dimensionless. See that entry for the formula used 
# below. 

# Also, note that the way you determine the range of the data 
# for the geometric mean +/- one geometric standard deviation is NOT
# geometric mean value +/- the geometric standard deviation! Instead, it is
# geometric mean / geometric standard deviation to geometric mean * geometric
# standard deviation!

gm_sd <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
      
      if(na.rm){
            x <- x[complete.cases(x)]
      }
      
      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }
      
      if(zero.propagate){
            if(any(x == 0, na.rm = TRUE)){
                  return(0)
            }
      } else { # If you don't want to propagate zeroes, then remove them from the vector.
            x <- x[x > 0]
      }
      
      # Now, proceed with whatever your vector is after removing zeroes (or not 
      # removing them if zero.propagate was TRUE but there weren't any zeroes 
      # to start with anyway.)
      ToSum <- rep(NA, length(x))
      # There's probably a way to do this next bit without actually writing
      # a loop, but I'm not sure how. 
      for(i in 1:length(x)){
            ToSum[i] <- (log(x[i]/gm_mean(x)))^2
      }
      
      return(exp(sqrt(sum(ToSum)/(length(x))))) # This is population geometric 
      # standard deviation. For *sample* geometric sd, you'd want the denominator
      # to be N-1 instead of N. 
}


gm_95CI <- function(x, na.rm = TRUE) {
      
      # If any values are negative, return NaN.
      if(any(x < 0, na.rm = TRUE)){
            return(NaN)
      }
      
      return(log10(quantile(10^x, c(0.025, 0.975))))
      
}

# Here is the markdown syntax for the equations for geometric mean and sd: (remove extra spaces)

# Geometric mean: $$ \text{geometric mean} = \mu_g = \Bigg(\displaystyle\prod_{i=1}^{n} x_i\Bigg)^\frac{1}{n}
# = \sqrt[n]{x_1 \times x_2  \times x_3  \times \dots  \times x_n}$$
# 
# when there are no zero values equivalent to: $$\text{geometric mean} =
# \text{exp}\bigg(\frac{1}{n} \sum_{i=1}^{n} ln(x_i)\bigg) = e^{\frac{1}{n} log(x_1 + x_2
# + x_3  + \dots  + x_n)} = \text{exp}(\text{mean}(ln(x)))$$
# 
# Geometric standard deviation: $$\text{geometric standard deviation} =
# \sigma_g = exp \sqrt{ \frac{ \sum_{i=1}^{n} \big(ln \frac{A_i}{\mu_g}\big)^2 }{n-1} } $$


