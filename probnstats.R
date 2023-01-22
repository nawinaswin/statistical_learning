write r code to find solution to the matching problem in probability 

# function to calculate the probability of a birthday match
birthday_match_prob <- function(n) {
  # 365 days in a year
  p <- 1
  for (i in 1:(n-1)) {
    p <- p * ((365-i)/365)
  }
  1 - p
}

# test the function
print(birthday_match_prob(23))
