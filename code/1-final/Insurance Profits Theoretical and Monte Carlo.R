####what policy rate to set for a given probability of losing money?####
library(tidyverse)
#parameters
p <- .05 #probability of payout
l <- -12000 #average loss in case of payout
n <- 10000 #number of policies
B <- 10000 #number of repetitions of monte carlo

#calculate premium for 1% max chance of losing money
z <- qnorm(0.01) 

#calculate required profit (premium) when no payout for 1% max chance of losing money
#formula from HarvardX Probability the big short interest rates video
premium <- -l*(n*p - z*sqrt(n*p*(1 - p))) / (n*(1 - p) + z*sqrt(n*p*(1 - p)))
premium #required profit when there is not a payout

#validate with monte carlo
mc_1 <- replicate(B, {
  policy <- sample(c(l, premium), n, replace = TRUE, prob = c(p, 1 - p))
  sum(policy)
})

exp_value <- premium*(1 - p) + l*p #exp value of a policy
mean(mc_1) #mean profit
sd(mc_1)/sqrt(n)   #std error of profits 
mean(mc_1 < 0) #% of times we lose money


####number of policies needed for a 1% max chance of losing money at p = .06####
###doesn't work for values of p larger than .05
p2 <- .045
n2 <- ceiling((z^2*(premium - l)^2*p2*(1 - p2)) / (l*p2 + premium*(1 - p2))^2)
n2

#validate with mc
mc_2 <- replicate(B, {
  policy2 <- sample(c(l, premium), n2, replace = TRUE, prob = c(p2, 1 - p2))
  sum(policy2)
})

mean(mc_2)
sd(mc_2)/sqrt(n2)
mean(mc_2 < 0)

####calculate probability of losing money with a random payout rate####
set.seed(1)
mc_3 <- replicate(B, {
  p3 <- p + sample(seq(0, .02, length.out = 10), 1)
  policy3 <- sample(c(l, premium), n, replace = TRUE, prob = c(p3, 1 - p3))
  sum(policy3)
})


mean(mc_3)
sd(mc_3)/sqrt(n)
mean(mc_3 < 0)