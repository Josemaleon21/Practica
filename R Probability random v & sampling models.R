
# Random Variables

library(tidyverse)
library(dslabs)


options(digits = 3)
options(scipen = 99)

beads <- rep( c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

n <- 1000
B <- 10000

S <- replicate(B, {
  X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19)) 
  sum(X)
})

mean(S)

mean(S < 0)

s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")



set.seed(1)

green <- 2
black <- 18
red <- 18

p_green <- green / (green+black+red)

p_green
1-p_green

X <- sample(c(17,-1), 1, prob = c(p_green, p_not_green))
X

abs(17-(-1))*sqrt(p_green*p_not_green)

X <- sample(c(17,-1), n, prob = c(p_green, p_not_green), replace = TRUE)

p_green*17 + p_not_green*(-1)

16*sqrt(p_green*p_not_green)


S <- replicate(B, {
  X <- sample(c(17,-1), n, prob = c(p_green, p_not_green))
  sum(X)
})




# Exercise SAT test

n <- 44
B <- 10000
p_guess <- 0.25
p_not_guess <- 0.75

X <- sample(c(1, 0), n, prob = c(p_guess, p_not_guess), replace = TRUE)

1 - pnorm(8, avg, se)

n*(avg <- 1*p_guess + 0*p_not_guess)

se <- sqrt(n)*abs(1 - -0.25)*sqrt(p_guess*p_not_guess)

set.seed(21)

S <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(p_guess, p_not_guess)) 
  sum(X)
})

mean(S >= 8)


p <- seq(0.25, 0.95, 0.05)


expected_value <- 44 * (1*p + 0*(1-p))
# calculate the standard error at given p
standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
# calculate likelihood of score of 35 or greater
1-pnorm(35, expected_value, standard_error)




n <- 500
B <- 10000
p_win <- 5/38
p_not_win <- 33/38


X <- sample(c(6, -1), n, prob = c(p_win, p_not_win), replace = TRUE)

avg <- 6*p_win + -1*p_not_win
se <- (abs(6 - -1)*sqrt(p_win*p_not_win))*sqrt(n)

avg <- n*avg

200000*(0.02/0.98)

pnorm(0, avg, se)
sum(X)



# Exercise about banks rates

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Monte Carlo Simulation

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})


library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")


esperanza <- n*(p*loss_per_foreclosure + (1-p)*0)     
error_s <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))  


x <- - loss_per_foreclosure*p/(1-p)
x

interest_rate <- x/180000

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    
n*(loss_per_foreclosure*p + x*(1-p))

# Monte Carlo Simulation

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)  





defaults <- sample( c(0,1), n, replace = TRUE, prob=c(1-p_default, p_default))

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults * loss_per_foreclosure)
S

S <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})


# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)