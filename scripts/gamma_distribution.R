# understanding gamma distribution

# couple observations: if mean and sd are increased
# by same percentage then each percentile increases by the same percentage
#  if mean and sd are the same then it is monotonically decreasing prob
# density. If sd<mean, then there is a mode to the right of zero. 

alpha <- 1 # shape
beta <- 10 # rate

mu = 0.7*c(1, 1.25, 1.25, 1.25, 1.5, 1.5)
sigma = 0.5*c(1, 1, 1.25, 1.5, 1.5, 2)

alpha <-  mu^2/sigma^2
beta <-  mu/sigma^2


x <- seq(from = 0, to = 4, by = 0.01)

plot(x, dgamma(x, alpha[c(1,  4)], rate = beta[c(1, 4)]), type = "p")
abline(v = mu[c(1, 4)])



q <- qgamma(0.99, alpha, beta)
q
q/q[1]
