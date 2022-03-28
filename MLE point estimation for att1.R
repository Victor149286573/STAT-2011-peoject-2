#mle for att1

#fucnction
LL <- function(beta, sigma){
  R = dnorm(x, beta, sigma, log = TRUE)
  -sum(R)
}

#library
library(bbmle)

#
fit_norm <- mle2(LL, start = list(beta = 0, sigma = 1), lower = c(-Inf, 0), upper = c(Inf, Inf), method = 'L-BFGS-B')