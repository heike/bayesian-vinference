
alpha.ml <- function(p, weight = NULL, eps = 10^(-7)) {
  # matrix p
  if (is.null(dim(p))) {
    m <- length(p)
    n <- 1
    p <- matrix(p, nrow = 1)
  } else {
    n <- dim(p)[1]
    m <- dim(p)[2]
  }
  if (is.null(weight)) {
    weight <- rep(1, n)
  } else {
    n <- sum(weight)
  }

  weight <- weight / sum(weight)
  ps <- p
  ps[p < eps] <- eps # make sure we don't take the log of 0
  ps <- ps / rowSums(ps)
  logp <- sum(weight * rowSums(log(ps))) / m
  ml <- function(alpha) {
    digamma(alpha) - digamma(alpha * m) - logp
  }
  # find alpha such that digamma(alpha) - digamma(alpha*m) = logp
  alpha <- uniroot(f = ml, interval = c(eps, 10))

  alpha$root
}
