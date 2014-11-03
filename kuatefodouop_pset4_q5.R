
## Question 1.5

imp <- read.table("./dat/impala.txt")[2:6, ]
imp <- as.numeric(levels(imp))[imp]
wat <-  read.table("./dat/waterbuck.txt")[2:6, ]
wat <- as.numeric(levels(wat))[wat]

imp.const <- sum.const(imp)
wat.const <- sum.const(wat)

## Check dist intgrates to 1
const.check(imp, imp.const) # 1
const.check(wat, wat.const) # 1


# Posterior prob of N (unnormalized)
post.prob <- function(y, N) {
  if (N < max(y)) {
    return(0)
  }
  n <- length(y)
  S <- sum(y)
  prod.comb <- prod(choose(N, y))
  post.p <- 1 / N * prod.comb * beta(1 + S, 1 + n * N - S)
  if (!is.finite(post.p)) { post.p <- 0 } # Neglect cases of underflow/infinite combinatorics
  
  return(post.p)
}

# Bare summation
sum.const <- function(y, nmax=10000) {
  sum <- 0
  for (i in 1:nmax) {
    sum <- sum + post.prob(y, i)
  }
  return(1 / sum)
}

# Importance sampling using geom distribution
imp.const <- function(y, mu=100, niters=10000) {
  sum <- 0
  for (i in 1:niters) {
    N <- rgeom(1, 1 / mu)
    sum <- sum + 1 / niters * (post.prob(y, N) / dgeom(N, 1 / mu))
  }
  return(1 / sum)
}

# Check integrate to 1
const.check <- function(y, const, nmax=10000) {
  sum <- 0
  for (i in 1:nmax) {
    sum <- sum + const * post.prob(y, i)
  }
  return(sum) # Should be 1
}