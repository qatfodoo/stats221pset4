
## Question 1.6

imp <- read.table("./dat/impala.txt")[2:6, ]
imp <- as.numeric(levels(imp))[imp]
wat <-  read.table("./dat/waterbuck.txt")[2:6, ]
wat <- as.numeric(levels(wat))[wat]


## Compute probability analaticaly
prob_less100.imp <- 0
prob_less100.wat <- 0
for (i in 0:100) {
  prob_less100.imp <- prob_less100.imp + imp.post(i)
  prob_less100.wat <- prob_less100.wat + wat.post(i)
}

prob_more100.imp <- 1 - prob_less100.imp
prob_more100.wat <- 1 - prob_less100.wat

imp.post <- function(N) {
  
  norm.const <- 6267314
  if (N < max(imp)) {
    return(0)
  }
  n <- length(imp)
  S <- sum(imp)
  prod.comb <- prod(choose(N, imp))
  post.p <- norm.const * 1 / N * prod.comb * beta(1 + S, 1 + n * N - S)
  if (!is.finite(post.p)) { post.p <- 0 } # Neglect cases of underflow/infinite combinatorics
  
  return(post.p)
}

wat.post <- function(N) {
  
  norm.const <- 525394839
  if (N < max(wat)) {
    return(0)
  }
  n <- length(wat)
  S <- sum(wat)
  prod.comb <- prod(choose(N, wat))
  post.p <- norm.const * 1 / N * prod.comb * beta(1 + S, 1 + n * N - S)
  if (!is.finite(post.p)) { post.p <- 0 } # Neglect cases of underflow/infinite combinatorics
  
  return(post.p)
}