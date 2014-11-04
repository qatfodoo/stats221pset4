source("mcmc_raft.R")

## question 1.4

imp <- read.table("./dat/impala.txt")[2:6, ]
imp <- as.numeric(levels(imp))[imp]
wat <-  read.table("./dat/waterbuck.txt")[2:6, ]
wat <- as.numeric(levels(wat))[wat]

# 2 steps chains
imp.chain_2st <- mcmc.mh2step(imp)
wat.chain_2st <- mcmc.mh2step(wat, max_x=400)

## 2 step diagnosis
mcmc.diagnosis(imp.chain_2st)
mcmc.diagnosis(wat.chain_2st)
## 2 step rubin gelman
rubin.gelman(imp, mcmc.mh2step)
rubin.gelman(wat, mcmc.mh2step)

# Chain sampling S.exp = N * theta
imp.chain_Sexp <- mcmc.mh_Sexp(imp)
wat.chain_Sexp <- mcmc.mh_Sexp(wat, max_x=400)

## Chain diagnosis
mcmc.diagnosis(imp.chain_Sexp)
mcmc.diagnosis(wat.chain_Sexp)
## Chain rubin-gelman
rubin.gelman(imp, mcmc.mh_Sexp)
rubin.gelman(wat, mcmc.mh_Sexp)

# Chain using 
imp.chain <- mcmc.mhdir(imp)
wat.chain <- mcmc.mhdir(wat, max_x=400)

## Chain diagnosis
mcmc.diagnosis(imp.chain)
mcmc.diagnosis(wat.chain)
## Chain rubin-gelman
rubin.gelman(imp, mcmc.mhdir)
rubin.gelman(wat, mcmc.mhdir)

