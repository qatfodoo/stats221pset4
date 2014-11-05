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

# Chain sampling S.exp = N * theta with wrong acceptance rate
## (hides non-symmetry)
imp.chain_Sexplogonly <- mcmc.mh_Sexplogonly(imp, max_x=200)
wat.chain_Sexplogonly <- mcmc.mh_Sexplogonly(wat, max_x=600)

## Chain diagnosis
mcmc.diagnosis(imp.chain_Sexplogonly)
mcmc.diagnosis(wat.chain_Sexplogonly)
## Chain rubin-gelman
rubin.gelman(imp, mcmc.mh_Sexplogonly)
rubin.gelman(wat, mcmc.mh_Sexplogonly)

# Chain using direct geom and beta
imp.chain_dir <- mcmc.mhdir(imp)
wat.chain_dir <- mcmc.mhdir(wat, max_x=400)

## Chain diagnosis
mcmc.diagnosis(imp.chain_dir)
mcmc.diagnosis(wat.chain_dir)
## Chain rubin-gelman
rubin.gelman(imp, mcmc.mhdir)
rubin.gelman(wat, mcmc.mhdir)

