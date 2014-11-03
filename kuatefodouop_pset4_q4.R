source("mcmc_raft.R")

## question 1.4

imp <- read.table("./dat/impala.txt")[2:6, ]
imp <- as.numeric(levels(imp))[imp]
wat <-  read.table("./dat/waterbuck.txt")[2:6, ]
wat <- as.numeric(levels(wat))[wat]

# 2 steps chains
imp.chain_2st <- mcmc.mh2step(imp)
wat.chain_2st <- mcmc.mh2step(wat)

## 2 step diagnosis
mcmc.diagnosis(imp.chain_2st)
mcmc.diagnosis(wat.chain_2st)
## 2 step rubin gelman
rubin.gelman(imp, mcmc.mh2step)
rubin.gelman(wat, mcmc.mh2step)

# Chain sampling S.exp = N * theta
imp.chain <- mcmc.mh(imp)
wat.chain <- mcmc.mh(wat, max_x=400)

## Chain diagnosis
mcmc.diagnosis(imp.chain)
mcmc.diagnosis(wat.chain)
## Chain rubin-gelman
rubin.gelman(imp, mcmc.mh)
rubin.gelman(wat, mcmc.mh)
