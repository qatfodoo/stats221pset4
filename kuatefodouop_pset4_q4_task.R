source("mcmc_raft.R")

## question 1.4 on Odyssey

data.names <- c("impala", "waterbuck")
N.it <- 1e5

# 10 iterations of each chain, save plot and store chains
if (Sys.getenv("SLURM_JOB_ID") != "") { # Divide computation per tasks
  
  job.id <- as.numeric(Sys.getenv("SLURM_JOB_ID"))
  print(paste("Job id", job.id, sep=": "))
  task.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  print(paste("Task id", task.id, sep=": "))
  data.id <- task.id %% 2 + 1 # Set of data considered
  data.name <- data.names[data.id]
  chain.id <- ceiling(task.id / 2) # Chain id for data set
  
  gc() # garbage collection
  t1.sim <- as.numeric(Sys.time())
  
  data <-  read.table(paste("./dat/", data.name, ".txt", sep=""))[2:6, ]
  data <- as.numeric(levels(data))[data]
  
  png(filename=paste("./out/", data.name, "_chain", chain.id, "_postcont.png", sep=""))
  max_x <- ifelse(data.id == 1, 120, 350)
  chain <- mcmc.mh(data, mcmc.niters=N.it, max_x)
  dev.off()
  save("chain", file=paste("./out/", data.name, "_chain", chain.id, ".RData", sep=""))
  
  t2.sim <- as.numeric(Sys.time())
  dt.sim <- (t2.sim - t1.sim) / 60 # dt in min
  print(paste(paste("Simulation elapsed time (min), task", task.id, sep=" "), dt.sim, sep=": "))

}
