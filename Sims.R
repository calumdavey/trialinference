# Load packages 
library(data.table)

# Generate population 
data <- data.table(
  ID=seq(1,20000,1), # Unique ID for each child 
  Maths=rnorm(20000, mean=38, sd=10),
  Bullied=rbinom(n=20000, size=1, prob=0.10)*100)

# Do trial function   
doTrial <- function(outcome=NULL){
  outcome <- outcome[order(sample(length(outcome), length(outcome)))]
  (mean(outcome[1:(length(outcome)/2)]) - 
     mean(outcome[(1+length(outcome)/2):length(outcome)]))/sd(outcome)
}

# Draw 10,000 samples of 1000 & conduct trials
set.seed(41)
s <- replicate(10000, data[sample(nrow(data), 1000), doTrial(outcome = Maths)])
  # SD is SE (100 SEs from 100 trials each)
  ses <- unlist(lapply(split(s, seq(1,length(s),by=100)), sd), use.names = F)
  par(mfrow=c(1,2))
  hist(ses)

# Draw 100 samples of 1000 & use permutation test (100 perms) to get SE (100 SEs)
s <- replicate(100, data[sample(nrow(data), 1000), Maths])
  # SD is SE (100 SEs from the permutation of 100 trials of 1000 children)
  ses <- apply(s, 2, function(X) sd(replicate(100, doTrial(outcome = X))))
  hist(ses)

