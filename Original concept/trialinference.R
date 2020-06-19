# Trial inference with permutation 
# Calum Davey
# LSHTM 
# 18 MAR 2020 

# Generate population 
pop <- rnorm(10000, mean=0, sd=1)

# Sample from the population 
sampsize <- 500
sample <- sample(pop, size = sampsize)

# Assign to two groups 
arms <- rep(c(0,1), each = sampsize/2)

# Calculate difference in the means 
means <- aggregate(data, by=list(arms), 'mean')
effect <- means[1,2]-means[2,2]

# Permute assignment to two groups, calcuate difference in means 
perm <- function(arms, data){
  arm <- sample(arms, size=sampsize, replace=FALSE)
  means <- aggregate(data, by=list(arm), 'mean')
  return(means[1,2]-means[2,2])
}

results <- replicate(1000, perm(arms, sample))

# Distribution of effects == SE
sd(results)

# Standard error from CLT 
sds <- aggregate(sample, by=list(arms), 'sd')
((sds[1,2]^2)/(sampsize/2-1) + (sds[2,2]^2)/(sampsize/2-1))^.5
