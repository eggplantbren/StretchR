# Main executable for affine-invariant Nested Sampling
source('model.R')

numDimensions = as.integer(20)
numWalkers = as.integer(10)

# Initialise the walkers from the prior
walkers <- array(NA, c(numWalkers, numDimensions))
for(i in 1:numWalkers)
	walkers[i, ] = fromPrior(numDimensions)



