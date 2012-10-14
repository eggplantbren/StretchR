# This file contains functions that define what model we're working on.
numDimensions <- as.integer(10)

# Given a vector of parameters, evaluate the natural logarithm of the prior
# probability density.
logPrior <- function(params)
{
	result <- 0.
	if(any(params < 0 | params > 1))
		result <- -Inf
	return(result) 
}

# Call this function to generate a single point in parameter space
# for starting an MCMC chain.
startingPoint <- function()
{
	params <- runif(numDimensions)
	return(params)
}

# Given a vector of parameters, evaluate the natural logarithm of the
# likelihood function.
logLikelihood <- function(params)
{
	logL <- -0.5*sum(((params - 0.5)/0.01)^2)
	return(logL)
}


