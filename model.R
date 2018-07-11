# This file contains functions that define what model we're working on.
numDimensions <- as.integer(10)

# Given a vector of parameters, evaluate the natural logarithm of the prior
# probability density.
logPrior <- function(params)
{
	result <- 0.
	if(any(params < -10 | params > 10))
		result <- -Inf
	return(result) 
}

# Call this function to generate a single point in parameter space
# for starting an MCMC chain.
startingPoint <- function()
{
	params <- -10. + 20.*runif(numDimensions)
	return(params)
}

# Given a vector of parameters, evaluate the natural logarithm of the
# likelihood function.
logLikelihood <- function(params)
{
	alpha = 0.9
	logL <- -0.5*params[1]**2
	logL = logL - 0.5*sum((params[2:numDimensions] - alpha*params[1:numDimensions-1])**2)/(1. - alpha**2)
	return(logL)
}


