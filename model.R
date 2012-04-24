# This file contains functions that define what model we're working on.

# Given a vector of parameters, evaluate the natural logarithm of the prior
# probability density.
logPrior <- function(params)
{
	result <- 0.
	if(any(abs(params) > 10.))
		result <- -Inf
	return(result) 
}

# Call this function to generate a single point in parameter space
# for starting an MCMC chain.
fromPrior <- function(dimensions=20)
{
	params <- -10. + 20.*runif(dimensions)
}

# Given a vector of parameters, evaluate the natural logarithm of the
# likelihood function.
logLikelihood <- function(params)
{
	logP <- -0.5*sum(params**2)	
}


