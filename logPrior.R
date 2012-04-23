# Given a vector of parameters, evaluate the natural logarithm of the prior
# probability density.
logPrior <- function(params)
{
	logP <- -0.5*sum(params^2)
}

