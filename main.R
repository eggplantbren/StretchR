# Main executable for affine-invariant sampling
# This just targets the posterior using emcee-style stretch moves
source('model.R')

numWalkers <- as.integer(2*numDimensions)
if(numDimensions >= numWalkers)
	print('WARNING: Use more walkers!')

# Initialise the walkers from the prior
walkers <- array(NA, c(numWalkers, numDimensions)) # Empty array
logP <- array(NA, c(numWalkers, 1)) # logPrior values
logL <- array(NA, c(numWalkers, 1)) # logLikelihood values

# Fill the array. Each row is a walker
for(i in 1:numWalkers)
{
	walkers[i, ] <- startingPoint()
	logP[i] <- logPrior(walkers[i, ])
	logL[i] <- logLikelihood(walkers[i, ])
}

# MCMC parameters
steps <- as.integer(20000*numWalkers)
skip <- as.integer(steps/10000)

# Storage
keep <- array(NA, c(steps/skip, numDimensions))
tries <- as.integer(0)
accepts <- as.integer(0)

# Main MCMC loop
for(i in 1:steps)
{
    # Which walker to save/plot
    which = sample(1:numWalkers, 1)

	# Plotting stuff
	if(i%%skip == 0)
	{
		keep[(i/skip), ] <- walkers[which, ]

		# Trace plot of first parameter
		plot(seq(1, i/skip)/(steps/skip)*100,
             keep[1:(i/skip), 1],
             type='l', xlab='% complete', ylab='Value')
		print('Acceptance Ratio: ')
		print(as.double(accepts)/tries)

		# Ad-hoc burn-in discard
		start = 1. + 0.5*(i/skip)
		print('Mean of last 50% of run: ')
		print(mean(keep[start:(i/skip), 1]))
		print('Stdev of last 50% of run: ')
		print(sd(keep[start:(i/skip), 1]))
	}

	# Which walker to move
	which <- (i-1) %% numWalkers + 1
	
	# Do the stretch move
	other <- sample(1:numWalkers, 1)
	while(which == other)
		other <- sample(1:numWalkers, 1)

	# Generate the proposal
	Z <- 0.5*(runif(1) + 1.)**2
	proposal <- Z*walkers[which, ] + (1. - Z)*walkers[other, ]
	logP_proposal <- logPrior(proposal)
	logL_proposal <- logLikelihood(proposal)

	# Calculate acceptance probability
	logA <- logP_proposal - logP[which]
	logA <- logA + logL_proposal - logL[which]
	logA <-	logA + (numDimensions - 1)*log(Z)
	if(logA > 0.)
		logA = 0.

	# Accept?
	if(runif(1) <= exp(logA))
	{
		walkers[which, ] = proposal
		logP[which] = logP_proposal
		logL[which] = logL_proposal
		accepts <- accepts + as.integer(1)
	}
	tries <- tries + as.integer(1)
}

