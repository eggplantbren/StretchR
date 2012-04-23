#
source('model.R')

params <- fromPrior(dimensions=20)
logP <- logPrior(params)
logL <- logLikelihood(params)

print(params)
print(logP)
print(logL)

