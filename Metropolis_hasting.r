# metropolis-hastings to generate posteria for normal data with mean mu and variance sigma^2
# mu ~ norm(0, 100)    sigma^2 ~ (100)
# esitmate posteria  f(mu, sigma | data)  using metropolis-hastings 

d = 0.5
d2 = 0.5

mu = 0
sigma = 1
nruns = 1000
data = c(-2.1, 1.9, 0.8, -0.7, -1.0)

mustore = double(nruns)
sigmastore = double(nruns)

for (run in 1:nruns){
  munew = mu + runif(1, -d, d)
  sigmanew = sigma + runif(1, -d2, d2)
  num = dnorm(munew, 0, 10) * dexp(sigmanew^2 , 1) * prod(dnorm(data, munew, sigmanew))
  den = dnorm(mu, 0, 10) * dexp(sigma^2 , 1) * prod(dnorm(data, mu, sigma))
  acc = num / den
  r = runif(1,0,1)
  
  if (r < acc){
    mu = munew
    sigma = sigmanew
  }
  mustore[run] = mu
  sigmastore[run] = sigma
}

plot(mustore, type = 'l')
