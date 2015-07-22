library(LearnBayes)
data(studentdata)

head(studentdata)
table(studentdata$Drink)

studentdata$Wakeup
hours.of.sleep <- studentdata$WakeUp - studentdata$ToSleep
summary(hours.of.sleep)
head(hours.of.sleep)
hist(hours.of.sleep, main= "")

boxplot(hours.of.sleep ~ studentdata$Gender)

fit <- lm(hours.of.sleep ~ studentdata$ToSleep)
fit

# calculate the t-stat

x = rnorm(10, mean = 50, sd = 10)
y = rnorm(10, mean = 50, sd = 10)

m = length(x)
n = length(y)

sp = sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m + n -2))
t.stat = sqrt(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))

tstatistic = function(x, y){
	m = length(x)
    n = length(y)

    sp = sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m + n -2))
    t.stat = (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))

    return(t.stat)
}

data.x = c(1,4,3,6,5)
data.y = c(5,4,7,6,10)

mean(data.x)
mean(data.y)

tstatistic(data.x, data.y)

#monte carlo simulation for t-stat, number of rejections/total number

alpha = 0.1; m = 10; n = 10 # sets alpha, m , n
N = 10000                   # sets number of simulations
n.reject = 0 				#counter for number of rejections, start at 0

for(i in 1:N){
	x = rnorm(m, mean = 0, sd = 1) #simulate xs from pop 1
	y = rnorm(n, mean = 0, sd = 1) #simulate xs from pop 2
	t.stat = tstatistic(x,y)       #computes the tstat

	if(abs(t.stat)>qt(1-alpha/2, n + m - 2)) #reject if |T| exceeds critical pt
		n.reject = n.reject + 1
}

true.sig.level = n.reject/N #proportion of rejections
true.sig.level

# Can change things around to do comparisons
# normal pops
x = rnorm(m, mean = 0, sd = 1) 
y = rnorm(n, mean = 0, sd = 1)

#normal pops with zero means and different variance
x = rnorm(m, mean = 0 , sd = 1)
y = rnorm(n, mean = 0, sd = 10)

#T populations
x = rt(m, df = 4)
y = rt(n, df = 4)

#exponential pops
x = rexp(m, rate = 1)
y = rexp(m, rate = 1)

#mixture
x = rnorm(m, mean = 10, sd = 2)
y = rexp(n , rate = 1/10)

m = 10; n = 10 
my.tsimulation = function()
tstatistic(rnorm(m, mean = 10, sd = 2), rexp(n, rate = 1/10))

tstat.vector = replicate(10000, my.tsimulation())
plot(density(tstat.vector), xlim = c(-5, 8), ylim = c(0, .4), lwd = 3)
curve(dt(x, df = 18), add = TRUE)
legend(4, .3, c("exact", "t(18)"), lwd = c(3,1))

binomial.conf.interval = function(y,n){
	z = qnorm(0.95)
	phat = y/n
	se = sqrt(phat*(1-phat)/n)
	return(c(phat-z*se, phat+z*se))
}

?rbinom
y = rbinom(n = 1, size = 20, prob = 0.5)
y

binout = replicate(20, binomial.conf.interval(y, 20))
binout

# chapter 2

# discrete priors over a grid
p = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)

plot(p, prior, type = "h", ylab= "prior prob")

data = c(11,16)
post = pdisc(p, prior, data)
round(cbind(p, prior, post), 2)

library(lattice)
PRIOR = data.frame("prior", p, prior)
POST  = data.frame("posterior", p , post)
names(PRIOR) = c("Type", "p", "prob")
names(POST) = c("Type", "p", "prob")
data = rbind(PRIOR, POST)

xyplot(prob~p|Type, data = data, layout = c(1,2),
	type = "h", lwd = 3, col = "black")

a = 3.26; b = 7.19; s = 11; f = 16;
curve(dbeta(x, a+s, b+f), from = 0, to = 1, xlab = "p", ylab = "Density", lty = 1, lwd = 4)
curve(dbeta(x, s+1, f+1), add = TRUE, lty = 2, lwd = 4)
curve(dbeta(x, a, b), add = TRUE, lty = 3, lwd = 4)

#calculate the posterior that the proportion of heavy sleepers is greater than 0.5?
1 - pbeta(0.5, a + s, b + f)
# this probability is small

# calculate the 95% CI, this is exact
qbeta(c(0.05, 0.95), a + s, b + f)

# or we could do simulations to get the calculated values
ps = rbeta(1000, a + s, b + f)
hist(ps)
sum(ps >= 0.5)/1000
# calculate the 90 interval from the simulated samples
quantile(ps, c(0.05, 0.95))

# use the histprior function in learnbayes
# create a grid and then calculate the posterior over the grid
midpt = seq(0.05, 0.95, by = 0.1) # set up the grid
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0) # prior weights on the grid
prior = prior/sum(prior)
prior

curve(histprior(x, midpt, prior), from = 0, to = 1, ylab = "Prior density", ylim = c(0, 0.3))
# take a look at the blocky grid

curve(histprior(x, midpt, prior)*dbeta(x, s + 1, f + 1), from = 0, to = 1, ylab = "post density")
p = seq(0, 1, length = 500)
p
post = histprior(p, midpt, prior)*dbeta(p, s+1, f+1)
post = post/sum(post)

# sample with replacement 
ps = sample(p, replace = TRUE, prob = post)
hist(ps)

# output vector
p = seq(0.05, 0.95, by = .1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0 )
prior = prior/sum(prior)
m = 20; ys = 0:20
pred = pdiscp(p, prior, m , ys)
round(cbind(0:20, pred), 3)

# beta prior
ab = c(3.26, 7.19)
m = 20; ys = 0:20
pred = pbetap(ab, m, ys)

#simulate draws from the prior and store them
p = rbeta(1000, 3.26, 7.19)
y = rbinom(1000, 20, p)
freq = table(y) # save the frequencies out of 1000 draws on y
ys = as.integer(names(freq))
predprob = freq/sum(freq)

plot(ys, predprob, type = "h", xlab = "y")
dist = cbind(ys, predprob)
dist 

covprob = 0.9
discint(dist, covprob)

## Chapter 3
## Known means but unknown variance
data(footballscores)

d = footballscores$favorite - footballscores$underdog - footballscores$spread
n = length(d)
v = sum(d^2)

# simulate draws from the chisquared dist 
P = rchisq(1000, n )/v
s = sqrt(1/P)
hist(s ,main = "") # take a look at the simulated sample standard deviation of game outcomes
# and point spreads

quantile(s, probs = c(0.025, 0.5, 0.975))

# hospital example
alpha = 16; beta = 15174; # these are the priors on the gamma distribution
yobs = 1; ex = 66 # this hospital has one death in 66 heart transplants
y = 1:10 # predictive density after observing 10 more heart transplants
lam = alpha/beta
py = dpois(y, lam*ex)*dgamma(lam, shape = alpha, rate = beta)/dgamma(lam, shape = alpha + y, rate = beta + ex)
cbind(y, round(py, 3))

# we can simulate the posterior density
lambdaA = rgamma(1000, shape = alpha + yobs, rate = beta + ex)
hist(lambdaA)

# now lets take a look at another hospital with more surgeries 
ex = 1767; yobs = 4
y = 0:10

py = dpois(y, lam*ex)*dgamma(lam, shape = alpha, rate = beta)/dgamma(lam, shape = alpha + y, rate = beta + ex)
cbind(y, round(py, 3))

lambdaB = rgamma(1000, shape = alpha + yobs, rate = beta + ex)

# now take a look at the comparisons between the prior and posterior of each hospital where the first 
# hospital has less experience with surgeries, while the second hospital has much more experience
# you will see that for hospital 2 there is less influence of the prior.

par(mfrow = c(2, 1))
plot(density(lambdaA), main = "HOSPITAL A", xlab = "lambdaA", lwd = 3)
curve(dgamma(x, shape = alpha, rate = beta), add = TRUE)
legend("topright", legend = c("prior", "posterior", lwd = c(1,3)))


plot(density(lambdaB), main = "HOSPITAL B", xlab = "lambdaB", lwd = 3)
curve(dgamma(x, shape = alpha, rate = beta), add = TRUE)
legend("topright", legend = c("prior", "posterior", lwd = c(1,3)))

mu = 100
tau = 12.16
sigma = 15
n = 4
se = sigma/sqrt(n)
ybar = c(110, 125, 140) # observed IQ test scores
tau1 = 1/sqrt(1/se^2 + 1/tau^2)
mu1 = (ybar/se^2 + mu/tau^2)* tau1^2
summ1 = cbind(ybar, mu1, tau1)
summ1

# choose another prior with the same median, 100, but from a t distribution
tscale = 20/qt(0.95, 2)
tscale

# compare the normal vs. the t dist, notice the t density has sig flatter tails
par(mfrow= c(1, 1))
curve(1/tscale*dt((x - mu)/tscale, 2), 
	from = 60, to = 140, xlab = "theta", ylab = "Prior Density")
curve(dnorm(x, mean = mu, sd = tau), add = TRUE, lwd = 3)
legend("topright", legend = c("t density", "normal density"), lwd = c(1, 3))

# use the discrete distribution to compute the posterior mean and posterior sd

norm.t.compute = function(ybar){
	theta = seq(60, 180, length = 500)
	like  = dnorm(theta, mean = ybar, sd = sigma/sqrt(n))
	prior = dt((theta - mu)/tscale, 2)
	post  = prior*like
	post  = post/sum(post)
	m     = sum(theta * post)
	s     = sqrt(sum(theta^2 * post) - m^2)
	c(ybar, m, s)
}

summ2 = t(sapply(c(110, 125, 140), norm.t.compute))
summ2
#      [,1]     [,2]      [,3]
# [1,]  110 78.58024 1.4010641
# [2,]  125 78.96803 1.0361267
# [3,]  140 79.19416 0.8179035

dimnames(summ2)[[2]] = c("ybar", "mu1 t", "tau1 t")
summ2
cbind(summ1, summ2) # compare the posteriors for each of the prior distributions

theta = seq(60, 180, length = 500)
normpost = dnorm(theta, mu1[3], tau1)
normpost = normpost/sum(normpost)
plot(theta, normpost, type = "l", lwd = 3, ylab = "Post Density")
like = dnorm(theta, mean = 140, sd = sigma/sqrt(n))
prior = dt((theta - mu)/tscale, 2)
tpost = prior * like / sum(prior * like)
lines(theta, tpost)
legend("topright", legend = c("t prior", "normal prior"), lwd = c(1, 3))

# 3.5 mixtures of conjugate priors- example is the coin is biased
probs = c(.5, .5)
beta.par1 = c(6, 14)
beta.par2 = c(14, 6)

betapar = rbind(beta.par1, beta.par2)
data = c(7, 3)
post = binomial.beta.mix(probs, betapar, data)
post

# the prior contains 2 humps on the data
curve(post$probs[1]*dbeta(x, 13, 17) + post$probs[2]*dbeta(x, 21, 9), 
	from = 0, to = 1, lwd = 3, xlab = "P", ylab = "DENSITY")
curve(.5*dbeta(x, 6, 12) + .5*dbeta(x, 12, 6), 0, 1, add = TRUE) #
legend("topleft", legend = c("Prior", "posterior"), lwd = c(1,3))

# test if coin is fair given data
pbinom(5, 20, 0.5)

n = 20; y = 5; a = 10; p = 0.5
m1 = dbinom(y, n , p)* dbeta(p, a, a)/dbeta(p, a + y, a + n - y)
lambda = dbinom(y, n , p )/(dbinom(y, n, p ) + m1)
lambda
pbetat(p, .5, c(a, a), c(y, n - y))

prob.fair = function(log.a){
	a = exp(log.a);
	m2 = dbinom(y, n , p)* dbeta(p, a, a)/dbeta(p, a + y, a + n - y);
	dbinom(y, n, p)/(dbinom(y, n, p) + m2)
}

n = 20; y = 5; p = 0.5; 
curve(prob.fair(x), from = -4, to = 5, xlab = "log a", ylab = "Prob(coin is fair)", lwd = 2)
# this cannot be interpreted that the pvalue is the probability of fairness

# Chapter 4
# normal data with both parameters unknown, mean and variance
data(marathontimes)
head(marathontimes)

#set up the grid
d = mycontour(normchi2post, c(220, 330, 500, 9000), marathontimes$time, xlab = "mean", ylab = "variance" )
S = sum((marathontimes$time - mean(marathontimes$time))^2)
n = length(marathontimes$time)
sigma2 = S/rchisq(1000, n - 1)
mu = rnorm(1000, mean = mean(marathontimes$time), sd = sqrt(sigma2)/sqrt(n))

points(mu, sigma2)
quantile(mu, c(0.025, 0.975)) # means
quantile(sqrt(sigma2), c(0.025, 0.975)) # SD

# Dirichlet distribution for presidentail elections
alpha = c(728, 548, 138)
theta = rdirichlet(1000, alpha)
hist(theta[, 1] - theta[, 2], main = "") # compare bush vs. dukakis

# compare obama vs. mccain
data(election.2008)
head(election.2008)
attach(election.2008)

prob.Obama = function(j){
	p = rdirichlet(5000, 500*c(M.pct[j], O.pct[j], 100 - M.pct[j] - O.pct[j])/100 + 1)
	mean(p[,2]>p[,1])
}
Obama.win.probs = sapply(1:51, prob.Obama)

#now using a simulation of a biased coin
sim.election = function(){
	winner = rbinom(51, 1, Obama.win.probs)
	sum(EV*winner)
}
sim.EV = replicate(1000, sim.election())

hist(sim.EV, min(sim.EV):max(sim.EV), col = "blue")
abline(v = 365, lwd = 3)
text(375, 30, "Actual \n Obama \n total")


x = c(-0.86, -0.3, -0.05, 0.73)
n = c(5, 5, 5, 5)
y = c(0, 1, 3, 5)

data = cbind(x, n , y)
response = cbind(y, n - y)
results = glm(response ~ x, family = binomial)
summary(results)

# we want to put priors on this
beta.select(list(p = .5, x = .2), list(p = .9, x = .5))






