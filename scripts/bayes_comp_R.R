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









