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






