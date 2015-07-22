#prob stats in R

# function to compute t-stat

X <- rnorm(10, mean = 50, sd = 10)
Y <- rnorm(10, mean = 50, sd = 10)

sp <- sqrt(((m -1)*sd(X)^2 + (n -1)*sd(Y)^2)/ (m + n -2 ))
t.stat <- (mean(X) - mean(Y))/(sp*sqrt(1/m+1/n))

tstatistic <- function(x,y){
	m <- length(x)
	n <- length(y)
	sp <- sqrt(((m-1)*sd(x)^2 + (n -1)*sd(y)^2)/ (m + n -2 ))
    t.stat <- (mean(x) - mean(y))/(sp*sqrt(1/m+1/n))
    return(t.stat)
}

data.x <- c(1,4,3,6,5)
data.y <- c(5,4,7,6,10)

tstatistic(data.x, data.y)

# function for monty carlo simulation

abs(t) > 
alpha <- 0.1
m <- 10
n <- 10

N <- 10000
n.reject <- 0
for (i in 1:N){
	x = rnorm(m, mean = 0, sd = 1)
	y = rnorm(n, mean = 0, sd = 1)
	t.stat = tstatistic(x,y)
	if (abs(t.stat) > qt(1-alpha/2, n+m-2))
		n.reject = n.reject + 1
} 

true.sig.level = n.reject/N
true.sig.level

m <- 10
n <- 10

my.simulation <- function()
   tstatistic(rnorm(m, mean = 10, sd = 2), rexp(n, rate = 1/10))
tstat.vector <- replicate(10000, my.simulation())
plot(density(tstat.vector), xlim = c(-5,8), ylim = c(0,.4), lwd = 3)
curve(dt(x, df = 18), add = TRUE)

# intro to bayes
#proportion of college students that sleep at least 8 hours
p <- seq(0.05, 0.95, by = 0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
plot(p, prior, type = "h", ylab = "Prior prob")






















