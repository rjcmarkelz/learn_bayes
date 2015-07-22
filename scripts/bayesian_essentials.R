# normal model
library(bayess)
data(normaldata)

shift <- normaldata[,2]
shift

hist(shift, nclass = 10, col = "steelblue", prob = TRUE, main = "")

n = length(shift)
mmu = sum(shift)/(n + 1)
mmu

vmu = 0.75^2/(n + 1); vmu

# students t distribution
mtmu = sum(shift)/(n + 1); mtmu
stmu = (2+(n-1)*var(shift))/((n+2)*(n+1)); stmu

install.packages("mnormt")
library(mnormt)
curve(dmt(x, mean = mmu, S = stmu, df = n + 2), col = "chocolate2", lwd = 2, xlab = "x", ylab = "", xlim=c(-.5,.5))
curve(dnorm(x, mean = mmu, sd = sqrt(vmu)), col = "steelblue2", lwd = 2, add = TRUE, lty = 2)

