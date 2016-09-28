# from book Prob graph models R
prior <- c(working = 0.99, broken = 0.01)
prior
likelihood <- rbind(working = c(good = 0.99, bad = 0.01),
    broken = c(good = 0.6, bad = 0.4))
likelihood
data <- c("bad", "bad", "bad", "bad")
data

# bayes update function
bayes <- function(prior, likelihood, data){
    #create matrix to store successive computation of posteriors
    posterior <- matrix(0, nrow = length(data), ncol = length(prior))
    dimnames(posterior) <- list(data, names(prior))
    initial_prior <- prior 

    # for each data calculate the posterior given current prior
    for(i in 1:length(data)){
        posterior[i, ] <- prior*likelihood[, data[i]]/sum(prior*likelihood[, data[i]])

        prior <- posterior[i, ] # new prior is current posterior
    }

    return(rbind(initial_prior, posterior))
}

matplot(bayes(prior, likelihood, data), t = 'b', lty = 1, pch = 20, col = c(3,2))

# play with prior
prior <- c(working = 0.5, broken = 0.5)
matplot(bayes(prior, likelihood, data), t = 'b', lty = 1, pch = 20, col = c(3,2))

# play with data
prior <- c(working = 0.99, broken = 0.01)
data <- c("good","bad","good","good","good","good","good","good","bad","good")
matplot(bayes(prior, likelihood, data), t = 'b', lty = 1, pch = 20, col = c(3,2))


source("http://bioconductor.org/biocLite.R")
biocLite()
install.packages("gRain")
library("gRbase")
library("gRain")
library("Rgraphviz")

?ug
graph <- ug(~A:B:E + C:E:D)
class(graph)
plot(graph)

dagtest <- dag(~A + B:A + C:B + D:B + E:C:D)
dagtest
plot(dagtest)

machine_val <- c("working", "broken")
light_val <- c("good", "bad")

machine_prob <- c(99, 1)
light_bulb_prob <- c(99, 1, 60, 40)

M <- cptable(~machine, values = machine_prob, levels = machine_val)
L <- cptable(~light_bulb|machine, values = light_bulb_prob, levels = light_val)

plist <- compileCPT(list(M,L))
plist
# inspect
plist$machine
plist$light_bulb

# get posterior
net <- grain(plist)
net2 <- setEvidence(net, evidence = list(light_bulb = "bad"))

querygrain(net2, nodes = c("machine"))
# same as above coded by hand!


# variable elimination for following graph model
dag2 <- dag(~B:A + B:C + B:D)
dag2
plot(dag2)

#P(A,B,C,D)
A <- matrix(c(0.8, 0.2), 2, 1)
t(A)
t(B)
B <- matrix(c(0.6, 0.4, 0.3, 0.7), 2, 2)
C <- matrix(c(0.5, 0.5, 0.8, 0.8), 2, 2)
D <- matrix(c(0.3, 0.7, 0.4, 0.6), 2, 2)
# conditional distributions
B

# marginalize out A to obtain P(B,C,D)
Bs <- t(A) %*% t(B)
Bs
# marginalize out B to obtain P(C,D)
Cs <- Bs %*% t(C)

# marginalize out C to leave D
Ds <- Cs %*% t(D)
Ds #typo in book

# junction tree example using binary variables
dag3 <- dag(~C:F + E:F + A:C + D:E + B:A + B:D)
dag3
plot(dag3)

val <- c("true", "false")
F <- cptable(~F, values = c(10, 90), levels = val)
C <- cptable(~C|F, values = c(10, 90, 20, 80), levels = val)
E <- cptable(~E|F, values = c(50, 50, 30, 70), levels = val)
A <- cptable(~A|C, values = c(50, 50, 70, 30), levels = val)
D <- cptable(~D|E, values = c(60, 40, 70, 30), levels = val)
B <- cptable(~B|A:D, values = c(60, 40, 70, 30, 20, 80, 10, 90), levels = val)
B

plist <- compileCPT(list(F,E,C,A,D,B))
plist

# check distribution
print(plist$F)
print(plist$B)

# create the graph
jtree <- grain(plist)
jtree
plot(jtree)

querygrain(jtree, nodes = c("F"), type = "marginal")
querygrain(jtree, nodes = c("C"), type = "marginal")
querygrain(jtree, nodes = c("B"), type = "marginal")

# complex distribution
querygrain(jtree, nodes = c("A","B"), type = "marginal")

# now we observe a variable 
jtree2 <- setEvidence(jtree, evidence = list(F = "true"))
querygrain(jtree, nodes = c("F"), type = "marginal")
querygrain(jtree2, nodes = c("F"), type = "marginal")

# any joint marginal once we observe one F
#knowing what F is changes the marginal on A
querygrain(jtree, nodes = c("A"), type = "marginal")
querygrain(jtree2, nodes = c("A"), type = "marginal")

jtree3 <- setEvidence(jtree, evidence = list(F = "true", A = "false"))

#knowing F and A changes marginal on C
querygrain(jtree, nodes = c("C"), type = "marginal")
querygrain(jtree2, nodes = c("C"), type = "marginal")
querygrain(jtree3, nodes = c("C"), type = "marginal")

# Chapter 3- Learning parameters from the data
library(plyr)
iris <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",col.names=c("sepal_length","sepal_width","petal_length","petal_width","class"))
iris

iris_dist <- daply(iris, .(class), nrow) / nrow(iris)
iris_dist

# descretize the dataset
head(iris)
q <- quantile(iris$sepal_width,seq(0,1,.33))
iris$dsw[iris$sepal_width < q['33%']] <- "small"
iris$dsw[iris$sepal_width >= q['33%'] & iris$sepal_width < q['66%']] <- "medium"
iris$dsw[iris$sepal_width > q['66%']] <- "large"


p1 <- daply(iris,.(dsw,class), function(n) nrow(n))
p1 <- p1 / colSums(p1)

# bayesian learning example
# prob is vector of probability for each Theta
# nh and nt are number heads and number tails
# Theta is the vector of possible values of for Theta
posterior <- function(prob, nh, nt, Theta = c(0.2, 0.5, 0.8)){
     x = numeric(3)
     for(i in 1:3){
        x[i] = prob[i]* (Theta[i]^nh)*((1 - Theta[i])^nt)
     }
     norm = sum(x)
     return(x/norm)
}

posterior(c(0.2, 0.75, 0.05), 2, 8)
posterior(c(0.2,0.75,0.05),50,50)
#outputs a distribution summing to 1! Machine limitation in error rate
# therefore do LOG values

posterior2 <- function(prob, nh, nt, Theta = c(0.2, 0.5, 0.8)){
     x = numeric(3)
     for(i in 1:3){
        x[i] = exp(log(prob[i]) + nh*log((Theta[i])) + nt*log(1 - Theta[i]))
     }
     norm = sum(x)
     return(x/norm)
}
posterior2(c(0.2,0.75,0.05),50,50) #does not appear to change output

#prior distribution is uniform
posterior(c(1/3,1/3,1/3),2,8,c(0.2,0.5,0.8))
posterior(c(1/3,1/3,1/3),8,2,c(0.2,0.5,0.8))
posterior2(c(1/3,1/3,1/3),5,5,c(0.2,0.5,0.8))

# maximum likelihood
library(graph)
library(Rgraphviz)
library(plyr)

data0 <- data.frame(
       x = c("a","a","a","a","b","b","b","b"),
       y = c("t","t","u","u","t","t","u","u"),
       z = c("c","d","c","d","c","d","c","d"))
data0

edges0 <- list(x = list(edges = 2), y = list(edges = 3), z = list())
edges0

g0 <- graphNEL(nodes = names(data0), edgeL = edges0, edgemod = "directed")
plot(g0)

data1 <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data", col.names=c("parents","has_nurs","form","children","housing","finance","social","health","class"))
head(data1)

edges1 <- list(parents = list(), has_nurs = list(), form = list(), children = list(),
                housing = list(), finance = list(), social = list(), health = list(),
                class = list(edges = 1:8))

g1 <- graphNEL(nodes = names(data1), edgeL = edges1, edgemod = "directed")
plot(g1)

# computes frequency of one combination of variable of interest
make_cpt <- function(df, pa){
    prob <- nrow(df)
    parents <- data.frame(df[1,pa])
    names(parents) <- pa

    data.frame(parents, prob)
}

# takes two paramters, the graph and the data
# To find the parents of all the variables it reverses the edge direction
learn <- function(g, data){
    rg <- reverseEdgeDirections(g)
    result <- list()

    #now look at each variable independently
    for(var in rg@nodes){
        pa <- unlist(adj(rg,var)) # find parents of each node
        # if node has parent then conditional probability
        if(length(pa) > 0){
            # for each possible value and its parents, calculate frequency
            X <- ddply(data, c(var,pa), make_cpt, pa)
            Y <- ddply(data, pa, make_cpt, pa)

            # apply bayes formula to transform counts to probabilities
            for(i in 1:nrow(Y)){
                c <- sapply(1:nrow(X), function(j) all(X[j,pa] == Y[i,pa]))
                c <- which(c)
                X$prob[c] <- X$prob[c]/Y$prob[i]
            }
        }
        # node does not have a parent, calculate marginal
        # apply bayes formula to transform marginal counts to probabilities (frequencies)
        else {
            X <- ddply(data, var, function(df) c(prob = nrow(df)))
            X$prob <- X$prob/sum(X$prob)
        }

        result[[length(result) + 1]] <- X
    }

    return(result)
}

# now for some learning
# test on small dataset
learn(g0, data0) 

learn(g1, data1)
# notice that some of the variables have prob = 1!
# this is because there is only 1 observation of that in the dataset!
# this is where hyper parameters come into play to bound prob between 0 and 1


# EM Algorithm
# latent variable models
# can use domain knowledge to write models

# Chapter 4 bayesian models
library(MASS)
Sigma <- matrix(c(10,3,3,2), 2, 2)
x1 <- mvrnorm(100, c(1,2), Sigma)
x2 <- mvrnorm(100, c(-1,-2), Sigma)
plot(x1, col = 2, xlim = c(-5,5), ylim = c(-5, 5))
points(x2, col = 3)
# to classify this data we need a non-linear seperator
# naive bayes classifier
# already implemented in this package
install.packages("e1071")
library(e1071)
data(iris)

?naiveBayes
model <- naiveBayes(Species~., data = iris)
model
# note need to apply laplace smoothing of the data if model is not balanced
p <- predict(model, iris)
hitrate <- sum(p == iris$Species)/nrow(iris)

#randomly sample data from dataset
ni <- sample(1:nrow(iris), 2*nrow(iris)/3)
no <- setdiff(1:nrow(iris), ni)

model <- naiveBayes(Species~., data = iris[ni,])
p <- predict(model, iris[no, ])
plot(p)

# beta-binomial
x <- seq(1, 20)
# parameter varying from 0.1 to 0.5
plot(x, dbinom(x, 20, 0.5), t = 'b', col = 1, ylim = c(0, 0.3))
lines(x, dbinom(x, 20, 0.3), t = 'b', col = 2)
lines(x, dbinom(x, 20, 0.1), t = 'b', col = 3)

# prior distribution
# beta distribution is conjugate to binomial and bernoulli distributions
# this will ensure that the posterior on theta is also Beta-distributed

# gaussian mixture model
# first example of latent variable model
# this takes advantage of the factor that data tends to group into clusters,
# aggregating depending on underlying meaning

# three mvnorm data sets with two dimensions
# equal number of points per cluster
N <- 400 
X <- list(
    mvrnorm(N, c(1,1), matrix(c(1, -0.5, -0.5, 1), 2, 2)/4),
    mvrnorm(N, c(3,3), matrix(c(2, 0.5, 0.5, 1), 2, 2)/4),
    mvrnorm(N, c(5,5), matrix(c(1, -0.5, -0.5, 4), 2, 2)/4))

plot(0, 0 , xlim = c(-1,7), ylim = c(-1,7), type = 'n')
for(i in 1:3)
    points(X[[i]], pch = 18, col = 1 + i)

install.packages("mixtools")
library(mixtools)

x <- do.call(rbind, X) #transform X into a matrix
head(x)
model2 <- mvnormalmixEM(x, verb = TRUE)
model3 <- mvnormalmixEM(x, k = 3, verb = TRUE)

plot(model2, xlim = c(0,50), ylim = c(-4000, -3000))
par(new=TRUE)
plot(model3, xlim = c(0,50), ylim = c(-4000, -3000))
model3$lambda

X <- list(
    mvrnorm(100, c(1,1), matrix(c(1,-0.5,-0.5,1),2,2)/4),
    mvrnorm(200, c(3,3), matrix(c(2,0.5,0.5,1),2,2)/4),
    mvrnorm(300, c(5,5), matrix(c(1,-0.5,-0.5,4),2,2)/4))
x <- do.call(rbind,X)

model3_2 <- mvnormalmixEM(x, k = 3, verb = TRUE)
plot(model3, which = 2) # can ID all three sets
plot(model2, which = 2) # can ID only 2 groups!
plot(model3_2, which = 2) # very similar to model3

# Chapter 5- approximate inference
# need to get around computing the distribution at each step 
# also need to be able to work with models that have many dimensions
 
q <- function(x) dnorm(x, 0, 0.5) # centered on 0
rq <- function(x) rnorm(1, 0, 0.5)

# target distribution is a mixture of Gaussians with two components
p <- function(x) 0.6*dnorm(x, 0, 0.1) + 0.4*dnorm(x, 0.6, 0.2)

# N number of samples
# k coefficient of proposal distribution
# p distribution to estimate
# q proposal distribution
# rq sampler of the proposal distribution
rejection <- function(N, k, p, q, rq){
    accept <- logical(N)
    x <- numeric(N)

    for(i in 1:N){
        z0 <- rq() # draw a point from proposal distribution
        u0 <- runif(1, 0, 1) # draw one point from uniform

        if(u0 < p(z0)/(k*q(z0))) # rejection test
            accept[i] <- TRUE
        else accept[i] <- FALSE

        x[i] <- z0
    }
    data.frame(x = x, accept = accept)
}

set.seed(600)
k <- 3.1 # scaling factor k
x <- rejection(100, k, p, q, rq)
head(x)

# now with 5000 samples
x <- rejection(50000, k, p, q, rq)
hist(x$x[x$accept],freq=F,breaks=200,col='grey')
lines(t,p(t),col=2,lwd=2)

# proposal distribution
hist(x$x,freq=F,breaks=200,col='grey')
lines(t,q(t),col=2,lwd=2)

# therefore running the algorithm for longer will improve the results
# yeah the samples converge to the target distribution!

# importance sampling
# N number of samples
# f function we want to know the expectation of
# p this is the target distribution function
# this is the proposal distrubution function
# rq this is the sampler from the proposal distrubutions


importance <- function(N, f, p, q, rq){
    x <- sapply(1:N, rq) # sample from proposal distrubution

    A <- sum((f(x)*p(x))/q(x)) # numerator
    B <- sum(p(x)/q(x)) # denominator

    return(A/B)
}

set.seed(600)

# mixture of guassian approximated by gaussian
q <- function(x) dnorm(x, 0, 0.5) # centered on 0
rq <- function(x) rnorm(1, 0, 0.5)
p <- function(x) 0.6*dnorm(x, 0, 0.1) + 0.4*dnorm(x, 0.6, 0.2)
print(importance(1000,identity,p,q,rq))
print(importance(10000,identity,p,q,rq))
print(importance(50000,identity,p,q,rq))
# with importance sampling we need fewer samples than rejection sampling
# to approximate 

# students t-distribution
p <- function(x) dt(x, 2)
q <- function(x) dnorm(x, 0, 1.5)
rq <- function(x) rnorm(x, 0, 1.5)

# Gamma distribution
p <- function(x) dgamma(x, 2)
q <- function(x) dexp(x, 0.5)
rq <- function(x) rexp(1, 0.5)


