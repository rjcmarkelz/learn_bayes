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



