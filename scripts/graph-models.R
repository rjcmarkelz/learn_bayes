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
