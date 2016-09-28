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

