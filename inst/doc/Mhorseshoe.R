## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(Mhorseshoe)
library(ggplot2)
library(horseshoe)

## -----------------------------------------------------------------------------
# making simulation data.
set.seed(123)
N <- 300
p <- 500
p_star <- 50
true_beta <- c(rep(1, p_star), rep(0, p-p_star))

# design matrix X.
X <- matrix(1, nrow = N, ncol = p)
for (i in 1:p) {
  X[, i] <- stats::rnorm(N, mean = 0, sd = 1)
}
  
# response variable y.
y <- vector(mode = "numeric", length = N)
e <- rnorm(N, mean = 0, sd = 2)
for (i in 1:p_star) {
  y <- y + true_beta[i] * X[, i]
}
y <- y + e

## ----fig.width = 6, fig.height= 4---------------------------------------------
# horseshoe in horseshoe package.
horseshoe_result <- horseshoe::horseshoe(y, X, method.tau = "halfCauchy",
                                         method.sigma = "Jeffreys",
                                         burn = 0, nmc = 500)

# exact_horseshoe.
exact_horseshoe_result <- exact_horseshoe(X, y, burn = 0, iter = 500)

df <- data.frame(index = 1:100,
                 horseshoe_BetaHat = horseshoe_result$BetaHat[1:100],
                 horseshoe_LeftCI = horseshoe_result$LeftCI[1:100],
                 horseshoe_RightCI = horseshoe_result$RightCI[1:100],
                 exhorseshoe_BetaHat = exact_horseshoe_result$BetaHat[1:100],
                 exhorseshoe_LeftCI = exact_horseshoe_result$LeftCI[1:100],
                 exhorseshoe_RightCI = exact_horseshoe_result$RightCI[1:100],
                 true_beta = true_beta[1:100])

# Estimation results of the horseshoe.
ggplot(data = df, aes(x = index, y = true_beta)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = horseshoe_BetaHat), size = 2, col = "red") +
  geom_errorbar(aes(ymin = horseshoe_LeftCI,
                    ymax = horseshoe_RightCI), width = .1, col = "red") +
  labs(title = "95% Credible intervals of the horseshoe function", y = "beta")

# Estimation results of the exact_horseshoe.
ggplot(data = df, aes(x = index, y = true_beta)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = exhorseshoe_BetaHat), 
             size = 2, col = "red") +
  geom_errorbar(aes(ymin = exhorseshoe_LeftCI, 
                    ymax = exhorseshoe_RightCI), width = .1, col = "red") +
  labs(title = "95% Credible intervals of the exact_horseshoe function",
       y = "beta")

## ----fig.width = 6, fig.height= 4---------------------------------------------
# approx_horseshoe with fixed default threshold.
approx_horseshoe_result <- approx_horseshoe(X, y, burn = 0, iter = 500, 
                                            auto.threshold = FALSE)

# modified approx_horseshoe with adaptive probability algorithm.
mapprox_horseshoe_result <- approx_horseshoe(X, y, burn = 0, iter = 500)

df2 <- data.frame(index = 1:100,
                  approx_BetaHat = approx_horseshoe_result$BetaHat[1:100],
                  approx_LeftCI = approx_horseshoe_result$LeftCI[1:100],
                  approx_RightCI = approx_horseshoe_result$RightCI[1:100],
                  mapprox_BetaHat = mapprox_horseshoe_result$BetaHat[1:100],
                  mapprox_LeftCI = mapprox_horseshoe_result$LeftCI[1:100],
                  mapprox_RightCI = mapprox_horseshoe_result$RightCI[1:100],
                  true_beta = true_beta[1:100])

# Estimation results of the approx_horseshoe.
ggplot(data = df2, aes(x = index, y = true_beta)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = approx_BetaHat), size = 2, col = "red") +
  geom_errorbar(aes(ymin = approx_LeftCI, 
                    ymax = approx_RightCI), width = .1, col = "red") +
  labs(title = "95% Credible intervals of the approx_horseshoe", y = "beta")

# Estimation results of the mapprox_horseshoe.
ggplot(data = df2, aes(x = index, y = true_beta)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = mapprox_BetaHat), 
             size = 2, col = "red") +
  geom_errorbar(aes(ymin = mapprox_LeftCI, 
                    ymax = mapprox_RightCI), 
                width = .1, col = "red") +
  labs(title = "95% Credible intervals of the modified_approx_horseshoe", 
       y = "beta")

## ----fig.width = 6, fig.height= 4---------------------------------------------
exact_activeset <- rep(p, 500)
approx_activeset <- apply(approx_horseshoe_result$ActiveSet, MARGIN = 1, sum)
mapprox_activeset <- apply(mapprox_horseshoe_result$ActiveSet, MARGIN = 1, sum)

# active set plot
ggplot(data = data.frame(X = 1:500,
                         exact_activeset = exact_activeset,
                         approx_activeset = approx_activeset,
                         mapprox_activeset = mapprox_activeset)) +
  geom_line(mapping = aes(x = X, y = exact_activeset,
                          color = "exact")) +
  geom_line(mapping = aes(x = X, y = approx_activeset,
                          color = "approx"),
            alpha = 0.5) +
  geom_line(mapping = aes(x = X, y = mapprox_activeset,
                          color = "modified_approx"),
            alpha = 0.5) +
  scale_color_manual(name = "algorithm",
                     values = c("black", "red", "blue"), 
                     breaks = c("exact", "approx", "modified_approx"), 
                     labels = c("exact", "approx", "modified_approx"))

