% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exact_algorithm.R
\name{exact_horseshoe}
\alias{exact_horseshoe}
\title{Run exact MCMC algorithm for horseshoe prior}
\usage{
exact_horseshoe(
  X,
  y,
  burn = 1000,
  iter = 5000,
  a = 1/5,
  b = 10,
  s = 0.8,
  tau = 1,
  sigma2 = 1,
  w = 1,
  alpha = 0.05
)
}
\arguments{
\item{X}{Design matrix, \eqn{X \in \mathbb{R}^{N \times p}}.}

\item{y}{Response vector, \eqn{y \in \mathbb{R}^{N}}.}

\item{burn}{Number of burn-in samples. Default is 1000.}

\item{iter}{Number of samples to be drawn from the posterior. Default is
5000.}

\item{a}{Parameter of the rejection sampler, and it is recommended to leave
it at the default value, \eqn{a = 1/5}.}

\item{b}{Parameter of the rejection sampler, and it is recommended to leave
it at the default value, \eqn{b = 10}.}

\item{s}{\eqn{s^{2}} is the variance of tau's MH proposal distribution.
0.8 is a good default. If set to 0, the algorithm proceeds by
fixing the global shrinkage parameter \eqn{\tau} to the initial setting
value.}

\item{tau}{Initial value of the global shrinkage parameter \eqn{\tau} when
starting the algorithm. Default is 1.}

\item{sigma2}{error variance \eqn{\sigma^{2}}. Default is 1.}

\item{w}{Parameter of gamma prior for \eqn{\sigma^{2}}. Default is 1.}

\item{alpha}{\eqn{100(1-\alpha)\%} credible interval setting argument.}
}
\value{
\item{BetaHat}{Posterior mean of \eqn{\beta}.}
\item{LeftCI}{Lower bound of \eqn{100(1-\alpha)\%} credible interval for
\eqn{\beta}.}
\item{RightCI}{Upper bound of \eqn{100(1-\alpha)\%} credible interval for
\eqn{\beta}.}
\item{Sigma2Hat}{Posterior mean of \eqn{\sigma^{2}}.}
\item{TauHat}{Posterior mean of \eqn{\tau}.}
\item{LambdaHat}{Posterior mean of \eqn{\lambda_{j},\ j=1,2,...p.}.}
\item{BetaSamples}{Samples from the posterior of \eqn{\beta}.}
\item{LambdaSamples}{Lambda samples through rejection sampling.}
\item{TauSamples}{Tau samples through MH algorithm.}
\item{Sigma2Samples}{Samples from the posterior of the parameter
\eqn{sigma^{2}}.}
}
\description{
The exact MCMC algorithm introduced in Section 2.1 of Johndrow et al. (2020)
was implemented in this function. This algorithm is the horseshoe estimator
that updates the global shrinkage parameter \eqn{\tau} using
Metropolis-Hastings algorithm, and uses blocked-Gibbs sampling for
\eqn{(\tau, \beta, \sigma)}. The local shrinkage parameter
\eqn{\lambda_{j},\ j = 1,2,...,p} is updated by the rejection sampler.
}
\details{
See \code{\link{Mhorseshoe}} or browseVignettes("Mhorseshoe").
}
\examples{
# Making simulation data.
set.seed(123)
N <- 50
p <- 100
true_beta <- c(rep(1, 10), rep(0, 90))

X <- matrix(1, nrow = N, ncol = p) # Design matrix X.
for (i in 1:p) {
  X[, i] <- stats::rnorm(N, mean = 0, sd = 1)
}

y <- vector(mode = "numeric", length = N) # Response variable y.
e <- rnorm(N, mean = 0, sd = 2) # error term e.
for (i in 1:10) {
  y <- y + true_beta[i] * X[, i]
}
y <- y + e

# Run
result <- exact_horseshoe(X, y, burn = 0, iter = 100)

# posterior mean
betahat <- result$BetaHat

# Lower bound of the 95\% credible interval
leftCI <- result$LeftCI

# Upper bound of the 95\% credible interval
RightCI <- result$RightCI

}
\references{
Johndrow, J., Orenstein, P., & Bhattacharya, A. (2020). Scalable
Approximate MCMC Algorithms for the Horseshoe Prior. In Journal of Machine
Learning Research (Vol. 21).
}
