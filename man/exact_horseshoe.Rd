% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exact_algorithm.R
\name{exact_horseshoe}
\alias{exact_horseshoe}
\title{Run exact MCMC algorithm for horseshoe prior}
\usage{
exact_horseshoe(
  y,
  X,
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
\item{y}{Response vector, \eqn{y \in \mathbb{R}^{N}}.}

\item{X}{Design matrix, \eqn{X \in \mathbb{R}^{N \times p}}.}

\item{burn}{Number of burn-in samples. The default is 1000.}

\item{iter}{Number of samples to be drawn from the posterior. The default is
5000.}

\item{a}{A tuning parameter of the rejection sampler, where the default
value is \eqn{a = 1/5}.}

\item{b}{A tuning parameter of the rejection sampler, where the default
value is \eqn{b = 10}.}

\item{s}{\eqn{s^{2}} is the variance of tau's MH proposal distribution.
0.8 is a good default. If set to 0, the algorithm proceeds by fixing the
global shrinkage parameter \eqn{\tau} to the initial setting value.}

\item{tau}{Initial value of the global shrinkage parameter \eqn{\tau} when
starting the algorithm. The default is 1.}

\item{sigma2}{Initial value of error variance \eqn{\sigma^{2}}. The default
is 1.}

\item{w}{A hyperparameter of gamma prior for \eqn{\sigma^{2}}. The default
is 1.}

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
The exact MCMC algorithm for the horseshoe prior introduced in section 2.1
of Johndrow et al. (2020).
}
\details{
The exact MCMC algorithm introduced in Section 2.1 of Johndrow et al. (2020)
is implemented in this function. This algorithm uses a blocked-Gibbs
sampler for \eqn{(\tau, \beta, \sigma^2)}, where the global shrinkage
parameter \eqn{\tau} is updated by an Metropolis-Hastings algorithm. The
local shrinkage parameter \eqn{\lambda_{j},\ j = 1,2,...,p} is updated by
the rejection sampler.
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

# Run exact_horseshoe
result <- exact_horseshoe(y, X, burn = 0, iter = 100)

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
Learning Research, 21, 1-61.
}
