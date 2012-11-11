#Given a matrix of returns for different assets
#Find the portfolio with minimum variance
#That achieves a mean return = target
weights.optimal = function(returns, target) {
  mean.return = mean(returns)
  cov.return = cov(returns)
  cov.inv = solve(cov.return)
  one = rep(1, ncol(cov.return))
  A = as.numeric(t(mean.return) %*% cov.inv %*% one)
  B = as.numeric(t(mean.return) %*% cov.inv %*% mean.return)
  C = as.numeric(t(one) %*% solve(cov.return) %*% one)
  D = B*C - (A^2)
  weights.optimal = (B * cov.inv %*% one - A * cov.inv %*% mean.return + target * (C * cov.inv %*% mean.return - A * cov.inv %*% one)) / (D)
  return (weights.optimal)
}

#Use bootstrapping to find the minimum variance portfolio
michaudResampledWeight = function(returns, target, num.bootstraps=500) {
  weights = apply(sapply(1:num.bootstraps, function(i) {
    bootstrap.sample.rows = sample(1:nrow(returns), nrow(returns), replace=T)
    bootstrap.sample = returns[bootstrap.sample.rows,]
    weights.optimal(bootstrap.sample, target)
  }), 1, sum)
  weights = weights / num.bootstraps
  return (weights)
}

