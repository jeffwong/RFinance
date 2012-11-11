OptimalTrades = function(prices, risks = NULL, riskTol=1000, initFunds=1000, numAssets) {
  n = length(prices)
  if(n %% numAssets != 0) {
    stop("Market data is missing, length of prices vector should be a 
      multiple of numAssets")
  }

  #Limit trading to initFunds
  U.funds = sapply(1:n, function(i) {
    price = prices[i]
    c(rep(0, (i-1)), rep(-price, (n-(i-1))))
  })
  c.funds = rep(-initFunds, n)

  #No short selling
  U.quantity = .runningSumByAsset(n, numAssets)
  c.quantity = rep(0, n)

  U = rbind(U.funds, U.quantity)
  constraints = c(c.funds, c.quantity)

  #OPTIONAL
  #Limit total exposure to risk at any given time
  if(!is.null(risks)) {
    if(all(risks > 0)) {
      status = 0
      print("D matrix is positive definite.  A unique solution should be found")
    } else if(all(risks >= 0)) {
      status = 1
      print("D matrix is positive semi-definite.  A solution should be found,
        but is not necessarily unique")
    } else {
      status = -1
      print("D matrix is neither positive definite nor semi-definite.
        Solution not guaranteed")
    }

    U.risk = -1 * .runningSumByAsset(n, numAssets) %*% diag(risks)
    c.risk = rep(-riskTol, n)
    U = rbind(U, U.risk)
    constraints = c(constraints, c.risk)

    opt = solve.QP(diag(risks), -prices, t(U), constraints)
    
    decisions = opt$solution
    profits = -((prices %*% decisions)[1,1])
  } else {
    #METHOD USING CONSTROPTIM (DEPRECATED)
    #if(!all(U %*% initDecisions - constraints >= 0)) stop("Initial decision vector is outside feasible region")
    #opt = constrOptim(initDecisions, .OptimalTrades.Utility, .OptimalTrades.Utility.grad, ui=U, ci=constraints, prices=prices)
    #
    #status = opt$convergence
    #decisions = opt$par
    #profits = -opt$value
    #END DEPRECATED
  
    #METHOD USING LPSOLVE
    lprec = make.lp(0, n)
    set.objfn(lprec, prices)
    for(i in 1:nrow(U)) {
      add.constraint(lprec, U[i,], ">=", constraints[i])
    }
    set.bounds(lprec, lower=rep(-99999999, n))

    status = solve(lprec)
    decisions = get.variables(lprec)
    profits = -get.objective(lprec)
  }

  return ( list(status=status, profits=profits, decisions=decisions) )
}

#DEPRECATED
#Minimize this function
.OptimalTrades.Utility = function(decisions, prices) {
  return ( (decisions %*% prices)[1,1] )
}

#DEPRECATED
#Derivative of Utility function with respect to decisions
.OptimalTrades.Utility.grad = function(decisions, prices) {
  return (prices)
}

OptimalTrades.Decisions.plot = function(decisions, numAssets, symbols) {
  T = length(decisions) / numAssets
  decisions.clean = .cleanOptimization(decisions, 0.001)
  decisions.matrix = matrix(decisions.clean, T, numAssets, byrow=T)
  indices1 = which(decisions.matrix > -1 & decisions.matrix < 0)
  indices2 = which(decisions.matrix > 0 & decisions.matrix < 1)
  indices3 = which(decisions.matrix <= -1)
  indices4 = which(decisions.matrix >= 1)
  decisions.matrix[indices1] = -1
  decisions.matrix[indices2] = 1
  decisions.matrix[indices3] = -2
  decisions.matrix[indices4] = 2
  decisions.matrix[which(decisions.matrix == 0)] = 0
  
  #DEPRECATED
  #Using R's heatmap function, cannot plot color legend 
  #if(!missing(symbols)) {
  #  colnames(decisions.matrix) = symbols
  #}
  #heatmap(decisions.matrix, Rowv=NA, scale="none", revC=T, col=topo.colors(5),
  #  ylab="Time", xlab="Assets", main="Optimal Trading Strategy")
  #END DEPRECATED

  rownames(decisions.matrix) = 1:T
  if(!missing(symbols)) {
    colnames(decisions.matrix) = symbols
  } else {
    colnames(decisions.matrix) = 1:numAssets
  }
  decisions.matrix.rev = apply(decisions.matrix, 2, rev)
  heatmap_2(decisions.matrix.rev, Rowv=NA, scale="none", col=topo.colors(5),
    legend=1, main="Optimal Trading Strategy")

  return (list(decisions.matrix=decisions.matrix, decisions.clean=decisions.clean))
}

OptimalTrades.Prices.plot = function(prices, numAssets, symbols) {
  T = length(prices) / numAssets
  prices.matrix = matrix(prices, T, numAssets, byrow=T)
  rownames(prices.matrix) = 1:T
  if(!missing(symbols)) {
    colnames(prices.matrix) = symbols
  } else {
    colnames(prices.matrix) = 1:numAssets
  }
  prices.matrix.rev = .standardize(apply(prices.matrix, 2, rev))
  heatmap_2(prices.matrix.rev, Rowv=NA, scale="none",
    legend=1, main="Prices")
  return (prices.matrix)
}

OptimalTrades.Decisions.FindAssociations = function(decisions, numAssets, symbols, ...) {
  T = length(decisions) / numAssets

  decisions.buyMatrix = matrix(decisions, T, numAssets, byrow=T)
  if(!missing(symbols)) colnames(decisions.buyMatrix) = symbols
  buy.indices = which(decisions.buyMatrix > 0)
  decisions.buyMatrix[buy.indices] = 1
  decisions.buyMatrix[-buy.indices] = 0
  buy.rules = apriori(decisions.buyMatrix, ...)

  decisions.sellMatrix = matrix(decisions, T, numAssets, byrow=T)
  if(!missing(symbols)) colnames(decisions.sellMatrix) = symbols
  sell.indices = which(decisions.sellMatrix < 0)
  decisions.sellMatrix[sell.indices] = 1
  decisions.sellMatrix[-sell.indices] = 0
  sell.rules = apriori(decisions.sellMatrix, ...)

  if(length(buy.rules) == 0 & length(sell.rules) == 0) {
    warning("No rules found")
  }

  return (list(buy.rules=buy.rules, sell.rules=sell.rules))
}

OptimalTrades.BenchmarkDecisions = function(prices, initFunds=1000, numAssets, myDecisions) {
  opt = OptimalTrades(prices, initFunds, numAssets)
  decisions.factor = rep(0, length(prices))
  decisions.factor[which(opt$decisions < 0)] = -1
  decisions.factor[which(opt$decisions > 0)] = 1
  decisions.factor[which(opt$decisions == 0)] = 0
  myDecisions.factor = rep(0, length(prices))
  myDecisions.factor[which(myDecisions < 0)] = -1
  myDecisions.factor[which(myDecisions > 0)] = 1
  myDecisions.factor[which(myDecisions == 0)] = 0
  table(myDecisions.factor, decisions.factor)
}

.cleanOptimization = function(x, tol) {
  indices = which(abs(x) < tol)
  x[indices] = 0
  x
} 

.runningSumByAsset = function(n, numAssets) {
  runningSum = matrix(0,n,n)
  runningSum[ (row(runningSum) - col(runningSum)) %% numAssets == 0 & 
    row(runningSum) >= col(runningSum) ] = 1
  return (runningSum)
}

.standardize = function(X) {
  apply(X, 2, function(j) {
    (j - mean(j)) / sd(j)
  })
}
