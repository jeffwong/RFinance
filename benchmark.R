require(OptimalTrading)

numTests = 100
increment = 4
numAssets = 2
initFunds = 10000

riskTol = initFunds

cpu.time = sapply(seq(from=increment, by=increment, length.out=numTests), function(i) {
  prices = abs(rnorm(i))
  risks = abs(rnorm(i))
  elapsed = system.time(opt <- OptimalTrades(prices, risks=risks, riskTol=riskTol, initFunds=initFunds, numAssets=numAssets))[3]
  if(opt$status != 0) {
    stop(paste("Did not converge when prices had size", i, sep=" "))
  }
  elapsed
})

plot(increment * 1:numTests, cpu.time, main="Optimal Trades Benchmark", xlab="Number of Trades",
  ylab="CPU Time (seconds)")

prices = abs(rnorm(60*6.5*numAssets))
risks = abs(rnorm(60*6.5*numAssets))
system.time(opt <- OptimalTrades(prices, risks=risks, riskTol=10^10, initFunds=initFunds, numAssets=numAssets))
opt
