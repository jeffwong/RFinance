#type=option is the same as quantmod's getOptionChain(symbol, Exp=NULL)

getHighFrequencyData = function(Symbols, type="stock") {
  if(type == "stock") {
    market = do.call('rbind',lapply(Symbols, function(symbol) {
      link = paste("http://finance.yahoo.com/q?s=", symbol, "&ql=1", sep="")
      tables = readHTMLTable(link)
      data = tables[1]$table1
      if(is.null(data)) {
        warning(paste("Error in downloading data for", symbol))
        return (NULL)
      }
    
      bid = .parseBidAsk(data[5,2])
      ask = .parseBidAsk(data[6,2])

      data.frame(asset=symbol,
        price=as.numeric(colnames(data)[2]),
        time=data[1,2],
        change=data[2,2],
        prevclose=data[3,2],
        open=data[4,2],
        bid=bid$bidask,
        bidquantity=bid$bidaskquantity,
        ask=ask$bidask,
        askquantity=ask$bidaskquantity)
    }))
  } else {
    market = lapply(Symbols, function(symbol) {
      link = paste("http://finance.yahoo.com/q/op?s=", symbol, "+Options", 
        sep = "")
      html = readLines(link)
      optiondates = html[42]
      indices = gregexpr("m=", optiondates)
      base = paste('http://finance.yahoo.com/q/op?s=', symbol, '&', sep='')
      optiondates.expiry = sapply(indices[[1]], function(index) {
        expiry = substr(optiondates, index+2, index+8)
      })
      optiondates.links = sapply(optiondates.expiry, function(expiry) {
        paste(base, 'm=', expiry, sep='')
      })
      chains = lapply(optiondates.links, function(backmonth.link) {
        tryCatch(chain <- .getOptionsTable(backmonth.link, symbol))
      })
      options = do.call('rbind', chains)
      return (options)
    })
  }
  return (market)
}

.parseBidAsk = function(bidask) {
  bid.index = regexpr("x", bidask)
  bid = as.numeric(substr(bidask,1, bid.index-1))
  bidquantity = as.numeric(substr(bidask, bid.index+1, nchar(as.character(bidask))))
  return ( list(bidask=bid, bidaskquantity=bidquantity) )    
}

.getOptionsTable = function(link, symbol) {
  tables = readHTMLTable(link)
  if(length(tables) < 15) {
    warning(paste("package XML could not parse HTML tables at ", link))
    return (NULL)
  } else {
    calls = tables[11][[1]]
    puts = tables[15][[1]]
    if(is.null(calls) | is.null(puts)) {
      warning(paste("package XML could not parse HTML tables at ", link))
      return (NULL)
    }
    options = rbind(calls, puts)
    colnames(options)[8] = "OpenInt"
    underlying = rep(symbol, nrow(options))
    type = c(rep("call", nrow(calls)), rep("put", nrow(puts)))
    options = cbind(underlying, cbind(type, options))
  
    return (options)
  }
}
