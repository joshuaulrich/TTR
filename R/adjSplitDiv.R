'adjSplitDiv' <-
function(splits, dividends, close) {

  if( !missing(dividends) &&
       missing(close) )
    stop('\'close\' must be specified to adjust dividends')
       
  # Really need a better error message if as.xts fails... seriously
  if(missing(close)) {
    close <- NA
  } else {
    close <- try.xts(close,
      error=stop('\'as.xts(close)\' failed'))
  }
  if(missing(splits)) {
    splits <- NA
  } else {
    splits <- try.xts(splits,
      error=stop('\'as.xts(splits)\' failed'))
  }
  if(missing(dividends)) {
    dividends <- NA
  } else {
    dividends <- try.xts(dividends,
      error=stop('\'as.xts(dividends)\' failed'))
  }

  obj <- merge(splits,dividends,close)
  adj <- .Call('adjSplitDiv',obj[,1],obj[,2],obj[,3])
  adj <- xts(cbind(adj[[1]],adj[[2]]),index(obj))
  colnames(adj) <- c('Split','Div')
  
  return(adj)

}
