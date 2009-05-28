'adjRatios' <-
function(splits, dividends, close) {

  if( !missing(dividends) &&
       missing(close) )
    stop('\'close\' must be specified to adjust dividends')
       
  # Really need a better error message if as.xts fails... seriously
  if(missing(close) || all(is.na(close)) || NROW(close)==0) {
    close <- NA
  } else {
    close <- try.xts(close,
      error=stop('\'as.xts(close)\' failed'))
  }
  if(missing(splits) || all(is.na(splits)) || NROW(splits)==0) {
    splits <- NA
  } else {
    splits <- try.xts(splits,
      error=stop('\'as.xts(splits)\' failed'))
  }
  if(missing(dividends) || all(is.na(dividends)) || NROW(dividends)==0) {
    dividends <- NA
  } else {
    dividends <- try.xts(dividends,
      error=stop('\'as.xts(dividends)\' failed'))
  }

  obj <- merge.xts(close,splits,dividends)
  adj <- .Call('adjRatios',obj[,2],obj[,3],obj[,1])
  adj <- xts(cbind(adj[[1]],adj[[2]]),index(obj))
  colnames(adj) <- c('Split','Div')
  
  return(adj)

}
