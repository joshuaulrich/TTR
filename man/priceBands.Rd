\name{PBands}
\alias{PBands}
\alias{priceBands}
\title{ construct (optionally further smoothed and centered ) volatility bands around prices }
\description{
John Bollinger's famous adaptive volatility bands most often use the typical price of an HLC series, 
or may be calculated on a univariate price series (see \code{\link{BBands}}.

This function applies a second moving average denoted by \code{fastn} to filter out 
higher-frequency noise, making the bands somewhat more stable to temporary fluctuations and spikes. 

If \code{centered} is TRUE, the function also further smoothes and centers the bands around a centerline adjusted to remove this higher frequency noise.  
If \code{lavg} is also true, the smoothing applied for the middle band (but not the volatility bands) is doubled to further smooth the price-response function. 

If you have multiple different price series in \code{prices}, and want to use this function, call this functions using \code{lapply(prices,PBands)}
}
\usage{
PBands(prices, n = 20, maType = "SMA", sd = 2, ..., fastn = 2, centered = FALSE, lavg = FALSE)
}

\arguments{
  \item{prices}{ A univariate series of prices}
  \item{n}{ Number of periods to average over }
  \item{maType}{ A function or a string naming the function to be called }
  \item{sd}{ The number of standard deviations to use }
  \item{\dots}{ any other pass-thru parameters, usually for function named by \code{maType} }
  \item{fastn}{ number of periods to use for smoothing higher-frequency 'noise' }
  \item{centered}{ whether to center the bands around a series adjusted for high frequency noise, default FALSE}
  \item{lavg}{ whether to use a longer (n*2) smoothing period for centering, default FALSE }
}

\author{ Brian G. Peterson }

\seealso{
    \code{\link{BBands}}
}
\examples{
    data(ttrc)
    bbands.close <- BBands( ttrc[,"Close"] )
}
\keyword{ ts }