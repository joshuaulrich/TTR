\name{DVI}
\alias{DVI}
\title{DV Intermediate Oscillator}
\usage{
  DVI(price, n = 252, wts = c(0.8, 0.2), smooth = 3,
    magnitude = c(5, 100, 5), stretch = c(10, 100, 2))
}
\arguments{
  \item{price}{Price series that is coercible to xts or
  matrix.}

  \item{n}{Number of periods for the percent rank.}

  \item{wts}{The weight given to the smoothed returns
  (magnitude) component and the up/down days (stretch)
  component, respectively.}

  \item{smooth}{The number of periods to smooth price.}

  \item{magnitude}{A set of 3 periods used to smooth
  magnitude.}

  \item{stretch}{A set of 3 periods used to smooth
  stretch.}
}
\value{
  A object of the same class as \code{price} or a vector
  (if \code{try.xts} fails) containing the DVI values.
}
\description{
  The DV Intermediate oscillator (DVI) is a very smooth
  momentum oscillator that can also be used as a trend
  indicator.  Created by David Varadi.
}
\details{
  The DVI combines smoothed returns over different time
  windows and the relative number of up versus down days
  (stretch) over different time windows.
}
\examples{
data(ttrc)
dvi <- DVI(ttrc[,"Close"])
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:\cr
  \url{http://cssanalytics.wordpress.com/2009/12/13/what-is-the-dvi/}\cr
  \url{http://marketsci.wordpress.com/2010/07/27/css-analytics\%E2\%80\%99-dvi-indicator-revealed/}\cr
}
\keyword{ts}

