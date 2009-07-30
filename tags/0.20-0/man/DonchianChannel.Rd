\name{DonchianChannel}
\alias{DonchianChannel}
\alias{Donchian}
\title{ Donchian Channel }
\description{
  Donchian Channels were created by Richard Donchian and were used to
  generate buy and sell signals for the Turtle Trading system.
}
\usage{
  DonchianChannel(HL, n=10)
}
\arguments{
  \item{HL}{ Object that is coercible to xts or matrix and contains High-Low prices. }
  \item{n}{ Number of periods for moving average. }
}
\details{
  Donchian Channels consist of two (sometimes three) lines:

  The top line is the highest high of the past \code{n} periods.
  The bottom line is the lowest low of the past \code{n} periods.
  The middle line is the average of the top and bottom lines.
}
\value{
  A object of the same class as \code{HL} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ high }{ The highest high series. }
  \item{ mid }{ The average of \code{high} and \code{low}. }
  \item{ low }{ The lowest low series. }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.linnsoft.com/tour/techind/donch.htm}\cr
}
\seealso{
  See \code{\link{BBands}}.
}
\examples{
  data(ttrc)
  dc <- DonchianChannel( ttrc[,c("High","Low")] )
}
\keyword{ ts }
