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
  \item{HL}{ High-Low price series to use. }
  \item{n}{ Number of periods for moving average. }
}
\details{
  Donchian Channels consist of two (sometimes three) lines:

  The top line is the highest high of the past \code{n} periods.
  The bottom line is the lowest low of the past \code{n} periods.
  The middle line is the average of the top and bottom lines.
}
\value{
  A matrix containing the columns:
  \item{ high }{ The lower Bollinger Band. }
  \item{ mid }{ The middle Moving Average (see notes). }
  \item{ low }{ The upper Bollinger Band. }
}
\author{ Josh Ulrich }
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
