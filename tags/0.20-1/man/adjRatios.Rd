\name{adjRatios}
\alias{adjRatios}
\alias{adjust}
\title{ Split and dividend adjustment ratios }
\description{
  Create split and dividend adjustment ratio vectors.
}
\usage{
  adjRatios(splits, dividends, close)
}
\arguments{
  \item{splits}{ Split series that is coercible to xts. }
  \item{dividends}{ Dividend series that is coercible to xts. }
  \item{close}{ Close price series that is coercible to xts. }
}
\details{
  If only \code{splits} is provided, the resulting object will only have as
  many observations as \code{splits}.\cr
  \cr If \code{splits} and \code{close} are provided, the resulting object
  will have as many observations as \code{max(NROW(splits),NROW(close))}.\cr
  \cr\code{close} is required if \code{dividends} is provided.
}
\value{
  A xts object containing the columns:
  \item{ Split }{ The split adjustment ratio. }
  \item{ Div }{ The dividend adjustment ratio. }
}
\author{ Joshua Ulrich }
%\note{
%  If only splits are given, the ratio will only be as long as the split vector
%  If splits/close are given, the ratio will be as long as the close vector
%  If you want the dividend ratio, close must be supplied
%}
%\examples{
%  ### Note: you must have a working internet
%  ### connection for these examples to work!
%}
\keyword{ ts }
