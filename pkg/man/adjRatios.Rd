\name{adjRatios}
\alias{adjRatios}
\alias{adjust}
\title{Split and dividend adjustment ratios}
\usage{
  adjRatios(splits, dividends, close)
}
\arguments{
  \item{splits}{Split series that is coercible to xts.}

  \item{dividends}{Dividend series that is coercible to
  xts.}

  \item{close}{Close price series that is coercible to
  xts.}
}
\value{
  A xts object containing the columns: \describe{ \item{
  Split }{ The split adjustment ratio. } \item{ Div }{ The
  dividend adjustment ratio. } }
}
\description{
  Create split and dividend adjustment ratio vectors.
}
\details{
  \itemize{ \item If only \code{splits} is provided, the
  resulting object will only have as many observations as
  \code{splits}.  \item If \code{splits} and \code{close}
  are provided, the resulting object will have as many
  observations as \code{max(NROW(splits), NROW(close))}.
  \item \code{close} is required if \code{dividends} is
  provided. }
}
\author{
  Joshua Ulrich
}
\keyword{ts}

