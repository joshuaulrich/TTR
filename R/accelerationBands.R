#' Acceleration Bands
#'
#' @aliases accelerationBands ABands
#'
#' @param HLC Object that is coercible to xts or matrix and contains High-Low-Close prices.
#' @param n Number of periods for moving average.
#' @param maType A function or a string naming the function to be called.
#' @param \dots Other arguments to be passed to the \code{maType} function.
#'
#' @return A object of the same class as \code{HLC} or a matrix (if
#' \code{try.xts} fails) containing the columns:
#'  \describe{
#'   \item{ dn }{ The lower Acceleration Band. }
#'   \item{ mavg }{ The middle Moving Average (see notes). }
#'   \item{ up }{ The upper Acceleration Band. }
#'  }
#'
#' @author Serkan Korkmaz
#'
#' @seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#' options; and note Warning section.
#' @references The following site(s) were used to code/document this indicator:
#'
#' \cr \url{https://quantstrategy.io/blog/understanding-the-acceleration-bands/} \cr
#' \cr \url{https://trendspider.com/learning-center/getting-started-with-acceleration-bands-in-technical-analysis/} \cr
#' \cr \url{https://github.com/TA-Lib/ta-lib/blob/main/src/ta_func/ta_ACCBANDS.c} \cr
#'
#' @keywords ts
#'
#' @examples
#'
#'  ## The examples below show the differences between using a
#'  ## High-Low-Close series when
#'  ## calculating Acceleration Bands.
#'  data(ttrc)
#'  accelerationBands.HLC <- accelerationBands( ttrc[,c("High","Low","Close")] )
#'
#' @rdname accelerationBands
#' @export
accelerationBands <- function(HLC, n = 20, maType, ...) {
    HLC <- try.xts(HLC, error = as.matrix)

    if (NCOL(HLC) < 3) {
        stop("Price series must contain High, Low and Close.")
    }

    H <- HLC[, 1]
    L <- HLC[, 2]
    C <- HLC[, 3]

    maArgs <- list(n = n, ...)
    # Default MA
    if (missing(maType)) {
        maType <- 'SMA'
    }

    mavg <- do.call(maType, c(list(C), maArgs))

    k <- 4 * (H - L) / (H + L)
    upR <- H * (1 + k)
    dnR <- L * (1 - k)

    up <- do.call(maType, c(list(upR), maArgs))
    dn <- do.call(maType, c(list(dnR), maArgs))

    res <- cbind(dn, mavg, up)
    colnames(res) <- c("dn", "mavg", "up")

    reclass(res, HLC)
}
