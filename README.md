### About

TTR is an [R](https://www.r-project.org) package that provides the most popular
technical analysis functions for financial market data. Many of these functions
are used as components of systematic trading strategies and financial charts.

### TTR for enterprise

Available as part of the Tidelift Subscription.

The maintainers of `TTR` and thousands of other packages are working with Tidelift to deliver commercial support and maintenance for the open source dependencies you use to build your applications. Save time, reduce risk, and improve code health, while paying the maintainers of the exact dependencies you use. [Learn more.](https://tidelift.com/subscription/pkg/cran-ttr?utm_source=cran-ttr&utm_medium=referral&utm_campaign=enterprise&utm_term=repo)

### Supporting TTR through Patreon

If you are interested in supporting this project, please consider [becoming a patron](https://www.patreon.com/joshuaulrich).

### Installation

The current release is available on [CRAN](https://CRAN.R-project.org/package=TTR),
which you can install via:

```r
install.packages("TTR")
```

To install the development version, you need to clone the repository and build
from source, or run one of:

```r
# lightweight
remotes::install_github("joshuaulrich/TTR")
# or
devtools::install_github("joshuaulrich/TTR")
```

You will need tools to compile C, C++, and Fortran code. See the relevant
appendix in the [R Installation and Administration manual](https://cran.r-project.org/doc/manuals/r-release/R-admin.html)
for your operating system:

- [Windows](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset)
- [MacOS](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS) (the [R for Mac OS X Developer's Page](https://mac.R-project.org/) might also be helpful)
- [Unix-alike](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Essential-and-useful-other-programs-under-a-Unix_002dalike)

### Getting Started

Here are a few examples of some of the more well-known indicators:

```r
# "TTR Composite" (simulated data)
data(ttrc)

# Bollinger Bands
bbands <- BBands( ttrc[,c("High","Low","Close")] )

# Directional Movement Index
adx <- ADX(ttrc[,c("High","Low","Close")])

# Moving Averages
ema <- EMA(ttrc[,"Close"], n=20)
sma <- SMA(ttrc[,"Close"], n=20)

# MACD
macd <- MACD( ttrc[,"Close"] )

# RSI
rsi <- RSI(ttrc[,"Close"])

# Stochastics
stochOsc <- stoch(ttrc[,c("High","Low","Close")])
```

TTR works with the `chartSeries()` function in [quantmod](https://github.com/joshuaulrich/quantmod). Here's an example that uses `chartSeries()` and adds TTR-calculated indicators and overlays to the chart.

```r
# "TTR Composite" (simulated data)
data(ttrc)

# Use quantmod's OHLCV extractor function to help create an xts object
xttrc <- xts(OHLCV(ttrc), ttrc[["Date"]])

chartSeries(xttrc, subset = "2006-09/", theme = "white")
addBBands()
addRSI()
```

![](https://drive.google.com/uc?export=view&id=1TrgoZujgcI9GCMEWHlDgzkQQvBItyLwq)

###### Have a question?

Ask your question on [Stack Overflow](https://stackoverflow.com/questions/tagged/r)
or the [R-SIG-Finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance)
mailing list (you must subscribe to post).

### Contributing

Please see the [contributing guide](.github/CONTRIBUTING.md).

### See Also

- [quantmod](https://CRAN.R-project.org/package=quantmod): quantitative financial modeling framework
- [xts](https://CRAN.R-project.org/package=xts): eXtensible Time Series based
on [zoo](https://CRAN.R-project.org/package=zoo)

### Author

[Joshua Ulrich](https://about.me/joshuaulrich)

