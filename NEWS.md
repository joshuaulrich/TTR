# Changes in 0.24.4

* Added Ethan B. Smith as a contributor. Thanks Ethan!

### NEW FEATURES

- Added a `TR()` function to calculate the true high, true low, and true
  range. Refactored `ATR()` to use the `TR()` function. Thanks to @openbmsjsc
  and Steve Bronder for the reports, and Ethan B. Smith for the PR.
  (#18, #114, #124)

### BUG FIXES

* Fix `stockSymbols()` for ticker "NA". `read.table()` converts the string "NA"
  to a missing value (NA) because `na.strings = "NA"` by default. This causes
  an issue because there's actually a company with "NA" for the ticker. (#128)

- `CTI()` did not pad its result with leading NA when the input was not
  coerced to an xts object. This was different from other TTR functions
  (e.g. `SMA()`, `RSI()`, `ROC()`). (#127)

- Removed the `VMA()` function, which was never correct because the
  results made no sense.

- Check that the `wma()` C function has enough non-NA values and throw
  an error if it doesn't. This could cause the `WMA()` function to crash
  the user's R session. (#126)

- `runMean(..., cumulative = TRUE)` didn't account for leading NA in the
  denominator. (#122)

- `runSD(x, cumulative = TRUE)` returned all NA when `x` had any leading
  NA. Thanks to Ethan B. Smith for the report. (#121)

- The `TRIX()` signal line did not use `nSig` unless `maType` was provided.
  Thanks to @SatoshiReport for the... report. (#120)

### MISCELLANEOUS

- Use symbols for native routine entry points to make them explicit and
  unable to be found accidentally. (#123)

# Changes in 0.24.3

### SIGNIFICANT USER-VISIBLE CHANGES

- Significant improvement to `ALMA()` calculation speed. Thanks to
  Ethan B. Smith for the report and suggested fix. (#117)

### NEW FEATURES

- Added Keltner Channels. Thanks to Nick Procyk for the patch and docs (#106)

### BUG FIXES

- `runPercentRank()` would segfault if `x` had fewer non-NA observations
  than the value for `n`. Thanks to Ian Fellows for the report. (#112)

- `run*(x, n = 1, cumulative = TRUE)` functions would return NA for the
  first value. Thanks to Ethan B. Smith for the report and PR! (#111, #108, #88)

- Fix NA check off-by-one error in `aroon()` that caused it to fail if there
  were exactly enough non-NA values. (#102)

# Changes in 0.24.2

### BUG FIXES

- Check for `ratio > 0` before calculating `n` in `zlema()` C code. The prior
  code could result in division by 0, which was flagged by clang-UBSAN.
  Thanks to Prof Brian Ripley for the report. (#100)

# Changes in 0.24.1

### BUG FIXES

- Fix leading NA accounting in `wma()` C code. The prior code caused invalid
  reads under valgrind. Thanks to Prof Brian Ripley for the report. (#99)

- Check for `ratio > 0` before calculating `n` `n ema()` C code. The prior
  code could result in division by 0, which was flagged by UBSAN. Thanks to
  Prof Brian Ripley for the report. (#100)

- Make `ALMA()` output length equal input length when the input can not be
  converted to xts. This was caused by the difference between
  `rollapply.default()` and `rollapply.xts()`. Thanks to GitHub user
  @marksimmonds for the report. (#29)

- Fix `stoch()` in very rare cases where `fastK = Inf`. I could only reproduce
  this if the Close is > High and High and Low are equal, but that is a data
  error. I fixed anyway because there may be other cases I don't anticipate.
  Thanks to GitHub user @cjuncosa for the report. (#52)

- Fix `MFI()` when money flow is always zero or positive. The denominator of
  the money ratio will be zero if there is no negative money flow for `n`
  consecutive observations (e.g. during a strong up-trend), which causes the
  money flow index to be Inf. Set the money flow index to 100 in this case.

  And the money ratio will be NaN if there's no money flow for `n`
  consecutive observations (e.g. if there are no trades), which causes the
  money flow index to be NaN. Set the money flow index to 50 in this case.

  Thanks to GitHub user @jgehw for the report, reproducible example, and
  suggested patch. (#81)

# Changes in 0.24.0

### SIGNIFICANT USER-VISIBLE CHANGES

- Updated `stockSymbols()` to use the NASDAQ FTP site instead of downloading
  the CSV from the NASDAQ stock screener page. Some columns are no longer
  populated because they are not provided in the FTP file:
      LastSale ,MarketCap, IPOyear, Sector, Industry
  These columns will be removed in a future version. (#98, #5, #97)

- `runPercentRank(x, n, cumulative = TRUE)` now sets observations in the
  initialization period to NA. This is consistent with the other
  running/rolling functions in TTR. If you want the previous behavior,
  you should use `runPercentRank(x, n = 1, cumulative = TRUE)`. Thanks to
  GitHub user @httassadar for the report. (#73)

### NEW FEATURES

- Add Ehler's Correlation Trend Indicator. Thanks to Evelyn Mitchell for
  the suggestion, and for Ethan B. Smith for the initial implementation. (#92)

### BUG FIXES

- `runMAD()` returned incorrect values when `cumulative = TRUE` and the input
  contained leading NA. Thanks to GitHub user @stellathecat for the report.
  This also affected `runMedian()`. (#93)

- `ZLEMA()` would crash when `ratio = 0.0` and `n` was not specified. Thanks
  to GitHub user @yogat3ch for the report! (#95)

- `WMA()` did not return an xts object when passed an xts object for `x` that
  had leading NA, with the default `wts = 1:n`. Thanks to Cory Fletcher for
  reporting this issue via email. (#96)

- `stoch()` was wrong when `bounded = FALSE`. Thanks to GitHub user @rfinfun
  for the report and patch. (#74)

- `HMA()` threw an error when `n` was an odd number. This was because the
  first call to `WMA()` used `n = n / 2` which caused `n` to not be an
  integer. Thanks to GitHub user @dragie for the report. (#76)

# Changes in 0.23.0

### SIGNIFICANT USER-VISIBLE CHANGES

- Update `DVI()` to use `runPercentRank()`. Thanks to Ivan Popivanov for the
  patch.

- `getYahooData()` now returns an xts object with Date index (not POSIXct).

- Column names for moving average function outputs are no longer based on the
  input column names.

### NEW FEATURES

- Add `HMA()` and `ALMA()` functions/docs. Thanks to Ivan Popivanov.

- Add Ultimate Oscillator function/docs/tests. Thanks to Ivan Popivanov.

### BUG FIXES

- `run*()` functions now error if there are not enough non-NA values.

- Change all instances of `lag()` to `lag.xts()` in case `x` is a matrix.
  Thanks to Ivan Popivanov for the report.

- Correct output column names in `ATR()` docs.

- `CLV()` now sets NaN and Inf values to 0, instead of only NaN values.

- Fix `OBV()` so `OBV[t] = OBV[t-1]` when `Close[t] == Close[t-1]`.

- Fix dead links in documentation.

# Changes in 0.22.0

### SIGNIFICANT USER-VISIBLE CHANGES

- `CCI()` now returns an object with column names ("cci").

- All moving average functions now attempt to set column names.

- Added clarification on the displaced nature of `DPO()`.

- `SAR()` now sets the initial gap based on the standard deviation of the
  high-low range instead of hard-coding it at 0.01.

### NEW FEATURES

- Added `rollSFM()` function that calculates alpha, beta, and R-squared for a
  single-factor model. Thanks to James Toll for the prototype.

- Added `runPercentRank()` function. Thanks to Charlie Friedemann.

- Moved slowest portion of `aroon()` to C.

- `DonchianChannel()` gains an `include.lag = FALSE` argument, which includes
  the current period's data in the calculation. Setting it to `TRUE` replicates
  the original calculation. Thanks to Garrett See and John Bollinger.

- The Stochastic Oscillator and Williams' %R now return 0.5 (instead of NaN)
  when a securities' price doesn't change over a sufficient period.

- All moving average functions gain `...`.

- Users can now change alpha in Yang Zhang volatility calculation.

### BUG FIXES

- Fixed `MACD()` when `maType` is a list. Now `mavg.slow = maType[[2]]` and
  `mavg.fast = maType[[1]]`, as users expected based on the order of the
  `nFast` and `nSlow` arguments. Thanks to Phani Nukala and Jonathan Roy.

- Fixed bug in `lags()`. Thanks to Michael Weylandt.

- Corrected error in Yang Zhang volatility calculation. Thanks to several
  people for identifying this error.

- Correction to `SAR()` extreme point calculations. Thanks to Vamsi Galigutta.

- `adjRatios()` now ensures all inputs are univariate. Thanks to Garrett See.

- `EMA()` and `EVWMA()` now ensure `n` is less than the number of non-NA
  values. Thanks to Roger Bos.

- Fix to `BBands()` docs. Thanks to Evelyn Mitchell.

# Changes in 0.21.1

### BUG FIXES

- Fixed `stockSymbols()` for nasdaq.com changes (again), and attempted to make
  `stockSymbols()` more robust to nasdaq.com changes.

- Corrected final calculation in Yang-Zhang volatility. Thanks to Shal Patel.

- Corrected `k` in Yang-Zhang volatility. Thanks to Ian Rayner.

- Corrected `s2o` and `s2c` in Yang-Zhang volatility. Thanks to Ian Rayner.

- Corrected `KST()` when input is xts (result is now * 100). Thanks to Yuanwei.

# Changes in 0.21.0

### NEW FEATURES

- Added variable moving average function, `VMA()`.

- Added Brian Peterson's price bands function, `PBands()`.

- Added David Varadi's `DVI()` indicator.

- Added `wilder` and `ratio` arguments to `DEMA`. Thanks to Matthew Fornari
  for the suggestion.

### BUG FIXES

- Changed `wilderSum()` to seed initial value with raw sum. This matches
  Wilder's original calculations. Thanks to Mahesh Bp for the report.

- The `BBands()` standard deviation calculation now uses the population
  instead of sample statistic. This is consistent with Bollinger Band
  literature. Thanks to Jeff Ryan for the patch.

- Fixed `stockSymbols()` for nasdaq.com changes.

- Fixed `ZLEMA()` default ratio by changing it from `2/(n-1)` to `2/(n+1)`.
  This makes it consistent with `EMA()`. Thanks to Dirk Eddelbuettel.

- Corrected close-to-close volatility. Thanks to James Toll for the report.

- `adjRatios()` failed (spectacularly) if there were missing close prices.
  Thanks to Garrett See for the report.

# Changes in 0.20.2

### NEW FEATURES

- Added `VWAP()` and `VWMA()`. Thanks to Brian Peterson.

- Added v-factor generalization to `DEMA()`. Thanks to John Gavin.

- Updated `volatility()` to handle univariate case of `calc = "close"`.
  Thanks to Cedrick Johnson.

- Moved `EMA()`, `SAR()`, and `wilderSum ()`from .Fortran to `.Call ()`and
  used `xts:::naCheck()` instead of TTR's NA check mechanism.

- `RSI ()`up/down momentum is now faster with xts. Thanks to Jeff Ryan.

- If `ratio` is specified in `EMA ()`but `n` is missing, the traditional
  value of `n` is approximated and returned as the first non-NA value.

### BUG FIXES

- Fix to `stoch()` when `maType` is a list and `n` is not set in the list's
  3rd element. Thanks to Wind Me.

- Fixed `fastK` in `stoch()` when `smooth != 1`.

- Fixed segfault in `EMA ()`when `n < NROW(x)`. Thanks to Douglas Hobbs.

- `test.EMA.wilder()` failed under R-devel. Thanks to Prof Brian Ripley.

# Changes in 0.20.1

### NEW FEATURES

- Updated `CMO()`, `DPO()`, `DonchianChannel()`, `RSI()`, and `TDI ()`
  to *explicitly* use xts internally.

### BUG FIXES

- Fixed bug in `WMA()`, `EVWMA()`, `ZLEMA()`, and `GMMA()`; results were not
  being `reclass()`ed back to their original class.

- Set column names after `cbind ()` call in the following functions:
    - `ADX()`
    - `aroon()`
    - `ATR()`
    - `BBands()`
    - `DonchianChannel()`
    - `EMV()`
    - `KST()`
    - `MACD()`
    - `stoch()`
    - `SMI()`
    - `TDI()`
    - `TRIX()`

- Fixed bug in `VHF()`; missing `abs()` calculation in the denominator.
  Thanks to Jürgen Wurzer for the report!

# Changes in 0.20.0

- Fixed version number; 0.20-0 is now > 0.14-0 (rookie mistake).

### SIGNIFICANT USER-VISIBLE CHANGES

- `getYahooData()` now returns an xts object.

- Added column names to output for `ADX()`, `EMV()`, and `CLV ()` (for xts).

- `momentum()` in `CMO()` no longer sets `na = 100`.

- Replaced `na` argument in `momentum()` and `ROC()` with `na.pad`.

- Moved `maType` argument default values from function formals to
  function body for the following functions:
    - `ADX()`
    - `ATR()`
    - `CCI()`
    - `DPO()`
    - `EMV()`
    - `KST()`
    - `MACD()`
    - `RSI()`
    - `TRIX()`
    - `BBands()`
    - `chaikinVolatility()`
    - `stoch()`
    - `SMI()`

### NEW FEATURES

- `adjRatios()` creates split and/or dividend adjustment ratio series via
  C code.

- `GMMA()` calculates the Guppy Multiple Moving Average.

- `volatility()` now has Yang Zhang, and Garman-Klass (Yang Zhang)
  calculations.

- The functions below now have cumulative argument.  This allows the
  calculation of "from inception" running series.
    - `runSum()`, `runMin()`, `runMax()`
    - `runMean()`, `runMedian()`
    - `runCov()`, `runCor()`, `runVar()`, `runSD()`, `runMAD()`

- Added internal smoothing to `FastK` in `stoch()` via `smooth` argument.
  Thanks to Stanley Neo.

- `getYahooData()` now uses `adjRatios(),` which yields significant speed
  improvements for larger data sets.

- All functions now use xts internally, adding support for all major time
  series classes. If `try.xts()` fails on the input object(s), they will be
  converted to a matrix and a matrix object will be returned.

- Added `bounded` arg to `stoch()` and `SMI()`, which includes the current
  period in the calculation.

- Added the zig zag indicator: `ZigZag()`.

- Added volatility estimators/indicators: `volatility()`, with the following
  calculations:
    - Close-to-Close
    - Garman Klass
    - Parkinson
    - Rogers Satchell

- Added Money Flow Index: `MFI()`.

- Added Donchian channel: `DonchianChannel()`.

- Added `multiple` argument to `TDI()`, allowing more user control.

- Added `naCheck()` and implemented it in the moving average functions.

### BUG FIXES

- Fixed bug when `maType` was a list and `n` was not specified in `maType`.
  This affected: `stoch()`, `SMI()`, `RSI()`, `KST()`, `MACD()`, `TRIX()`.

- Corrected NaN replacement in `CLV()`.

- Corrected `williamsAD()`: the result is 0 if C(t) = C(t-1).

- Corrected `runMedian()` and `runMAD()`.  The argument controlling which type
  of median to calculate for even-numbered samples wasn't being passed to
  the Fortran routine.

- `aroon()` calculation starts at period `n+1`, instead of `n`.

- Added NA to first element of `closeLag` of `ATR()`.

- Corrected `BBands()` and `CCI()` for `rowMeans()` use on xts objects.

- Made changes to Rd files to pass R CMD check on R-devel (2.9.0).

# Changes in 0.14.0

### SIGNIFICANT USER-VISIBLE CHANGES

- Changed default `type` of `ROC()` to `continuous`.

- Changed `BBands()` %B output value from `pct.b` to `pctB`.

- Changed `WPR()` output value from `pct.R` to `pctR`.

- Changed `WPR()` MA output value from `ma.emv` to `emvMA`.

- Changed `aroon()` output values from `aroon.xx` to `aroonXx`.

- Renamed:
    - `chaikinMF()` to `CMF()`
    - `stochastic()` to `stoch()`
    - `bollingerBands()` to `BBands()`

- Set `na = NA` for `momentum()` and `ROC()` functions in files KST.R,
  RSI.R, and TDI.R, and changed `ROC()` to use `type = "discrete"`
  in chaikinVolatility.R.

- Made the following changes to the `ZLEMA()` function:
   - Add `ratio = NULL` argument.
   - Non-integer lags are a weighted mean of the two nearest
     observations, based on their proximity to the lag value.
   - Change 'lag = ratio^(-1)' to fully support `ratio` argument.

- Changed the `BBands()` function's `sd` argument from a list
  that allows other dispersion functions to simply indicate the
  number of standard deviations to use.

### NEW FEATURES

- Changed `maType` arguments and updated documentation for:
    - `RSIm()`
    - `ADX()`
    - `ATR()`
    - `CCI()`
    - `DPO()`
    - `EMV()`
    - `RSI()`
    - `BBands()`
    - `chaikinVolatility()`
    - `stoch()`
    - `SMI()`
    - `TRIX()`
    - `MACD()`
    - `KST()`

- Added Stochastic Momentum Index `SMI()` and `williamsAD()` functions
  and documentation.

- Added Fortran implementations of
    - `SMA()`
    - `EMA()`
    - `WMA()`
    - `EVWMA()`
    - `ZLEMA()`
    - `PSAR()`

- Added NA checking/handling for many functions.

- Added `ratio = NULL` argument to `EMA()`.

- Changed all usage of `roll*()` to their respective Fortran
  implementations and removed the `rollFun()` function.  Added Fortran
  based functions are:
    - `runSum()`
    - `wilderSum()`
    - `runMin()`
    - `runMax()`
    - `runMean()`
    - `runCov()`
    - `runCor()`
    - `runVar()`
    - `runSD()`
    - `runMedian()`
    - `runMAD()`

- Changed `CCI()` to use `runMAD()` internally.

### DEPRECATED & DEFUNCT

- Removed `oscillator()` function and transferred functionality
  to `MACD()` function.

- Removed `chaikinOscillator()`, since it can be created via
  `MACD(chaikinAD(...))`.

### BUG FIXES

- `match.arg(type)` in `ROC()` changed to simple subsetting of type.

- Changed trailing zeros to trailing NAs in `DPO()`.

- Fixed `WMA()` bug that allowed `x` and `wts` vectors to have different
  length if either series had leading NAs (similar to `EVWMA()` function).

- Fixed `runCov()` bug that allowed `x` and `y` vectors to have different
  length if either series had leading NAs (similar to `EVWMA()` function).

- Corrected `EVWMA()` to start at period `n` instead of `n-1`.

- Removed `message` function from CCI.R, VHF.R, WPR.R, aroon.R
  bollingerBands.R, and stochastics.R.

# Changes in 0.13.2

### SIGNIFICANT USER-VISIBLE CHANGES

- Changed order of `oscillator()` arguments from `ma.slow`, `ma.fast`, `ma.sig`
  to the traditional `ma.fast`, `ma.slow`, `ma.sig`. Thanks to Jeff Ryan.

- The arguments to the `chaikinOscillator()` function were changed as above.

- Changed `EVWMA()` so period `n` contains the value for periods `(i-n+1):n`
  and so periods `1:(n-2)` will be NA.

- Changed `EMA()` so periods `1:n` will be NA.

# Changes in 0.13.1

### SIGNIFICANT USER-VISIBLE CHANGES

- Changed `bbands()` to `bollingerBands()`.

- Changed `DX()` to `ADX()`.

- Changed `stoch()` to `stochastic()`.


### BUG FIXES

- Corrected mis-spellings in documentation.
