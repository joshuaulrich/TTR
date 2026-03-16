#
# RUnit tests TTR Acceleration Bands
#

# Create input data
data(ttrc)
load(system.file("unitTests/output.accelBands.rda", package = "TTR"))

# acceleration bands
test.accelerationBands <- function() {
    checkEquals(
        accelerationBands(ttrc[, c("High", "Low", "Close")]),
        output
    )
}
