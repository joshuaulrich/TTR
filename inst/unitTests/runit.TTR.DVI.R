#
# RUnit tests TTR DVI
#

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

#################################################

test.correct_column_names <- function() {
  dat <- setNames(ttrc$Close, rownames(ttrc))
  dvi <- DVI(dat)

  checkEquals(colnames(dvi), c("dvi.mag", "dvi.str", "dvi"))
}
