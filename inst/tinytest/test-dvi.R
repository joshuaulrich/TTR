# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

# Run test
dat <- setNames(ttrc$Close, rownames(ttrc))
dvi <- DVI(dat)

expect_equal(colnames(dvi), c("dvi.mag", "dvi.str", "dvi"))
