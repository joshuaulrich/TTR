#
# RUnit tests TTR moving averages
#

# test reclass works and throws error
# test xtsAttributes, both CLASS and USER
# test all.equal(CLASS) and !all.equal(CLASS) cases

# Create input data
data(ttrc)
rownames(ttrc) <- ttrc$Date
ttrc$Date <- NULL

input <- list( all=ttrc[1:250,], top=ttrc[1:250,], mid=ttrc[1:250,] )
input$top[1:10,] <- NA
input$mid[9:20,] <- NA

#iAll <- as.matrix(ttrc[1:250,])
iAll <- ttrc[1:250,]
iTop <- iAll; iTop[1:10,] <- NA
iMid <- iAll; iMid[9:20,] <- NA

hl  <- c('High','Low')
hlc <- c('High','Low','Close')
cl  <- 'Close'

# Load output data
load(system.file("unitTests/output.misc.rda", package="TTR"))

#################################################

# Rate-of-Change
test.ROC.continuous <- function() {
  roc <- ROC(iAll[,cl], type='continuous')
  checkEqualsNumeric( roc, output$allROCc )
  checkEquals( attributes(roc), attributes(output$allROCc) )
  #checkEqualsNumeric( ROC(input$top$Close, type='continuous'), output$topROCc )
  #checkException( ROC(input$mid$Close) )
}
test.ROC.discrete <- function() {
  roc <- ROC(input$all$Close, type='discrete')
  checkEqualsNumeric( roc, output$allROCd )
  checkEquals( attributes(roc), attributes(output$allROCd) )
  #checkEqualsNumeric( ROC(input$top$Close, type='discrete'), output$topROCd )
}

# Momentum
test.momentum <- function() {
  mom <- momentum(input$all$Close)
  checkEqualsNumeric( mom, output$allMom )
  checkEquals( attributes(mom), attributes(output$allMom) )
  #checkEqualsNumeric( momentum(input$top$Close), output$topMom )
  #checkException( momentum(input$mid$Close) )
}

# Close Location Value
test.CLV <- function() {
  ia <- iAll[,hlc];  rownames(ia) <- NULL
  it <- iTop[,hlc];  rownames(it) <- NULL
  oa <- as.data.frame(output$allCLV); rownames(oa) <- rownames(ia)
  ot <- as.data.frame(output$topCLV); rownames(ot) <- rownames(it)
  checkEqualsNumeric( CLV(ia), output$allCLV )
  checkEquals( attributes(CLV(ia)), attributes(output$allCLV) )
  checkEqualsNumeric( CLV(it), output$topCLV )
  checkEquals( attributes(CLV(it)), attributes(output$topCLV) )
}

# Arms' Ease of Movement
test.EMV <- function() {
  ia <- iAll[,hl];  rownames(ia) <- NULL
  emv.all <- EMV(ia, input$all$Volume)
  checkEqualsNumeric( emv.all, output$allEMV )
  checkEquals( attributes(emv.all), attributes(output$allEMV) )
  #emv.top<- EMV(input$top[,c('High','Low')], input$top$Volume)
  #checkEqualsNumeric( emv.top, output$topEMV )
  #checkEquals( attributes(emv.top), attributes(output$topEMV) )
  #checkException( EMV(input$mid[,c('High','Low')], input$mid$Volume) )
  #checkException( EMV(input$all[,c('High','Low')], input$mid$Volume) )
  #checkException( EMV(input$mid[,c('High','Low')], input$all$Volume) )
}

# Know Sure Thing
test.KST <- function() {
  checkEqualsNumeric( KST(input$all$Close), output$allKST )
  checkEquals( attributes(KST(input$all$Close)), attributes(output$allKST) )
  #checkEqualsNumeric( KST(input$top$Close), output$topKST )
  #checkException( KST(input$mid$Close) )
}

# CTI
test.CTI <- function() {
  checkEquals(length(input$all$Close), length(CTI(input$all$Close)))
}
