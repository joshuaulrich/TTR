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
load(system.file("tinytest/output/misc.rda", package="TTR"))

#################################################

# Rate-of-Change
roc <- ROC(iAll[,cl], type='continuous')
expect_equal( roc, output$allROCc )
expect_equal( attributes(roc), attributes(output$allROCc) )
#expect_equal( ROC(input$top$Close, type='continuous'), output$topROCc )
#expect_error( ROC(input$mid$Close) )

roc <- ROC(input$all$Close, type='discrete')
expect_equal( roc, output$allROCd )
expect_equal( attributes(roc), attributes(output$allROCd) )
#expect_equal( ROC(input$top$Close, type='discrete'), output$topROCd )

# Momentum
mom <- momentum(input$all$Close)
expect_equal( mom, output$allMom )
expect_equal( attributes(mom), attributes(output$allMom) )
#expect_equal( momentum(input$top$Close), output$topMom )
#expect_error( momentum(input$mid$Close) )

# Close Location Value
ia <- iAll[,hlc];  rownames(ia) <- NULL
it <- iTop[,hlc];  rownames(it) <- NULL
oa <- as.data.frame(output$allCLV); rownames(oa) <- rownames(ia)
ot <- as.data.frame(output$topCLV); rownames(ot) <- rownames(it)
expect_equal( CLV(ia), output$allCLV )
expect_equal( attributes(CLV(ia)), attributes(output$allCLV) )
expect_equal( CLV(it), output$topCLV )
expect_equal( attributes(CLV(it)), attributes(output$topCLV) )

# Arms' Ease of Movement
ia <- iAll[,hl];  rownames(ia) <- NULL
emv.all <- EMV(ia, input$all$Volume)
expect_equal( emv.all, output$allEMV )
expect_equal( attributes(emv.all), attributes(output$allEMV) )
#emv.top<- EMV(input$top[,c('High','Low')], input$top$Volume)
#expect_equal( emv.top, output$topEMV )
#expect_equal( attributes(emv.top), attributes(output$topEMV) )
#expect_error( EMV(input$mid[,c('High','Low')], input$mid$Volume) )
#expect_error( EMV(input$all[,c('High','Low')], input$mid$Volume) )
#expect_error( EMV(input$mid[,c('High','Low')], input$all$Volume) )

# Know Sure Thing
expect_equal( KST(input$all$Close), output$allKST )
expect_equal( attributes(KST(input$all$Close)), attributes(output$allKST) )
#expect_equal( KST(input$top$Close), output$topKST )
#expect_error( KST(input$mid$Close) )

# CTI
expect_equal(length(input$all$Close), length(CTI(input$all$Close)))
