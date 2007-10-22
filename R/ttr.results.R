"ttr.results" <-
function(beg, end, sig, price) {

rows= nrow(price)
tr  = rep(0,rows)
ind = matrix(1,nrow(price),4)

# Make sure beg & end dates fall within dates in price matrix
if(beg>=end) stop(beg," is greater than ",end,". Specify new date range.\n")
if(beg > max(price[,1])) stop("Start date, ",beg," is > the last date in ",
                              "the date range, ",max(price[,1]),".\n")                              
if(end < min(price[,1])) stop("End date, ",end," is < the first date in ",
                              "the date range, ",min(price[,1]),".\n")
if(beg < min(price[,1])) {
   nbeg = min(price[,1])
   cat(" Start date,",beg,"lies outside date range. Using",nbeg,"instead.\n")
   beg=nbeg
}
if(end > max(price[,1])) {
   nend = max(price[,1])
   cat(" End date,",end,"lies outside date range. Using",nend,"instead.\n")
   end=nend
}

# Find start & end dates, and print warning (if necessary)
if(is.na(match(beg, price[,1]))) {
   nbeg = min(ifelse(price[,1]>=beg, price[,1],NA), na.rm=T)
   cat(" Start date,",beg,"not found; using",nbeg,"instead\n")
   beg=nbeg
}
if(is.na(match(end, price[,1]))) {
   nend = max(ifelse(price[,1]<=end, price[,1],NA), na.rm=T)
   cat(" End date,",end,"not found; using",nend,"instead\n")
   end=nend
}

beg.loc = match(beg, price[,1])  # Start location
end.loc = match(end, price[,1])  # End location

sig = c(0, sig[1:(length(sig)-1)])
sig[beg.loc] = 0   # NEED TO DOUBLE CHECK THIS: sig[beg.loc] OR sig[beg.loc-1]???
                   # IF beg.loc-1, THEN beg.loc+1 in FOR LOOP -> beg.loc
delta = c(NA, price[2:rows,2]/price[1:(rows-1),2]-1)

dp     = ifelse(sig==1, delta, 0)
dp.lm  = dp * 2
dp.ls  = ifelse(sig== 1, delta, ifelse(sig==-1, delta*-1, 0))
dp.lsm = dp.ls * 2

for(i in 2:nrow(ind)) {
   ind[i,1] = (1+dp    [i]) * ind[i-1,1]   # Long Only Return Index
   ind[i,2] = (1+dp.lm [i]) * ind[i-1,2]   # Long w/margin Return Index
   ind[i,3] = (1+dp.ls [i]) * ind[i-1,3]   # Long & Short Return Index
   ind[i,4] = (1+dp.lsm[i]) * ind[i-1,4]   # Long & Short w/margin Return Index

   if(sig[i]!=sig[i-1])            # Number of Trades
      tr[i] = tr[i-1] + 1
   else
      tr[i] = tr[i-1]

}

# Length of trades
tr = tr[beg.loc:end.loc]
tr = tr - tr[1]
ltr=NULL
for (i in 1:max(tr)) {
   ltr[i]=sum(tr==i)
}

# Calculate Trade Length Results
sigdif = diff(sig, lag=1)
sigdif = sigdif[beg.loc:end.loc]

wlbsd= 1:length(sigdif)
wbsdi= na.omit(replace(sigdif,ifelse( sigdif!=0,NA,wlbsd),NA))
wbsds= ifelse(wbsdi>0,1,-1)
wlgth= ltr*wbsds
wyrs = as.numeric(as.Date(as.character(end),"%Y%m%d")-
                  as.Date(as.character(beg),"%Y%m%d")) / 364.25
if(wyrs < 1) wyrs = 1

# Calculate Results
tpy = tr[length(tr)] / wyrs                         # Trades per year
atrr1 = 100*((ind[end.loc,1]/ind[beg.loc,1])^(1/wyrs)-1)   # Annulaized Rule Return
atrr2 = 100*((ind[end.loc,2]/ind[beg.loc,2])^(1/wyrs)-1)   # Annulaized Rule Return
atrr3 = 100*((ind[end.loc,3]/ind[beg.loc,3])^(1/wyrs)-1)   # Annulaized Rule Return
atrr4 = 100*((ind[end.loc,4]/ind[beg.loc,4])^(1/wyrs)-1)   # Annulaized Rule Return
asd1  = 100*sd(ifelse(dp    ==0,NA,dp    ),na.rm=T) * sqrt(250)
asd2  = 100*sd(ifelse(dp.lm ==0,NA,dp.lm ),na.rm=T) * sqrt(250)
asd3  = 100*sd(ifelse(dp.ls ==0,NA,dp.ls ),na.rm=T) * sqrt(250)
asd4  = 100*sd(ifelse(dp.lsm==0,NA,dp.lsm),na.rm=T) * sqrt(250)
asd1o = 100*sd(ifelse(dp    !=0,NA,delta ),na.rm=T) * sqrt(250)
asd2o = 100*sd(ifelse(dp.lm !=0,NA,delta ),na.rm=T) * sqrt(250)
asd3o = 100*sd(ifelse(dp.ls !=0,NA,delta ),na.rm=T) * sqrt(250)
asd4o = 100*sd(ifelse(dp.lsm!=0,NA,delta ),na.rm=T) * sqrt(250)
sharp1= atrr1/asd1
sharp2= atrr2/asd2
sharp3= atrr3/asd3
sharp4= atrr4/asd4
abhr = 100*((price[end.loc,2]/price[beg.loc,2])^(1/wyrs)-1)  # Annualized B&H Return
sdbh = 100*sd(delta[beg.loc:end.loc],na.rm=T) * sqrt(250)
sharpbh = abhr/sdbh
ltb = abs(mean(ifelse(wlgth>0,wlgth,NA),na.rm=T))   # Avg. Length - BUY
lts = abs(mean(ifelse(wlgth<0,wlgth,NA),na.rm=T))   # Avg. Length - SELL
lrb = range(abs(ifelse(wlgth>0,wlgth,NA)),na.rm=T)  # Range - BUY
lrs = range(abs(ifelse(wlgth<0,wlgth,NA)),na.rm=T)  # Range - SELL

# Print Output Table
cat("\n Results for the period:",beg,"to",end,"\n\n",
    "_Strategy_\t Rule return\t Rule-B&H\t Sharpe Ratio\t StDev Ratio \n",
    "Long Only \t", atrr1,"\t", atrr1-abhr, "\t", sharp1, "\t", asd1/asd1o, "\n",
    "Long & Short \t", atrr3,"\t", atrr3-abhr, "\t", sharp3, "\t", asd3/asd3o, "\n",
    "Long-Margin \t", atrr2,"\t", atrr2-abhr, "\t", sharp2, "\t", asd2/asd2o, "\n",
    "Long/Short-M \t", atrr4,"\t", atrr4-abhr, "\t", sharp4, "\t", asd4/asd4o, "\n",
    "B&H Ann Ret \t",  abhr, "\t 0.000000 \t", sharpbh, "\t 1.000000 \n\n",
    "Trades/Year \t",  tpy,"\n",
    "Avg Lgth Buy \t", ltb,"\n",
    "Avg Lgth Sell \t",lts,"\n",
    "Min/Max Buy \t",  lrb[1],"/",lrb[2],"\n",
    "Min/Max Sell \t", lrs[1],"/",lrs[2],"\n\n")
}
