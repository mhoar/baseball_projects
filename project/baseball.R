library("psych")
###################Loading BaseBall Stats  #########
baseball2014 <- read.delim("~/Documents/coursera/dataanainf/project/baseball2014Projections.csv")
#baseball = (oliverBaseball[1:345, 2:22] + zipBaseball[1:345, 2:22] + steamerBaseball[1:345, 2:22])/3
View(baseball2014)
names(baseball)

orig = par()
plot(baseball2014$SB, baseball2014$AVG)
summary(baseball2014$SB)
hist(scale(baseball2014$SB), col="lightblue", breaks=25, xlab="SB Distribution", ylab="Frequency", main="Stolen Base Projections" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)
hist(scale(baseball2014$SBAB), col="lightblue", breaks=25, xlab="SB Distribution per AB", ylab="Frequency", main="Stolen Base Projections" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)

describe(baseball2014$SB)

plot(scale(baseball2014$SB), scale(baseball2014$AVG), col="gray", pch=23, bg="#00339966", cex=.5, xlim=c(-1,9), xlab="Stolen Bases", ylab="Batting Average", main="SB v. AVG Distribution" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)
text(scale(baseball2014$SB), scale(baseball2014$AVG), baseball2014[ ,"Player"], cex=0.7, pos=4, col="#990000")

plot(baseball2014$SB, baseball2014$AVG, col="gray", pch=23, bg="#00339966", cex=.5,ylim=c(.2, .33), xlim=c(0, 70), xlab="Stolen Bases", ylab="Batting Average", main="SB v. AVG Distribution" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)
abline(h=mean(baseball2014$AVG), v=mean(baseball2014$SB), col="blue")
text(baseball2014$SB, baseball2014$AVG, baseball2014[ ,"Player"], cex=0.7, pos=4, col="#990000")

plot(baseball2014$HR, baseball2014$AVG, col="gray", pch=23, bg="#00339966", cex=.5,ylim=c(.2, .33), xlim=c(0, 50), xlab="Home Runs", ylab="Batting Average", main="HR v. AVG Distribution" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)
abline(h=mean(baseball2014$AVG), v=mean(baseball2014$HR), col="blue")
text(baseball2014$HR, baseball2014$AVG, baseball2014[ ,"Player"], cex=0.5, pos=4, col="#990000")

corSB_AVG = cor(baseball[ ,"SB"] , baseball[ , "AVG"])
plot(baseball[ ,"SB"], baseball[ , "AVG"], xlab="Stolen Bases", ylab="Batting Average", main="SB v. AVG" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)

text(baseball[ , "SB"], baseball[ , "AVG"], baseball[ ,"Name"], cex=0.6, pos=4, col="red")

plot(scale(baseball$SB), scale(baseball$HR))
text(scale(baseball$SB), scale(baseball$HR), baseball[ , "SB"], cex=0.5, pos=4, col="red")
plot(scale(baseball$SB), scale(baseball$AVG), col="gray", pch=23, bg="#00339966", cex=.5, xlim=c(-1,10), xlab="Stolen Bases", ylab="Batting Average", main="SB v. AVG Distribution" , sub="Based Upon 2014 Baseball Projections", col.sub="red", cex.sub=.8)
text(scale(baseball$SB), scale(baseball$AVG), baseball[ ,"Name"], cex=0.7, pos=4, col="#990000")
hist(baseball$SB, breaks=30, freq=F, col="lightblue", xlab="Stolen Bases", main="Stolen Bases Distribution", col.main="blue")

verySlowSpeed = slowSpeed = aveSpeed = fastSpeed = veryFastSpeed = 0
fifths = quantile(baseball2014$SBAB, c(.2, .4, .6, .8))

len = length(baseball2014$SBAB)
for (i in 1:len)
{
  if (baseball2014$SBAB[i] <= fifths[1])
    {verySlowSpeed = verySlowSpeed + 1}
  else if (baseball2014$SBAB[i] <= fifths[2])
   {slowSpeed = slowSpeed + 1} 
  else if (baseball2014$SBAB[i] <= fifths[3])
    {aveSpeed = aveSpeed + 1}
  else if (baseball2014$SBAB[i] <= fifths[4])
    {fastSpeed = fastSpeed + 1}
  else
    {veryFastSpeed = veryFastSpeed + 1}
}

speedCounts = c(verySlowSpeed, slowSpeed, aveSpeed, fastSpeed, veryFastSpeed)
barplot(speedCounts, col="lightblue", names.arg=c("V Slow", "Slow", "Ave", "Fast", "V Fast"), xlab="Relative Speed", main="Distribution of bases stealers into fifths")
plot(scale(baseball2014$SB), baseball2014$AB, main="SB to AB", ylim=c(200, 600), xlim = c(-1, 9), col="gray", pch=23, bg="#00339966", cex=.5, col.sub="red", cex.sub=.8)
text(scale(baseball2014$SB), baseball2014$AB, baseball2014[ ,"Player"], cex=0.7, pos=4, col="#990000")

stripchart(baseball2014$SB, method="stack", at=0, pch=19)
abline(v=quantile(baseball2014$SB, .75), col="blue")
text(baseball2014$SB, baseball2014[ ,"Player"], cex=0.7, pos=4, col="#990000")

###########  for SB to AB Ratio

avespd = subset(baseball2014, baseball2014$SBAB <= quantile(baseball2014$SBAB, .6 ) & baseball2014$SBAB > quantile(baseball2014$SBAB, .4 ))
vfspd = subset(baseball2014, baseball2014$SBAB > quantile(baseball2014$SBAB, .8 ))
vsspd = subset(baseball2014, baseball2014$SBAB < quantile(baseball2014$SBAB, .2 ))
sspd = subset(baseball2014, baseball2014$SBAB <= quantile(baseball2014$SBAB, .4 ) & baseball2014$SBAB > quantile(baseball2014$SBAB, .2 ))
fspd = subset(baseball2014, baseball2014$SBAB <= quantile(baseball2014$SBAB, .8 ) & baseball2014$SBAB > quantile(baseball2014$SBAB, .6 ))


for(i in 1:15)
{
  sampfspd = fspd[sample( nrow(fspd) ,50), ]
  sampavespd = avespd[sample( nrow(avespd), 50), ]
  sampvfspd = vfspd[sample( nrow(vfspd), 50), ]
  sampvsspd = vsspd[sample( nrow(vsspd), 50), ]
  sampsspd = sspd[sample( nrow(sspd), 50), ]
}

boxplot(sampvsspd$AVG, sampsspd$AVG, sampavespd$AVG, sampfspd$AVG, sampvfspd$AVG, names=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"), main="Comparing Sample Means per Speed Rating", col="#33669925" )
abline(h=mean(baseball2014$AVG), v=mean(baseball2014$SB), col="blue", lty=2, lwd=2)

###########  For Stolen Bases  ###########
avesb = subset(baseball2014, baseball2014$SB <= quantile(baseball2014$SB, .6 ) & baseball2014$SB > quantile(baseball2014$SB, .4 ))
vfsb = subset(baseball2014, baseball2014$SB > quantile(baseball2014$SB, .8 ))
vssb = subset(baseball2014, baseball2014$SB < quantile(baseball2014$SB, .2 ))
ssb = subset(baseball2014, baseball2014$SB <= quantile(baseball2014$SB, .4 ) & baseball2014$SB > quantile(baseball2014$SB, .2 ))
fsb = subset(baseball2014, baseball2014$SB <= quantile(baseball2014$SB, .8 ) & baseball2014$SB > quantile(baseball2014$SB, .6 ))

sampfsb = fsb[sample( nrow(fsb) ,50), ]
sampavesb = avesb[sample( nrow(avesb), 50), ]
sampvfsb = vfsb[sample( nrow(vfsb), 50), ]
sampvssb = vssb[sample( nrow(vssb), 50), ]
sampssb = ssb[sample( nrow(ssb), 50), ]


boxplot(sampvssb$AVG, sampssb$AVG, sampavesb$AVG, sampfsb$AVG, sampvfsb$AVG, names=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"), main="Comparing Sample BA Means per Stolen Bases", col="#33669925" )
abline(h=mean(baseball2014$AVG), v=mean(baseball2014$SB), col="blue", lty=2, lwd=2)

poslist = tapply(baseball2014$AVG, baseball2014$Pos, mean)
barplot(poslist, ylim=c(.24, .31), col="lightblue", main="Batting Average per Position")
abline(h=mean(baseball2014$AVG), col="blue", lty=2, lwd=2)

sampvfspd [ sampvfspd$AB > 400 & sampvfspd$AVG > .280, ]

tapply(baseball2014$AVG, baseball2014$Pos, describe)

boxplot(baseball2014$SB ~ baseball2014$Pos, main="Stolen Bases by Position", col="#33669925")
abline(h=mean(baseball2014$SB), col="blue", lty=2, lwd=2)
boxplot(baseball2014$HR ~ baseball2014$Pos, main="Home Run by Position", col="#33669925")
abline(h=mean(baseball2014$HR), col="blue", lty=2, lwd=2)
boxplot(baseball2014$AVG ~ baseball2014$Pos, main="Batting Average by Position", col="#33669925")
abline(h=mean(baseball2014$AVG), col="blue", lty=2, lwd=2)
boxplot(baseball2014$RBI ~ baseball2014$Pos, main="RBIs by Position", col="#33669925")
abline(h=mean(baseball2014$RBI), col="blue", lty=2, lwd=2)
boxplot(baseball2014$SLG ~ baseball2014$Pos, main="Sluggin Percentage by Position", col="#33669925")
abline(h=mean(baseball2014$SLG), col="blue", lty=2, lwd=2)

baseball2014[ baseball2014$HR > 20 & baseball2014$Pos == "OF" & baseball2014$AVG > .27, ]


baseball2014$Color[baseball2014$Pos == "1B"] = "red"
baseball2014$Color[baseball2014$Pos == "2B"] = "blue"
baseball2014$Color[baseball2014$Pos == "3B"] = "green"
baseball2014$Color[baseball2014$Pos == "SS"] = "yellow"
baseball2014$Color[baseball2014$Pos == "OF"] = "brown"
baseball2014$Color[baseball2014$Pos == "C"] = "purple"

dotchart(baseball2014$HR, group = baseball2014$Pos, color=baseball2014$Color, cex=.7)

############## ANOVA for SPD  ###########

### add column and initialize to NA
baseball2014["SPD"] = NA

### create loop to categorize speed into fifths (Very Slow, Slow, Average, Fast, Very Fast)

for (i in 1:nrow(baseball2014))
{
  if ((baseball2014[i , "SBAB"] <= quantile(baseball2014$SBAB, .6 )) & (baseball2014[i , "SBAB"] > quantile(baseball2014$SBAB, .4 )))
    {
      baseball2014[i, "SPD"] = "Average"
    }
  else if ((baseball2014[i , "SBAB"] <= quantile(baseball2014$SBAB, .4 )) & (baseball2014[i , "SBAB"] > quantile(baseball2014$SBAB, .2 )))
    {
      baseball2014[i, "SPD"] = "Slow"
    }
  else if ((baseball2014[i , "SBAB"] <= quantile(baseball2014$SBAB, .8 )) & (baseball2014[i , "SBAB"] > quantile(baseball2014$SBAB, .6 )))
    {
      baseball2014[i, "SPD"] = "Fast"
    }
  else if (baseball2014[i , "SBAB"] < quantile(baseball2014$SBAB, .2 ))
   {
      baseball2014[i, "SPD"] = "Very Slow"
    }
  else 
    {
      baseball2014[i, "SPD"] = "Very Fast"
    }
}

################## Offensive Production Index  ###############
#  
# This offensive index is in lieu of runs created since there is not data to use that stat.
# In other words, it is an index that approximates a comprehensive offensive production value
#
# OPI = Offensive Production * Opportunity 
# OPI = sqrt((R + HR + RBI) * (sqrt (AB) * (AVG + OBP + SLG) / 3))
#
#

baseball2014["OPI"] = NA

for (i in 1:nrow(baseball2014))
{
  if (baseball2014[ i , "AB"] > 50)
    baseball2014[i, "OPI"] = sqrt((baseball2014[i, "R"] + baseball2014[i, "RBI"] + baseball2014[i, "HR"]) * (sqrt(baseball2014[i, "AB"]) * (baseball2014[i, "AVG"] + baseball2014[i, "OBP"] + baseball2014[i, "SLG"]) / 3 ))
  else
    baseball2014[i, "OPI"] = NA
}

isOPI = is.na(baseball2014$OPI)
count = count[!isOPI]
length(count)

 
OPItop50 = subset(baseball2014, baseball2014$OPI > quantile(baseball2014$OPI, .1, na.rm=T) )
OPIlist = OPItop50[order(-OPItop50$OPI), ]
View(OPIlist)

boxplot(baseball2014$OPI ~ baseball2014$SPD, main="OPI by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$OPI, na.rm=T), col="blue", lty=2, lwd=2)

boxplot(OPItop50$OPI ~ OPItop50$SPD, main="OPI by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(OPItop50$OPI, na.rm=T), col="blue", lty=2, lwd=2)

hist(baseball2014[ , "OPI"])
describe(baseball2014[, "OPI"])
hist(baseball2014[ , "AB"])

qqnorm(baseball2014[, "OPI"], main="Normal Q-Q Plot for OPI")
qqline(baseball2014[, "OPI"])

#### Side by Side plots and ANOVA for AVG, RBI, and HR

boxplot(baseball2014$AVG ~ baseball2014$SPD, main="Batting Average by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$AVG), col="blue", lty=2, lwd=2)

aov.avg = aov(baseball2014$AVG ~ baseball2014$SPD, baseball2014)
summary(aov.avg)
model.tables(aov.avg, "means")

boxplot(baseball2014$RBI ~ baseball2014$SPD, main="RBI by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$RBI), col="blue", lty=2, lwd=2)

aov.rbi = aov(baseball2014$RBI ~ baseball2014$SPD, baseball2014)
summary(aov.rbi)
model.tables(aov.rbi, "means")

boxplot(baseball2014$HR ~ baseball2014$SPD, main="HR by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$HR), col="blue", lty=2, lwd=2)

aov.hr = aov(baseball2014$HR ~ baseball2014$SPD, baseball2014)
summary(aov.hr)
model.tables(aov.hr, "means")

boxplot(baseball2014$SLG ~ baseball2014$SPD, main="SLG by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$SLG), col="blue", lty=2, lwd=2)

aov.slg = aov(baseball2014$SLG ~ baseball2014$SPD, baseball2014)
summary(aov.slg)
model.tables(aov.slg, "means")

boxplot(baseball2014$SB ~ baseball2014$SPD, main="SB by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$SB), col="blue", lty=2, lwd=2)

##### plot the normal curve 
qqnorm(baseball2014[baseball2014$SPD == "Average", "HR"], main="Normal Q-Q Plot for HR--Average SPD")
qqline(baseball2014[baseball2014$SPD == "Average", "HR"])
qqnorm(baseball2014[baseball2014$SPD == "Very Slow", "HR"], main="Normal Q-Q Plot for HR--Very Slow SPD")
qqline(baseball2014[baseball2014$SPD == "Very Slow", "HR"])
qqnorm(baseball2014[baseball2014$SPD == "Very Fast", "HR"], main="Normal Q-Q Plot for HR--Very Fast SPD")
qqline(baseball2014[baseball2014$SPD == "Very Fast", "HR"])
qqnorm(baseball2014[baseball2014$SPD == "Slow", "HR"], main="Normal Q-Q Plot for HR--Slow SPD")
qqline(baseball2014[baseball2014$SPD == "Slow", "HR"])
qqnorm(baseball2014[baseball2014$SPD == "Fast", "HR"], main="Normal Q-Q Plot for HR--Fast SPD")
qqline(baseball2014[baseball2014$SPD == "Fast", "HR"])


qqnorm(baseball2014[baseball2014$SPD == "Average", "AVG"], main="Normal Q-Q Plot for AVG--Average SPD")
qqline(baseball2014[baseball2014$SPD == "Average", "AVG"])
qqnorm(baseball2014[baseball2014$SPD == "Very Slow", "AVG"], main="Normal Q-Q Plot for AVG--Very Slow SPD")
qqline(baseball2014[baseball2014$SPD == "Very Slow", "AVG"])
qqnorm(baseball2014[baseball2014$SPD == "Very Fast", "AVG"], main="Normal Q-Q Plot for AVG--Very Fast SPD")
qqline(baseball2014[baseball2014$SPD == "Very Fast", "AVG"])
qqnorm(baseball2014[baseball2014$SPD == "Slow", "AVG"], main="Normal Q-Q Plot for AVG--Slow SPD")
qqline(baseball2014[baseball2014$SPD == "Slow", "AVG"])
qqnorm(baseball2014[baseball2014$SPD == "Fast", "AVG"], main="Normal Q-Q Plot for AVG--Fast SPD")
qqline(baseball2014[baseball2014$SPD == "Fast", "AVG"])

qqnorm(baseball2014[baseball2014$SPD == "Average", "RBI"], main="Normal Q-Q Plot for RBI--Average SPD")
qqline(baseball2014[baseball2014$SPD == "Average", "RBI"])
qqnorm(baseball2014[baseball2014$SPD == "Very Slow", "RBI"], main="Normal Q-Q Plot for RBI--Very Slow SPD")
qqline(baseball2014[baseball2014$SPD == "Very Slow", "RBI"])
qqnorm(baseball2014[baseball2014$SPD == "Very Fast", "RBI"], main="Normal Q-Q Plot for RBI--Very Fast SPD")
qqline(baseball2014[baseball2014$SPD == "Very Fast", "RBI"])
qqnorm(baseball2014[baseball2014$SPD == "Slow", "RBI"], main="Normal Q-Q Plot for RBI--Slow SPD")
qqline(baseball2014[baseball2014$SPD == "Slow", "RBI"])
qqnorm(baseball2014[baseball2014$SPD == "Fast", "RBI"], main="Normal Q-Q Plot for RBI--Fast SPD")
qqline(baseball2014[baseball2014$SPD == "Fast", "RBI"])


### some summary stats using describe for categories

by(baseball2014$AVG, baseball2014$SPD, describe)
by(baseball2014$RBI, baseball2014$SPD, describe)
by(baseball2014$HR, baseball2014$SPD, describe)
by(baseball2014$SLG, baseball2014$SPD, describe)

describe(baseball2014[baseball2014$SPD == "Average", "HR" ])
##var   n  mean   sd median trimmed  mad min max range skew kurtosis   se
##1   1 112 10.29 8.79      7    9.18 7.41   0  33    33 0.96     -0.1 0.83
describe(baseball2014[baseball2014$SPD == "Very Fast", "HR" ])
##var   n mean   sd median trimmed  mad min max range skew kurtosis   se
##1   1 112 7.62 7.07      5    6.54 4.45   0  29    29 1.21     0.77 0.67
describe(baseball2014[baseball2014$SPD == "Very Slow", "HR" ])
##var   n mean   sd median trimmed   mad min max range skew kurtosis   se
##1   1 113 9.54 7.89      9    8.84 10.38   0  29    29 0.57    -0.76 0.74

########### correlation calcs ##########
#### HR to SLG
corHRSLG = cor(baseball2014$HR , baseball2014$SLG)
plot(baseball2014$HR, baseball2014$SLG, main="HR to SLG", xlab="Home Runs\n", ylab="SLGs", sub=c("Correlation Coeffecient: \n", corHRSLG), cex.sub=.8, cex.col="red")

par(mfrow=c(1,3))
####### AVG to RBI
corAVGRBI = cor(baseball2014$AVG , baseball2014$RBI)
plot(baseball2014$AVG, baseball2014$RBI, main="AVG to RBI", xlab="Batting Average\n", ylab="RBIs", sub=c("Correlation Coeffecient: \n", corAVGRBI), cex.sub=.8, col.sub="red")
abline(lm(baseball2014$RBI ~ baseball2014$AVG), col="red")
####### HR to RBI
corHRRBI = cor(baseball2014$HR , baseball2014$RBI)
plot(baseball2014$HR, baseball2014$RBI, main="HR to RBI", xlab="Home Runs\n", ylab="RBIs", sub=c("Correlation Coeffecient: \n", corHRRBI), cex.sub=.8, col.sub="red")
abline(lm(baseball2014$RBI ~ baseball2014$HR), col="red")
####### SLG to RBI
corSLGRBI = cor(baseball2014$RBI , baseball2014$SLG)
plot(baseball2014$SLG, baseball2014$RBI, main="RBI to SLG", xlab="Slugging Percentage\n", ylab="RBIs", sub=c("Correlation Coeffecient: \n", corSLGRBI), cex.sub=.8, col.sub="red")
abline(lm(baseball2014$RBI ~ baseball2014$SLG), col="red")

par(mfrow=c(1,3))
plot_ss(x = baseball2014$HR, baseball2014$RBI, leastSquares=TRUE, showSquares = TRUE)
plot_ss(x = baseball2014$SLG, baseball2014$RBI, leastSquares=TRUE, showSquares = TRUE)
plot_ss(x = baseball2014$AVG, baseball2014$RBI, leastSquares=TRUE, showSquares = TRUE)
par(orig)

summary(lm(baseball2014$RBI ~ baseball2014$SLG ))
summary(lm(baseball2014$RBI ~ baseball2014$AVG ))
summary(lm(baseball2014$RBI ~ baseball2014$HR ))

########################  Bootstrapping Stolen Bases ##############
## stolen base distribution
hist(baseball2014$SB, main="Stolen Base Distribution")
stripchart(baseball2014$SB, method="stack", at=0, pch=1, main="Stolen Base Distribution")
qqnorm(baseball2014$SB, main="Stolen Base Normal Q-Q Distribution")
qqline(baseball2014$SB)

###initial list of means
sampSB.means.OF = rep(NA, 1000)

for (i in 1:1000)  ####  OF
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$Pos == "OF" , "SB"], 10, replace=T)
  sampSB.means.OF[i] = mean(sampSB)
}

hist(sampSB.means.OF, main="Stolen Base Normal Distribution with Bootstrapping -- OF")
stripchart(sampSB.means.OF, method="stack", at=0, pch=19, main="Stolen Base Normal Distribution with Bootstrapping -- OF")
qqnorm(sampSB.means.OF, main="Stolen Base Normal Q-Q Distribution with Bootstrapping - OF")
qqline(sampSB.means.OF)

###initial list of means
sampSB.means.SS = rep(NA, 1000)

for (i in 1:1000)   ####  SS
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$Pos == "SS" , "SB"], 10, replace=T)
  sampSB.means.SS[i] = mean(sampSB)
}

hist(sampSB.means.SS, main="Stolen Base Normal Distribution with Bootstrapping -- SS")
stripchart(sampSB.means.SS, method="stack", at=0, pch=19, main="Stolen Base Normal Distribution with Bootstrapping -- SS")
qqnorm(sampSB.means.SS, main="Stolen Base Normal Q-Q Distribution with Bootstrapping -- SS")
qqline(sampSB.means.SS)

###initial list of means
sampSB.means.1B = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$Pos == "1B" , "SB"], 10, replace=T)
  sampSB.means.1B[i] = mean(sampSB)
}

hist(sampSB.means.1B, main="Stolen Base Normal Distribution with Bootstrapping -- 1B")
stripchart(sampSB.means.1B, method="stack", at=0, pch=19, main="Stolen Base Normal Distribution with Bootstrapping -- 1B")
qqnorm(sampSB.means.1B, main="Stolen Base Normal Q-Q Distribution with Bootstrapping -- 1B")
qqline(sampSB.means.1B)


###initial list of means
sampSB.means.3B = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$Pos == "3B" , "SB"], 10, replace=T)
  sampSB.means.3B[i] = mean(sampSB)
}

hist(sampSB.means.3B, main="Stolen Base Normal Distribution with Bootstrapping -- 3B")
stripchart(sampSB.means.3B, method="stack", at=0, pch=19, main="Stolen Base Normal Distribution with Bootstrapping -- 3B")
qqnorm(sampSB.means.3B, main="Stolen Base Normal Q-Q Distribution with Bootstrapping -- 3B")
qqline(sampSB.means.3B)

###initial list of means
sampSB.means.C = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$Pos == "C" , "SB"], 10, replace=T)
  sampSB.means.C[i] = mean(sampSB)
}

hist(sampSB.means.C, main="Stolen Base Normal Distribution with Bootstrapping -- C")
stripchart(sampSB.means.C, method="stack", at=0, pch=19, main="Stolen Base Normal Distribution with Bootstrapping -- C")
qqnorm(sampSB.means.C, main="Stolen Base Normal Q-Q Distribution with Bootstrapping -- C")
qqline(sampSB.means.C)

boxplot(baseball2014[baseball2014$Pos == "OF" , "SB"], baseball2014[baseball2014$Pos == "SS" , "SB"], baseball2014[baseball2014$Pos == "1B" , "SB"], baseball2014[baseball2014$Pos == "3B" , "SB"], baseball2014[baseball2014$Pos == "C" , "SB"], main="Comparison of SB by Pos", names=c("OF", "SS", "1B", "3B", "C"))
boxplot(sampSB.means.OF, sampSB.means.SS, sampSB.means.1B, sampSB.means.3B, sampSB.means.C, main="Bootstrap comparison of SB by Pos", names=c("OF", "SS", "1B", "3B", "C"))

######### Bootstrapping offensive categories by speed rating #####

###initial list of means
sampSB.means.vs = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$SPD == "Very Slow" , "HR"], 10, replace=T)
  sampSB.means.vs[i] = mean(sampSB)
}

hist(sampSB.means.vs, main="HR Normal Distribution with Bootstrapping -- Very Slow")
stripchart(sampSB.means.vs, method="stack", at=0, pch=19, main="HR Normal Distribution with Bootstrapping -- Very Slow")
qqnorm(sampSB.means.vs, main="HR Normal Distribution with Bootstrapping -- Very Slow")
qqline(sampSB.means.vs)

###initial list of means
sampSB.means.s = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$SPD == "Very Slow" , "HR"], 10, replace=T)
  sampSB.means.s[i] = mean(sampSB)
}

hist(sampSB.means.s, main="HR Normal Distribution with Bootstrapping -- Slow")
stripchart(sampSB.means.s, method="stack", at=0, pch=19, main="HR Normal Distribution with Bootstrapping -- Slow")
qqnorm(sampSB.means.s, main="HR Normal Distribution with Bootstrapping -- Slow")
qqline(sampSB.means.s)

###initial list of means
sampSB.means.ave = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$SPD == "Average" , "HR"], 10, replace=T)
  sampSB.means.ave[i] = mean(sampSB)
}

hist(sampSB.means.ave, main="HR Normal Distribution with Bootstrapping -- Average")
stripchart(sampSB.means.ave, method="stack", at=0, pch=19, main="HR Normal Distribution with Bootstrapping -- Average")
qqnorm(sampSB.means.ave, main="HR Normal Distribution with Bootstrapping -- Average")
qqline(sampSB.means.ave)

###initial list of means
sampSB.means.f = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$SPD == "Fast" , "HR"], 10, replace=T)
  sampSB.means.f[i] = mean(sampSB)
}

hist(sampSB.means.f, main="HR Normal Distribution with Bootstrapping -- Fast")
stripchart(sampSB.means.f, method="stack", at=0, pch=19, main="HR Normal Distribution with Bootstrapping -- Fast")
qqnorm(sampSB.means.f, main="HR Normal Distribution with Bootstrapping -- Fast")
qqline(sampSB.means.f)

###initial list of means
sampSB.means.vf = rep(NA, 1000)

for (i in 1:1000)   ####  1B
{
  ### random sample  --  with replacement
  sampSB = sample(baseball2014[baseball2014$SPD == "Very Fast" , "HR"], 10, replace=T)
  sampSB.means.vf[i] = mean(sampSB)
}

hist(sampSB.means.vf, main="HR Normal Distribution with Bootstrapping -- Very Fast")
stripchart(sampSB.means.vf, method="stack", at=0, pch=19, main="HR Normal Distribution with Bootstrapping -- Very Fast")
qqnorm(sampSB.means.vf, main="HR Normal Distribution with Bootstrapping -- Very Fast")
qqline(sampSB.means.vf)

boxplot(sampSB.means.vs, sampSB.means.s, sampSB.means.ave, sampSB.means.f, sampSB.means.vf, main="Bootstrap comparison of HR by Speed", names=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"))

list.spd.hr = c(sampSB.means.vs, sampSB.means.s, sampSB.means.ave, sampSB.means.f, sampSB.means.vf)

################## Log transformation for HR values (to try and fix skewness) ########

basball2014["fnHR"] = NA

for (i in 1:nrow(baseball2014))
{
  baseball2014[i, "fnHR"] = sqrt(baseball2014[i, "HR"])
}


boxplot(baseball2014$fnHR ~ baseball2014$SPD, main="HR(sqrt) by Speed Rating", col="#33669925", at=c(0, 1, -1, 2, -2))
abline(h=mean(baseball2014$fnHR), col="blue", lty=2, lwd=2)

################# Proportions of Speed per Offensive Category ##################

spdCatHR = factor(NA)

for (i in 1:nrow(baseball2014))
{
  if (baseball2014[i, "HR"] >= quantile(baseball2014[ , "HR"], .75))
  {
    spdCatHR = c(spdCatHR, baseball2014[i, "SPD"])
  }
}
spdCatHR = factor(spdCatHR, levels=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"))
barplot(table(spdCatHR), col="lightblue", main="Speed Proportions per HR top 75%")
prop.table(table(spdCatHR))

spdCatRBI = factor(NA)

for (i in 1:nrow(baseball2014))
{
  if (baseball2014[i, "RBI"] >= quantile(baseball2014[ , "RBI"], .75))
  {
    spdCatRBI = c(spdCatRBI, baseball2014[i, "SPD"])
  }
}
spdCatRBI = factor(spdCatRBI, levels=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"))
barplot(table(spdCatRBI), col="lightblue", main="Speed Proportions per RBI top 75%")
prop.table(table(spdCatRBI))

spdCatSLG = factor(NA)

for (i in 1:nrow(baseball2014))
{
  if (baseball2014[i, "SLG"] >= quantile(baseball2014[ , "SLG"], .75))
  {
    spdCatSLG = c(spdCatSLG, baseball2014[i, "SPD"])
  }
}
spdCatSLG = factor(spdCatSLG, levels=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"))
barplot(table(spdCatSLG), col="lightblue", main="Speed Proportions per SLG top 75%")
prop.table(table(spdCatSLG))

spdCatAVG = factor(NA)

for (i in 1:nrow(baseball2014))
{
  if (baseball2014[i, "AVG"] >= quantile(baseball2014[ , "AVG"], .75))
  {
    spdCatAVG = c(spdCatAVG, baseball2014[i, "SPD"])
  }
}
spdCatAVG = factor(spdCatAVG, levels=c("Very Slow", "Slow", "Average", "Fast", "Very Fast"))
barplot(table(spdCatAVG), col="lightblue", main="Speed Proportions per AVG top 75%")
spdCatAVG.prop = prop.table(table(spdCatAVG))



par(mfrow=c(2,2))
barplot(table(spdCatAVG), col="lightblue", main="Speed Proportions per AVG top 75%")
barplot(table(spdCatSLG), col="lightblue", main="Speed Proportions per SLG top 75%")
barplot(table(spdCatRBI), col="lightblue", main="Speed Proportions per RBI top 75%")
barplot(table(spdCatHR), col="lightblue", main="Speed Proportions per HR top 75%")


################# Barplots for all offensive stats top 75% ######

spdMat = matrix(c(table(spdCatHR), table(spdCatSLG), table(spdCatAVG), table(spdCatRBI)), 4, 5, byrow=T)
dimnames(spdMat) = list(c("HR", "SLG", "AVG", "RBI"), c("Very Slow", "Slow", "Average", "Fast", "Very Fast"))
table(spdCatHR)
spdMat

par(orig)
barplot(spdMat, beside=T, main="Speed Proportions per HR, SLG, AVG, RBI top 75%", legend=c("HR", "SLG", "AVG", "RBI"), args.legend=list(x=28, y=35, bty="n"), col=c("lightblue", "dodgerblue", "mediumblue", "navy"))

################# Inference function   #############

source("http://bit.ly/dasi_inference")

inference(y = baseball2014$OPI, x = baseball2014$SPD, est = "mean", type = "ht", null = 0, 
          alternative = "greater", method = "theoretical")


##################################### Pitcher Analysis  ##########################
pitchers2014 <- read.csv("~/Documents/coursera/dataanainf/project/pitchers.csv")
View(pitchers2014)
pitchers2014 = pitchers2014[complete.cases(pitchers2014), ]
boxplot(pitchers2014$WHIP ~ pitchers2014$FTEAM, main="WHIP")
abline(h=mean(pitchers2014$WHIP), col="blue")
boxplot(pitchers2014$ERA ~ pitchers2014$FTEAM, main="ERA")
abline(h=mean(pitchers2014$ERA), col="blue")
boxplot(pitchers2014$K_9 ~ pitchers2014$FTEAM, main="K per 9")
abline(h=mean(pitchers2014$K_9), col="blue")
boxplot(pitchers2014$K_BB ~ pitchers2014$FTEAM, main="K per BB")
abline(h=mean(pitchers2014$K_BB), col="blue")
boxplot(pitchers2014$OBA ~ pitchers2014$FTEAM, main="K per BB")
abline(h=mean(pitchers2014$OBA), col="blue")

boxplot(pitchers2014$QS ~ pitchers2014$FTEAM, main="K per BB")
abline(h=mean(pitchers2014$QS), col="blue")

tapply(pitchers2014$ERA, pitchers2014$FTEAM, mean)
tapply(pitchers2014$WHIP, pitchers2014$FTEAM, mean)
tapply(pitchers2014$K_9, pitchers2014$FTEAM, mean)

boxplot(scale_pitchers$WHIP ~ scale_pitchers$POS + scale_pitchers$FTEAM)
abline(h=mean(scale_pitchers[ scale_pitchers$POS == "RP", "WHIP"]), col="green")
abline(h=mean(scale_pitchers[ scale_pitchers$POS == "SP", "WHIP"]), col="red")


#################Summary lm(fit)  ##############

summary(lm(baseball2014$HR ~ baseball2014$SB))
summary(lm(baseball2014$RBI ~ baseball2014$SB))
summary(lm(baseball2014$SLG ~ baseball2014$SB))
summary(lm(baseball2014$AVG ~ baseball2014$SB))

###############ANOVA   
aov.whip = aov(pitchers2014$WHIP ~ pitchers2014$FTEAM, pitchers2014)
summary(aov.whip)
model.tables(aov.whip, "means")
model.tables(aov.whip, "effects")

aov.whip.rp.sp = manova(pitchers2014$WHIP~pitchers2014$FTEAM * pitchers2014$POS, pitchers2014)
summary(aov.whip.rp.sp)
model.tables(aov.whip.rp.sp, "mean")
