###### Data taken from league player rating from espn fantasy eague

####  http://games.espn.go.com/flb/playerrater?leagueId=200684&teamId=4&slotCategoryId=14

setwd("~/Documents/R")
pitchers <- read.csv('./data/pitchersMidTerm2014.csv')
View(rox)
pitchers[ 34, 5] <- "FA"
pitchers[ 35, 5] <- "TW"
pitchers[ 45, 5] <- "FA"
pitchers[ 29, 5] <- "TW"
pitchers[ 24, 5] <- "TSS"
pitchers[ 28, 5] <- "FA"
pitchers[ 22, 5] <- "FA"
pitchers[ 19, 5] <- "FA"
pitchers[ 15, 5] <- "FA"
pitchers[ 19, 5] <- "HBH"
colMeans(pitchers[ , 6:26])

hist(pitchers$TOTAL_2014, breaks=15)
abline(v=mean(pitchers$TOTAL_2014))
abline(v=mean(pitchers$TOTAL_2014), col = "blue")

xTotal <- rowSums(pitchers[ , c(6:13, 16:25)])
pitchers <- cbind(pitchers, xTotal)

hist(scale(pitchers$xTotal), breaks=15)

pitchers <- pitchers[order(pitchers$xTotal, decreasing=T), ]

