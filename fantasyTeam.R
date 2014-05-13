##### Web Scraping
##con = url("http://games.espn.go.com/flb/clubhouse?leagueId=200684&teamId=4&seasonId=2014")
##htm <- readLines(con)
##close(con)
##View(htm)

###### from downoloaded file
###pitchers <- read.csv("./data/pitcher4252014.csv")
###View(pitchers)

##### install stringr package to work with strings (primarily, the player name data)
install.packages("stringr")
library(stringr)

#### Use XML package to parse web page
install.packages("XML")
library(XML)

##player rater
##url <- "http://games.espn.go.com/flb/playerrater?leagueId=200684&teamId=44"
##html <- htmlTreeParse(url, useInternalNode=T)
##players <- as.vector(xpathSApply(html, "//td[@class='playertablePlayerName']", xmlValue))


### my team's stats
team.url <- "http://games.espn.go.com/flb/clubhouse?leagueId=200684&teamId=4&seasonId=2014&scoringPeriodId=41&view=stats&context=clubhouse&version=currSeason&ajaxPath=playertable/prebuilt/manageroster&managingIr=false&droppingPlayers=false&asLM=false"
team.html <- htmlTreeParse(team.url, useInternalNode=T)

team.stats.headers <- as.vector(xpathSApply(team.html, "//td[@class='playertableStat']", xmlValue))
View(team.stats.headers)
#### notice extra space below for player data in "playertableStat "
team.stats <- as.vector(xpathSApply(team.html, "//td[@class='playertableStat ']", xmlValue))
View(team.stats)

team.players <- as.vector(xpathSApply(team.html, "//td[@class='playertablePlayerName']", xmlValue))
View(team.players)

team.players.id <- as.vector(xpathSApply(team.html, "//playerID", xmlValue))
View(team.players.id)

#### get the team roster from team.players by filtering pitchers; count each set
team.batters <- team.players[!grepl("SP|RP", team.players)]
cbatters <- length(team.batters)

team.pitchers <- team.players[grepl("SP|RP", team.players)]
cpitchers <- length(team.players)

######## fix data
####### subset batter and pitcher data
batend <- grep("FPCT", team.stats.headers)
pitchbeg <- grep("^G$", team.stats.headers)
batter.header <- team.stats.headers[1:batend]
View(batter.header)
pitcher.header <- team.stats.headers[pitchbeg:length(team.stats.headers)]
class(pitcher.header)
batter.data <- team.stats[1:(cbatters * length(batter.header) )]
View(batter.data)
pitcher.data <- team.stats[(cbatters* length(pitcher.header) + 1):length(team.stats)]
####### convert to matrix to make numbers 
batter.data.matrix <- matrix(batter.data, nrow=cbatters, ncol=length(batter.header), byrow=T)
batter.data.matrix[ , 1]
####### split the hits and at bats  
habs <- matrix(unlist(strsplit(batter.data.matrix[ , 1 ], "/")), nrow=cbatters, ncol=2, byrow=T)
class(habs)
head(habs)
all.batter.matrix <- cbind(habs, batter.data.matrix[ , 2:length(batter.header)])

head(all.batter.matrix)

#### creater all batter headers
habs.header <- unlist(strsplit(batter.header[1], "/"))
batter.header <- batter.header[ 2:batend]
all.batter.header <- c(habs.header, batter.header)
rm(habs.header, batter.header, team.stats.headers, batter.data.matrix, habs, batter.data)

head(team.players)

player.item <- str_split(team.players[1], ", ")
player.item <- unlist(player.item)
player.item
player.name <- player.item[1]


###### make data frame for batters
batters <- data.frame(all.batter.matrix)
names(batters) <- all.batter.header
View(batters)


### some summary stats
team.whip <- (sum(H) + sum(BB)) / sum(IP)
team.IP <- sum(IP)
team.ERA <- (sum(ER) * 9) / team.IP
team.k9 <- (sum(K) * 9) / team.IP
team.K <- sum(K)
team.sums <- as.data.frame(colSums(pitchers[ , c(3:16)]))
team.kbb <- sum(K)/sum(BB)
