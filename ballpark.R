#### Original source was taken from the retrosheet.org website.  The information used here was obtained free ofcharge from and is copyrighted by Retrosheet.  Interested parties may contact Retrosheet at "www.retrosheet.org".

#### SELECT visitor_ab, home_ab, visitor_hr, home_hr, visitor_h, home_h, visitor_2b, home_2b,
###  visitor_3b, home_3b, visitor_bb, home_bb, home, visitor, YEAR(date) as year
###  FROM games
###  WHERE YEAR(date) >= 2009

#### SELECT visitor_ab, home_ab, visitor_hr, home_hr, home, visitor FROM games
#### WHERE YEAR(date) = 2013
#### AND (visitor = 'COL' OR home = 'COL');

setwd("~/Documents/R")
rox <- read.csv('./data/coors_park_factors.csv')
View(rox)
park <- subset(rox, home == "COL")
away <- subset(rox, visitor == "COL")
park_ratio <- sum(park$home_hr + park$visitor_hr) / sum(park$home_ab + park$visitor_ab)
away_ratio <- sum(away$home_hr + away$visitor_hr) / sum(away$home_ab + away$visitor_ab)

rox.pf <- 100 * park_ratio / away_ratio

##### All Park Factors
allparks <- read.csv('./data/park_factors.csv')
View(allparks)

##### homerun park factors
calc.pf <- function(team, yr=2009){
    homepark <- allparks[(allparks$home==team) & (allparks$year==yr), ]
    awaypark <- allparks[(allparks$visitor==team) & (allparks$year==yr), ]
    hp.ratio <- sum(homepark$home_hr + homepark$visitor_hr) / sum(homepark$home_ab + homepark$visitor_ab)
    ap.ratio <- sum(awaypark$home_hr + awaypark$visitor_hr) / sum(awaypark$home_ab + awaypark$visitor_ab)
    
    return(team.pf <- 100 * hp.ratio / ap.ratio)
    
}
####### min doubles pf in 2010
calc.pf.2b <- function(team, yr=2010){
    homepark <- allparks[(allparks$home==team) & (allparks$year==yr), ]
    awaypark <- allparks[(allparks$visitor==team) & (allparks$year==yr), ]
    hp.ratio <- sum(homepark$home_2b + homepark$visitor_2b) / sum(homepark$home_ab + homepark$visitor_ab)
    ap.ratio <- sum(awaypark$home_2b + awaypark$visitor_2b) / sum(awaypark$home_ab + awaypark$visitor_ab)
    
    return(team.pf <- 100 * hp.ratio / ap.ratio)
    
}

teams <- as.character(levels(allparks[!duplicated(allparks$home) & allparks$year==2010, "home"]))

tlist <- as.vector(NA)
for (t in 1:length(teams))
{
    tlist[t] <- calc.pf.2b(teams[t])
}
list(teams[which(tlist == max(tlist, na.rm=T))], max(tlist, na.rm=T))

####### max bb pf in 2011
calc.pf.bb <- function(team, yr=2011){
    homepark <- allparks[(allparks$home==team) & (allparks$year==yr), ]
    awaypark <- allparks[(allparks$visitor==team) & (allparks$year==yr), ]
    hp.ratio <- sum(homepark$home_bb + homepark$visitor_bb) / sum(homepark$home_ab + homepark$visitor_ab)
    ap.ratio <- sum(awaypark$home_bb + awaypark$visitor_bb) / sum(awaypark$home_ab + awaypark$visitor_ab)
    
    return(team.pf <- 100 * hp.ratio / ap.ratio)
    
}

teams <- as.character(levels(allparks[!duplicated(allparks$home) & allparks$year==2010, "home"]))

tlist <- as.vector(NA)
for (t in 1:length(teams))
{
    tlist[t] <- calc.pf.bb(teams[t])
}
list(teams[which(tlist == max(tlist, na.rm=T))], max(tlist, na.rm=T))

