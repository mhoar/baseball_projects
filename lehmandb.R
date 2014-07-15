library(ggplot2)
setwd("/Users/mark/Documents/R/")

### Download zip file or csv files from http://www.seanlahman.com/baseball-archive/statistics/
#### to interactively choose csv fiel...  filename <- file.choose() 

ldb.master <- read.csv("./data/lahman2014/Master.csv", header=TRUE)
ldb.fielding <- read.csv("./data/lahman2014/Fielding.csv", header=TRUE)
ldb.batting <- read.csv("./data/lahman2014/Batting.csv", header=TRUE)
ldb.pitching <- read.csv("./data/lahman2014/Pitching.csv", header=TRUE)
ldb.team <- read.csv("./data/lahman2014/Teams.csv", header=TRUE)
ldb.salary <- read.csv("./data/lahman2014/Salaries.csv", header=TRUE)

head(ldb.master)
names(ldb.master)
tail(ldb.batting)
dim(ldb.pitching)

batting <- merge(ldb.batting, ldb.master, by = "playerID")
View(batting)
pitching <- merge(ldb.pitching, ldb.master, by = "playerID")
fielding <- merge(ldb.fielding, ldb.master, by = "playerID")

print(object.size(batting), units="Mb")
# 23.9 Mb
print(object.size(pitching), units="Mb")
# 14.7 Mb
print(object.size(fielding), units="Mb")
# 32.5 Mb

# bat <- batting[ , -c(39:47)]
# bat <- bat[ , -c(24:36)]
# View(bat)
# batting <- bat
# rm(bat)

#Get rid of No Hits 
batting <- batting[!(is.na(batting$AB)) & batting$AB > 0 , ]

###### Slashing Stats to add 
batting["BA"] <- batting[,"H"]/batting[,"AB"]

###    OBP= (H+BB+HBP) / (AB+BB+HBP+SF)
batting["OBP"] <- (batting[, "H"] + batting[, "BB"] + batting[, "HBP"]) / (batting[, "AB"] + batting[, "BB"] + batting[, "HBP"] + batting[, "SF"])

###### SLG=(H+2B+2∗3B+3∗HR)/(AB)
batting["SLG"] <- (batting[, "H"] + batting[, "X2B"] + 2 * batting[, "X3B"] + 3 * batting[ ,"HR"]) / (batting[, "AB"])

##### OPS = OBP + SLG
batting["OPS"] <- batting[ ,"OBP"] + batting[ ,"SLG"]

batting[batting$BA > .350 & batting$AB > 500, c("nameFirst", "nameLast", "BA", "yearID")]

batting[ batting$playerID == "hundlni01" & batting$yearID >= 2012, c("yearID", "H", "BA", "SLG", "OPS", "BB", "AB", "SO") ]

ldb.team["BA"] <- ldb.team[ ,"H"]/ldb.team[ ,"AB"]
team.lm <- lm(ldb.team$R ~ ldb.team$BA)
summary.lm <- summary(team.lm)
plot(ldb.team$R ~ ldb.team$BA, sub=paste("R squared: ", summary.lm$adj.r.squared, sep=" "))
abline(team.lm, col="red")

ldb.team["OBP"] <- (ldb.team[ ,"H"] + ldb.team[ , "BB"] + ldb.team[ ,"HBP"]) / (ldb.team[ ,"SF"] + ldb.team[ , "BB"] + ldb.team[ ,"HBP"] + ldb.team[ ,"AB"])
team.lm <- lm(ldb.team$R ~ ldb.team$OBP)
summary.lm <- summary(team.lm)
plot(ldb.team$R ~ ldb.team$OBP, sub=paste("R squared: ", summary.lm$adj.r.squared, sep=" "))
abline(team.lm, col="red")

r.value <- cor(ldb.team$BA, ldb.team$R)
r.squared <- r.value ** 2
r.squared

#### Runs Created:  The formula: (H + BB + HBP - CS - GIDP) times (Total Bases + .26(TBB - IBB + HBP) + .52(SH + SF + SB)) divided by (AB + TBB + HBP + SH + SF).

batting["RC"] <-  (batting[ ,"H"] + batting[ ,"BB"] + batting[ ,"HBP"] - batting[ ,"CS"] - batting[ ,"GIDP"]) * ((batting[ ,"H"] - batting[ ,"X2B"] - batting[ ,"X3B"] - batting[ ,"HR"]) + 2 * batting[ ,"X2B"] + 3 * batting[ ,"X3B"] + 4 * batting[ ,"HR"] + .26*(batting[ ,"BB"] - batting[ ,"IBB"] + batting[ ,"HBP"]) + .52*(batting[ ,"SH"] + batting[ ,"SF"] + batting[ ,"SB"])) / (batting[ ,"AB"] + batting[ ,"BB"] + batting[ ,"HBP"] + batting[ ,"SH"] + batting[ ,"SF"])

rc.sum.player <- aggregate(RC ~ playerID, data=batting, sum)
hist(rc.sum.player$RC)
rc.sum.player <- rc.sum.player[order(rc.sum.player$RC, decreasing=T), ]
head(rc.sum.player, n=25)

team.total <- table(ldb.team$yearID)
team.total.list <- as.data.frame(team.total)
team.total.list <- team.total.list[ , 2]
team.sum.year <- as.data.frame(aggregate(ldb.team$R~ldb.team$yearID, ldb.team, sum))
team.sum.year <- cbind(team.sum.year, team.total.list)
colnames(team.sum.year) <- c("year", "runs", "teamcount")

### merge rpg with lbd.team to make plot by league

team.sum.year["R/T"] <- team.sum.year[ , "runs"] / team.sum.year[ , "teamcount"]
team.sum.year
plot(team.sum.year$R/T ~ team.sum.year$year)
abline(lm(team.sum.year$R/T ~ team.sum.year$year))

runs.team.per.game <- as.data.frame(aggregate((ldb.team$R/ldb.team$G) ~ ldb.team$yearID, ldb.team, mean))
colnames(runs.team.per.game) <- c("year", "rpg")
plot(runs.team.per.game$rpg ~ runs.team.per.game$year, ylab="Avg. Runs per Game", xlab="Annually for All Teams")
abline(lm(runs.team.per.game$rpg ~ runs.team.per.game$year))

### modern error adjustment year > 1900
runs.team.per.game <- runs.team.per.game[runs.team.per.game$year > 1900, ]
plot(runs.team.per.game$rpg ~ runs.team.per.game$year, ylab="Avg. Runs per Game", xlab="Annually for All Teams", main="Since 1900")
abline(h=mean(runs.team.per.game$rpg), col="red")

### merge rpg with lbd.team to make plot by league


runs.team.per.game <- as.data.frame(aggregate((ldb.team$R/ldb.team$G) ~ ldb.team$yearID + ldb.team$lgID, ldb.team, mean))
colnames(runs.team.per.game) <- c("yearID", "lgID", "rpg")
### Since DH ...  1973
runs.team.per.game <- runs.team.per.game[runs.team.per.game$yearID > 1972, ]

#ldb.team <- merge(ldb.team, runs.team.per.game, by.x="yearID", by.y="year")
#View(ldb.team)
View(runs.team.per.game)
runplot <- qplot(yearID, rpg, data=runs.team.per.game, color=lgID, main="Runs Per Game by League\n Since AL implemented DH in 1973", ylab="Runs per Game", xlab="Annual Average Runs Scored")
runplot + theme_bw()+geom_smooth(method=lm, se=FALSE, size=2) + geom_point(shape=3, size=4)


# Model Based Clustering
install.packages("mclust")
library(mclust)
fit <- Mclust(runs.team.per.game)
plot(fit) # plot results 
summary(fit) # display the best model

plot(ldb.team$HR ~ ldb.team$SO)
abline(lm(ldb.team$HR ~ ldb.team$SO))

plot(ldb.team$SB ~ ldb.team$yearID)
abline(lm(ldb.team$SB ~ ldb.team$yearID))

plot(ldb.team$H / ldb.team$AB ~ ldb.team$yearID)
abline(lm(ldb.team$H / ldb.team$AB ~ ldb.team$yearID), col="red")
boxplot(ldb.team$H / ldb.team$AB ~ ldb.team$yearID)

team.salary <- aggregate(ldb.salary$salary ~ ldb.salary$teamID + ldb.salary$yearID, ldb.salary, sum)


######## Working with teams in modern era yearID >= 1955

teams <- ldb.team[ldb.team$yearID >= 1955, ]

teams["BA"] <- teams$H / teams$AB
teams["TB"] <- teams$H + teams$X2B + 2 * teams$X3B + 3 * teams$HR 
teams["RCB"] <- (teams$TB * (teams$H + teams$BB)) / (teams$AB + teams$BB) ### Simple Runs Created
teams["SLG"] <- teams$TB / teams$AB
teams["OBP"] <- (teams$H + teams$BB + teams$HBP) / (teams$SF + teams$BB + teams$HBP + teams$AB)
teams["OPS"] <- teams$SLG + teams$OBP
teams["BRA"] <- teams$SLG * teams$OBP  ### Batter Run Average
teams["Adj_OPS"] <- teams$SLG + teams$OBP * 1.8  ###  Adjusted OPS weighted for average bases
teams["XRR"] <- ( .5 * (teams$H - teams$HR - teams$X3B - teams$X2B))  + ( .72 * teams$X2B ) + ( 1.04 * teams$X3B ) + ( 1.44 * teams$HR ) + .33 * ( teams$HBP + teams$BB ) + .18 * teams$SB - .32 * teams$CS - .098 * ( teams$AB - teams$H )        #### Jim Furtado's extrapolated runs
teams["LWTs"] <- ( .46 * (teams$H - teams$HR - teams$X3B - teams$X2B))  + ( .8 * teams$X2B ) + ( 1.02 * teams$X3B ) + ( 1.4 * teams$HR ) + .33 * ( teams$HBP + teams$BB ) + .3 * teams$SB - .6 * teams$CS - .25 * ( teams$AB - teams$H ) + 701.2        #### Pete Palmer's linear weights



write.csv(teams, file = "./data/mod_era_teams.csv")

runplot <- qplot(R, RCB, data=teams, main="Runs to RCB", ylab="Runs per Game", xlab="Annual Average Runs Scored") 
slgplot <- qplot(R, SLG, data=teams, main="Runs to SLG", ylab="Team SLG", xlab="Annual Average Runs Scored")
opsplot <- qplot(R, OPS, data=teams, main="Runs to OPS", ylab="Team OPS", xlab="Annual Average Runs Scored")
runplot + theme_bw()+geom_smooth(method=lm, se=FALSE, size=2) + geom_point(shape=3, size=4)
slgplot + theme_bw()+geom_smooth(method=lm, se=FALSE, size=2) + geom_point(shape=3, size=4)
opsplot + theme_bw()+geom_smooth(method=lm, se=FALSE, size=2) + geom_point(shape=3, size=4)

######## games from Retrosheet database 

game_statistics <- read.csv(file.choose())

attach(game_statistics)


    game_statistics$total_runs <- with(game_statistics, home_score + visitor_score)

   
    total_runs <- with(game_statistics, home_score + visitor_score)

game_statistics["total_runs"] <- total_runs
View(game_statistics)
total_time_data <- game_statistics[c('game_minutes', 'total_runs', 'outs')]

require('lattice')

splom(total_time_data)

cor(game_minutes, outs)

plot(total_runs, outs)

projected_minutes <- lm(game_minutes ~ total_runs + outs)
summary(projected_minutes)
slope(project_minutes)

game_statistics$RedSox_playing <- with(game_statistics, ifelse(home == 'BOS' | visitor == 'BOS', 1, 0))
RedSox_games <- game_statistics[game_statistics$RedSox_playing == 1,]

summary(RedSox_games)

rs_runs <- hist(RedSox_games$total_runs, breaks= 40)
max(rs_runs$counts)  ##### Highest frequency
 rs_runs$mids[which(rs_runs$counts == max(rs_runs$counts))]  ####Gets the value at highest count

game_statistics$BrianGorman <- with(game_statistics, ifelse(hp_ump_name == 'Brian Gorman', 1, 0))
game_statistics$JimJoyce <- with(game_statistics, ifelse(hp_ump_name == 'Jim Joyce', 1, 0))
game_statistics$DaleScott <- with(game_statistics, ifelse(hp_ump_name == 'Dale Scott', 1, 0))
game_statistics$TimWelke <- with(game_statistics, ifelse(hp_ump_name == 'Tim Welke', 1, 0))
###Brian Gorman, Jim Joyce, Dale Scott, and Tim Welke. 
BrianGorman_games <- game_statistics[game_statistics$BrianGorman == 1,]
JimJoyce_games <- game_statistics[game_statistics$JimJoyce == 1,]
DaleScott_games <- game_statistics[game_statistics$DaleScott == 1,]
TimWelke_games <- game_statistics[game_statistics$TimWelke == 1,]

summary(BrianGorman_games$total_runs)
summary(JimJoyce_games$total_runs)
summary(DaleScott_games$total_runs)
summary(TimWelke_games$total_runs)

projected_runs <- lm(game_statistics$total_runs ~ game_statistics$RedSox_playing + game_statistics$BrianGorman + game_statistics$JimJoyce + game_statistics$DaleScott + game_statistics$TimWelke)
projected_runs
confint(projected_runs, level=.95)

game_statistics[which(total_runs == max(total_runs)), ]

###  parkRuns <- list(by(game_statistics$total_runs, game_statistics$park, mean))
parkRuns <- aggregate(game_statistics$total_runs, list(game_statistics$park), mean)
parkRunsVisitors <- aggregate(game_statistics$visitor_score, list(game_statistics$park), mean)


detach(game_statistics)