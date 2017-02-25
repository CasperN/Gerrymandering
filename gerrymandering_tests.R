## NC Gerrymandering ##

hr2016 <- read.csv("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/2016_NC_election_summary.csv")
hr2014 <- read.csv("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/2014_NC_election_summary.csv")
hr2012 <- read.csv("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/2012_NC_election_summary.csv")
hr2010 <- read.csv("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/2010_NC_election_summary.csv")
hr2008 <- read.csv("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/2008_NC_election_summary.csv")
hr2006 <- read.csv("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/2006_NC_election_summary.csv")


efficiency.gap <- function (data) {
  rep.wasted <- 0
  dem.wasted <- 0
  for (race in levels(data$contest.name)){
    needed <- .5*sum(subset(data, (contest.name == race) & (party.name == 'DEM' | party.name == 'REP'))$total.votes)
    dem.votes <- subset(data, (contest.name == race) & (party.name == 'DEM'))$total.votes
    rep.votes <- subset(data, (contest.name == race) & (party.name == 'REP'))$total.votes
    if (dem.votes > rep.votes){
      rep.wasted <- rep.wasted + rep.votes
      dem.wasted <- dem.wasted + (dem.votes - needed)
    } else {
      rep.wasted <- rep.wasted + (rep.votes - needed)
      dem.wasted <- dem.wasted + dem.votes
    }
  }
  gap <- (dem.wasted - rep.wasted)/(dem.wasted + rep.wasted)
  return (gap)
}
pre <- c(efficiency.gap(hr2006),efficiency.gap(hr2008), efficiency.gap(hr2010))

post <- c(efficiency.gap(hr2012), efficiency.gap(hr2014), efficiency.gap(hr2016))
t.test(pre,post)

plot(seq(2006, 2016, 2), c(efficiency.gap(hr2006),efficiency.gap(hr2008), efficiency.gap(hr2010),
                           efficiency.gap(hr2012), efficiency.gap(hr2014), efficiency.gap(hr2016)),
     ylab = 'efficiency gap', xlab = 'year')



#---------------------------------------------------------------------------------------
## PA Gerrymandering ##

load("C:/Users/pgao/Dropbox/Winter 2017/Multiple Testing/pa_final.RData")
state <- x
rm(x)


summarize.helper <- function(race.col, d.vote.col, r.vote.col){
  election <- data.frame(race = levels(as.factor(race.col)))
  election$d <- integer(nrow(election))
  election$r <- integer(nrow(election))
  for (k in 1:nlevels(as.factor(race.col))){
    i <- levels(as.factor(race.col))[k]
    election$d[k] <- sum(d.vote.col[race.col == i], na.rm = T)
    election$r[k] <- sum(r.vote.col[race.col == i], na.rm = T)
    election$t[k] <- election$d[k] + election$r[k]
    election$d.p[k] <- election$d[k] / election$t[k]
    if (election$d[k] > election$r[k]) {
      election$d.waste[k] <- election$d[k] - election$t[k]/2
      election$r.waste[k] <- election$r[k]
    } else {
      election$r.waste[k] <- election$r[k] - election$t[k]/2
      election$d.waste[k] <- election$d[k]
    }
  }
  return (election)
}

efficiency.gap <- function(election){
  return(sum(election$r.waste - election$d.waste) / sum(election$d+election$r))
}

uniform.swing.bias <- function(election){
  total.d.prop <- sum(election$d) / (sum(election$d) + sum(election$r))
  swing.wins <- sum((election$d.p + (total.d.prop - 0.5)) > 0.5)
  return ((swing.wins / nrow(election)) - 0.5)
}


lopsided.outcomes <- function(election){
  d.margins <- election$d.waste[election$d > election$r]
  r.margins <- election$r.waste[election$d <= election$r]
  test <- t.test(d.margins, r.margins) 
  return(test$p.value)
}

mean.median <- function(election){
  mean.share <- mean(election$d.p)
  median.share <- median(election$d.p)
  sigma <- sqrt(var(election$d.p))/sqrt(nrow(election))
  delta <- (mean.share - median.share) / sigma
}

summarize.election <- function(race.col, d.vote.col, r.vote.col) {
  election <- summarize.helper(race.col, d.vote.col, r.vote.col)
  p.bias <- uniform.swing.bias(election)
  e.gap <- efficiency.gap(election)
  lop.p <- lopsided.outcomes(election)
  delta <- mean.median(election)
  return(list(election = election, p.bias = p.bias, e.gap = e.gap, lop.p = lop.p, delta = delta))
}


d.vote.cols <- cbind(state$uscdv2000, state$uscdv2002, state$uscdv2004, state$uscdv2006, state$uscdv2008, state$uscdv2010)
r.vote.cols <- cbind(state$uscrv2000, state$uscrv2002, state$uscrv2004, state$uscrv2006, state$uscrv2008, state$uscrv2010)
gaps <- c()
bias <- c()
lop.ps <- c()
deltas <- c()
for (i in 1:ncol(d.vote.cols)){
  summary <- summarize.election(state$us_house_district, d.vote.cols[, i], r.vote.cols[, i])
  gaps[i] <- summary$e.gap
  bias[i] <- summary$p.bias
  lop.ps[i] <- summary$lop.p
  deltas[i] <- summary$delta
}


