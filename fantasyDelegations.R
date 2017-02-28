library(dplyr)
library(tidyr)
library(ggplot2)



load("datafiles/pa_final.RData")
state <- x
rm(x)

voteshares <- read.csv("House_1898_2014_voteshares_notext.csv", header = FALSE) %>%
  tbl_df %>%
  rename(year = V1, state_code = V2, district = V3, d_voteshare = V4, incumbent = V5, winner = V6) %>%
  mutate(d_voteshare = ifelse(d_voteshare < 0, 0, ifelse(d_voteshare > 1, 1, d_voteshare)))

trial <- filter(voteshares, state_code == 43, year == 2004)

states = c('AL', 'AK', 'AZ', 'AR', 'CA',
           'CO', 'CT', 'DE', 'FL', 'GA',
           'HI', 'ID', 'IL', 'IN', 'IA', 
           'KS', 'KY', 'LA', 'ME', 'MD',
           'MA', 'MI', 'MN', 'MS', 'MO',
           'MT', 'NE', 'NV', 'NH', 'NJ',
           'NM', 'NY', 'NC', 'ND', 'OH',
           'OK', 'OR', 'PA', 'RI', 'SC',
           'SD', 'TN', 'TX', 'UT', 'VT',
           'VA', 'WA', 'WV', 'WI', 'WY')


fantasy = function(dem_votes,rep_votes,house_districts,d_voteshare){
  districts = sort(unique(house_districts))
  dem_seats = 0
  dem_share = sum(dem_votes,na.rm=TRUE) / sum(dem_votes,rep_votes,na.rm=TRUE)
  for(d in districts){
    dem_seats = dem_seats + (sum(dem_votes[house_districts == d],na.rm=TRUE) > sum(rep_votes[house_districts == d],na.rm=TRUE))
  }
  N = 100000
  s = matrix(sample(d_voteshare,length(districts)*N,replace=TRUE), nrow = N, ncol = length(districts))
  e = .01
  s_seats = rowSums(s > .5)
  s_share = rowMeans(s)
  similar = (abs(rowMeans(s)-dem_share) < .001)
  
  plot(s_share,s_seats)
  print(dem_seats)
  print(dem_share)
  points(dem_share,dem_seats,col = 'red',pch='x')
  expected   = mean(s_seats[similar])
  expected_dev = sd(s_seats[similar])
  
  points(dem_share,expected,col='green',pch = 'x')
  segments(dem_share,expected - expected_dev, y1 = expected + expected_dev,col='green')
}

pa_d_vs = voteshares$d_voteshare[voteshares$state_code==38]
fantasy(state$stsdv2008,state$stsrv2008,state$pa_sen_district,pa_d_vs)
fantasy(state$uscdv2008,state$uscrv2008,state$us_house_district,pa_d_vs)

sum( voteshares$d_voteshare[voteshares$state_code==38 & voteshares$year==2008]>-1 )

