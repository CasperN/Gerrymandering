

### Boolean if a democrat won the 21st district in 2010

sum(x[x$pa_house_district == 21, 'uscdv2010'],na.rm=TRUE) >  sum(x[x$pa_house_district == 21, 'uscrv2010'],na.rm=TRUE)

length(sort(unique(x$pa_house_district)))
length(unique(x$pa_house_district))

sum(is.na(x$pa_house_district))

house = list()

for (i in 1:203){ # 203 districts in PA
  house[[i]] = list()
  house[[i]]$num_dem_votes = sum(x[x$pa_house_district == i, 'uscdv2010'],na.rm=TRUE)
  house[[i]]$num_rep_votes = sum(x[x$pa_house_district == i, 'uscrv2010'],na.rm=TRUE)
  house[[i]]$total_votes = house[[i]]$num_dem_votes + house[[i]]$num_rep_votes
  dem_win = (house[[i]]$num_dem_votes > house[[i]]$num_rep_votes)
  if(dem_win){
    house[[i]]$num_dem_wasted = house[[i]]$num_dem_votes - house[[i]]$total_votes/2
    house[[i]]$num_rep_wasted = house[[i]]$num_rep_votes
  } else {
    house[[i]]$num_rep_wasted = house[[i]]$num_rep_votes - house[[i]]$total_votes/2
    house[[i]]$num_dem_wasted = house[[i]]$num_dem_votes   
  }
}
