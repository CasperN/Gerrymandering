library(dplyr)
library(tidyr)
library(ggplot2)

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


# Test 1: This is a relatively straightforward t-test comparing the winning margins for Democrats and Republicans
# state is a two character string (e.g. 'TX'), req_year is the year to be analyzed, and uncontested_imputation
# is a value that represents the assumed winning vote percentage in districts that had uncontested races

t1_lopsided_margin <- function(voting_data, state, req_year, uncontested_imputation) {
  relevant_data <- filter(voting_data, state_code == which(states == state),
                          year == req_year)
  n_delegates <- dim(relevant_data)[1]
  d_districts <- filter(relevant_data, d_voteshare >= 0.5)
  r_districts <- filter(relevant_data, d_voteshare < 0.5)
  n_d <- sum(relevant_data$d_voteshare >= 0.5)
  n_r <- n_delegates - n_d
  
  # If there were any uncontested districts, replace the voteshares with the imputed value or its inverse
  
  imputed_data <- relevant_data %>%
    mutate(d_voteshare = ifelse(d_voteshare == 1, uncontested_imputation,
                                ifelse(d_voteshare == 0, 1 - uncontested_imputation, d_voteshare)))
  
  raw_d_mean <- mean(relevant_data$d_voteshare)
  raw_r_mean <- 1 - raw_d_mean
  
  d_mean <- mean(imputed_data$d_voteshare)
  r_mean <- 1 - d_mean
  
  # Print some preliminary information about the relevant data
  
  print(sprintf("The %s state delegation of %i had %i seats, %i Democratic/other and %i Republican",
          state, req_year, n_delegates, n_d, n_r))
  
  print(sprintf("The average Democratic share of the two-party total vote was %f%%",
          raw_d_mean*100))
  
  if (d_mean != raw_d_mean) {
    print(sprintf("With the input imputation value of %f, the average Democratic share was instead %f%%",
            uncontested_imputation, d_mean*100))
  }
  
  # Print the general idea behind this test
  
  print("If a political party wishes to create for itself an advantage, it will pack its opponents to win
        overwhelmingly in a small number of districts, while distributing its own votes more thinly.
        To test for a lopsided advantage, one can compare the winning margins of each party and see if they are
        systematically different using a two-sample t-test. In this test, the party with the smaller winning
        margins has the advantage.")
  
  # Perform the t-test
  
  d_winvotes <- d_districts$d_voteshare 
  r_winvotes <- 1 - r_districts$d_voteshare
  
  dems <- cbind(vote = round(d_winvotes,1), party = 'Democrat')
  reps <- cbind(vote = round(r_winvotes,1), party = 'Republican')
  
  winvotes <- as.data.frame(rbind(dems, reps)) 
  
  p <- ggplot(winvotes, aes(x = party, y = vote, color = party)) + geom_jitter(position = position_jitter(0.001))
  
  p <- p + coord_flip() + scale_color_manual(values = c('blue', 'red')) 
  
  p <- p + labs(title = 'Vote Proportion in Wins', y = 'Vote Proportion', x = 'Party')
  
  print(p)
  
  if (n_d >= 2 && n_r >= 2) {
    t_results = t.test(d_winvotes, r_winvotes, alternative = 'two.sided',
                       var.equal = FALSE)
    
    p_val <- t_results$p.value
    
    if (p_val > 0.05) {
      print('The difference between the winning margins of the two parties does not meet established standards
            for statistical significance')
      print(sprintf('The probability that this difference or larger could have arisen by partisan-unbiased mechanisms
                    is %f', p_val))
    }
    else {
      print('The difference between the winning margins of the two parties meets established standards for
            statistical significance')
      print(sprintf('The probability that difference in win margins (or larger) would have arisen by
                    partisan-unbiased mechanisms alone is %f', p_val))
    }
  }
  else {
    print('Cannot compare win margins. For this test, both parties must have at least two seats.')
  }
  
  
}

t1_lopsided_margin(voteshares, 'PA', 2012, 0.75)