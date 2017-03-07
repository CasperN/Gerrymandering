library(dplyr)
library(tidyr)
library(ggplot2)
library(TeachingDemos)

voteshares <- read.csv("House_1898_2014_voteshares_notext.csv", header = FALSE) %>%
  tbl_df %>%
  rename(year = V1, state_code = V2, district = V3, d_voteshare = V4, incumbent = V5, winner = V6) %>%
  mutate(d_voteshare = ifelse(d_voteshare < 0, 0, ifelse(d_voteshare > 1, 1, d_voteshare)))

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
  
  d_districts <- which(relevant_data$d_voteshare >= 0.5)
  r_districts <- which(relevant_data$d_voteshare < 0.5)
  
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
  
  d_winvotes <- imputed_data$d_voteshare[d_districts]
  r_winvotes <- 1 - imputed_data$d_voteshare[r_districts]
  
  dems <- cbind(vote = round(d_winvotes,2), party = 'Democrat')
  reps <- cbind(vote = round(r_winvotes,2), party = 'Republican')
  
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

t1_lopsided_margin(voteshares, 'IL', 2000, 0.75)


t2_consistent_advantage <- function(voting_data, state, req_year, uncontested_imputation) {
  
  # Extract relevant information
  
  relevant_data <- filter(voting_data, state_code == which(states == state),
                          year == req_year)
  
  n_delegates <- dim(relevant_data)[1]
  
  d_districts <- which(relevant_data$d_voteshare >= 0.5)
  r_districts <- which(relevant_data$d_voteshare < 0.5)
  
  n_d <- sum(relevant_data$d_voteshare >= 0.5)
  n_r <- n_delegates - n_d
  
  national_data <- filter(voting_data, year == req_year)
  
  # If there were any uncontested districts, replace the voteshares with the imputed value or its inverse
  
  imputed_data <- relevant_data %>%
    mutate(d_voteshare = ifelse(d_voteshare == 1, uncontested_imputation,
                                ifelse(d_voteshare == 0, 1 - uncontested_imputation, d_voteshare)))
  
  imputed_nat_data <- national_data %>%
    mutate(d_voteshare = ifelse(d_voteshare == 1, uncontested_imputation,
                                ifelse(d_voteshare == 0, 1 - uncontested_imputation, d_voteshare)))
  
  d_winvotes <- imputed_data$d_voteshare[d_districts]
  r_winvotes <- 1 - imputed_data$d_voteshare[r_districts]
  
  dems <- cbind(vote = round(d_winvotes,2), party = 'Democrat')
  reps <- cbind(vote = round(r_winvotes,2), party = 'Republican')
  
  winvotes <- as.data.frame(rbind(dems, reps))
  
  # Decide whether or not the parties are closely matched. If they are, use a mean-median difference. Otherwise, chi-squared.
  
  partisan_balance <- abs(mean(imputed_data$d_voteshare) - 0.5)
  
  if (partisan_balance < 0.07) {
    
    print("The parties are closely matched in overall strength, so a mean-median difference is used to check for a partisan advantage")
    
    mean_med_diff <- mean(imputed_data$d_voteshare) - median(imputed_data$d_voteshare)
    
    SK_mmdiff = mean_med_diff * sqrt(n_delegates/0.5708) / sd(imputed_data$d_voteshare) # The 0.5708 comes from Cabilio and Masaro 1996
    
    mmdiff_pval = min(pnorm(SK_mmdiff), 1 - pnorm(SK_mmdiff)) # One-tailed p-value
    
    if (mean_med_diff < 0) {
      print(sprintf("The mean-median difference is %f%% in a direction of advantage to the Democratic Party", abs(mean_med_diff)*100))
    }
    if (mean_med_diff > 0) {
      print(sprintf("The mean-median difference is %f%% in a direction of advantage to the Republican Party", abs(mean_med_diff)*100))
    }
    if (mean_med_diff == 0) {
      print("The mean and median are identical, suggesting that there is no identifiable advantage to either major party. This can occur 
            when all elections are uncontested")
    }
    
    print(sprintf("The mean-median difference would reach this value in %f%% of situations in a partisan-unbiased process", mmdiff_pval*100))
    
    if (mmdiff_pval < 0.05) {
      print("This difference is statistically significant (<0.05) and is therefore unlikely to have arisen by chance")
    }
    else {
      print("This difference is not statistically significant (>0.05)")
    }
    
    # Make a plot to show vote proportions for the two parties
    
    d_winvotes <- imputed_data$d_voteshare[d_districts]
    r_winvotes <- imputed_data$d_voteshare[r_districts]
    
    dems <- cbind(vote = round(d_winvotes,2), party = 'Democrat')
    reps <- cbind(vote = round(r_winvotes,2), party = 'Republican')
    
    winvotes <- as.data.frame(rbind(dems, reps))
    
    p <- ggplot(winvotes, aes(x = state, y = vote, color = party)) + geom_jitter(position = position_jitter(0.001))
    
    p <- p + coord_flip() + scale_color_manual(values = c('blue', 'red')) 
    
    p <- p + labs(title = 'Vote Proportion in Wins', y = 'Vote Proportion', x = 'State')
    
    print(p)
    }
  
  else {
    print("One party is dominant statewide, which means that to gain an advantage, the dominant party will attempt to spread its advantage
          as uniformly as possible across districts. To test abnormal uniformity, we use the chi-squared test to compare the 
          vote share of the majority party-controlled seats with nationwide patterns")
    
    # Perform the chi-squared test
    
    if (n_d > n_r) {
      
      # Calculate the variance of the Democratic party's vote share in the districts that it wins nationwide
      
      national_var = var(imputed_nat_data$d_voteshare[which(imputed_nat_data$d_voteshare >= 0.5)])
      
      # Compare this variance to the variance of the Democratic party's wins in this state with a chi-sq test of variance
      
      chisq_res <- sigma.test(d_winvotes, sigmasq = national_var, alternative = "less")
      
      chisq_p <- chisq_res$p.value
      
      # Print the results of the comparison
      
      print(sprintf("The variance of the Democratic majority's winning vote share is %f", 
                    sd(d_winvotes)))
      
      print(sprintf("At a national level, the standard deviation is %f",
                    sqrt(national_var)))
      
    }
    
    else {
      
      # Calculate the variance of the Republican party's vote share in the districts that it wins nationwide
      
      national_var = var(1 - imputed_nat_data$d_voteshare[which(imputed_nat_data$d_voteshare < 0.5)])
      
      # Compare this variance to the variance of the Republican party's wins in this state with a chi-sq test of variance
      
      chisq_res <- sigma.test(r_winvotes, sigmasq = national_var, alternative = "less")
      
      chisq_p <- chisq_res$p.value
      
      # Print the results of the comparison
      
      print(sprintf("The variance of the Republican majority's winning vote share is %f", 
                    sd(r_winvotes)))
      
      print(sprintf("At a national level, the standard deviation is %f",
                    sqrt(national_var)))
    }
    
    if (chisq_p < 0.05) {
      print(sprintf("At a p-value of %f, the difference in variation is statistically significant at a 95%% confidence level",
                    chisq_p))
    }
    else {
      print(sprintf("At a p-value of %f, the difference in variation is not statistically significant at a 95%% confidence level",
                    chisq_p))
    }
    
    # Make a barplot of Democratic and Republican winvotes
    
    p <- ggplot(winvotes, aes(x = as.numeric(row.names(winvotes)), y = vote, fill = party)) + geom_bar(stat = 'identity')
    
    p <- p + scale_fill_manual(values = c('blue', 'red'))
    
    p <- p + labs(title = 'Chi-squared Test for Unusually Uniform Outcomes', y = 'Vote Proportion', x = 'District') + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    
    print(p)
  }
  
}

t2_consistent_advantage(voteshares, 'IL', 2014, 0.75)

t2_consistent_advantage(voteshares, 'MO', 2014, 0.75)