library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(censusapi)
library(tidyr)
library(dplyr)
library(data.table)
library(tibble)

###########
## SETUP ##
###########

# Add key to .Renviron
Sys.setenv(CENSUS_KEY=CENSUS_KEY)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
#Sys.getenv("CENSUS_KEY")

##########################
## FUNCTION DEFINITIONS ##
##########################

# Get state list
get_state_df <- function() {
  states <- getCensus(
    name = "2019/acs/acs1",
    vars = "NAME",
    region = "state:*"
  )
  
  return(states)
}

# Convert a vector of state names to a vector of state codes
convert_states_to_codes <- function(state_list, state_name_list) {
  # Initialize empty vector
  state_code_list = c()
  
  # Look up state code and append to vector
  for (s in state_name_list) {
    
    if (s == "All") {
      state_code_list <- c("*")
    } else {
      rownum <- which(state_list$NAME == s)
      code <- state_list[rownum,1]
      state_code_list <- append(state_code_list, code)
    }
    
  }
  
  # Return vector of state codes
  return(state_code_list)
}

# Makes an API call for a given list of state codes and returns a dataframe of summed population counts
get_sums_for_state_list <- function(estim_type, estim_year, state_list) {
  # Stringify state code list
  states_str <- paste("state:", paste(state_list, collapse=","), sep="")
  
  # Make API call
  result <- getCensus(
    name = estim_type,
    vintage = estim_year,
    vars = c("NAME", # Name of state
             "B03002_001E", # Total
             "B03002_002E", # Total : Not Hispanic or Latino
             "B03002_003E", # Total : Not Hispanic or Latino : White alone
             "B03002_004E", # Total : Not Hispanic or Latino : Black or African American alone
             "B03002_005E", # Total : Not Hispanic or Latino : American Indian and Alaska Native alone
             "B03002_006E", # Total : Not Hispanic or Latino : Asian alone
             "B03002_007E", # Total : Not Hispanic or Latino : Native Hawaiian and Other Pacific Islander alone
             "B03002_008E", # Total : Not Hispanic or Latino : Some other race alone
             "B03002_009E", # Total : Not Hispanic or Latino : Two or more races
             "B03002_012E", # Total : Hispanic or Latino
             "B03002_013E", # Total : Hispanic or Latino : White alone
             "B03002_014E", # Total : Hispanic or Latino : Black or African American alone
             "B03002_015E", # Total : Hispanic or Latino : American Indian and Alaska Native alone
             "B03002_016E", # Total : Hispanic or Latino : Asian alone
             "B03002_017E", # Total : Hispanic or Latino : Native Hawaiian and Other Pacific Islander alone
             "B03002_018E", # Total : Hispanic or Latino : Some other race alone
             "B03002_019E" # Total : Hispanic or Latino : Two or more races
    ),
    region = states_str
  )
  
  # Create data frame
  n <- result %>% select(-state, -NAME) %>% colSums(na.rm=TRUE, dims=1)
  sums_df <- as.data.frame(n)
  
  row_labels <- c("Total", 
                  "Not Hispanic or Latino", 
                  "Not Hispanic or Latino : White alone",
                  "Not Hispanic or Latino : Black or African American alone",
                  "Not Hispanic or Latino : American Indian and Alaska Native alone",
                  "Not Hispanic or Latino : Asian alone",
                  "Not Hispanic or Latino : Native Hawaiian and Other Pacific Islander alone",
                  "Not Hispanic or Latino : Some other race alone",
                  "Not Hispanic or Latino : Two or more races",
                  "Hispanic or Latino",
                  "Hispanic or Latino : White alone",
                  "Hispanic or Latino : Black or African American alone",
                  "Hispanic or Latino : American Indian and Alaska Native alone",
                  "Hispanic or Latino : Asian alone",
                  "Hispanic or Latino : Native Hawaiian and Other Pacific Islander alone",
                  "Hispanic or Latino : Some other race alone",
                  "Hispanic or Latino : Two or more races")
  
  sums_df["category"] <- row_labels
  sums_df <- sums_df %>% select(category, n)
  
  return(sums_df)
}

# Takes in df of sums (output of get_sums_for_state_list()), adjusts for "Some other race alone", returns population proportions
calculate_adjusted_proportions <- function(sums_df) {
  # Define useful constants
  tot_n <- sums_df[1,2]
  nhl_n <- sums_df[2,2]
  hl_n <- sums_df[10,2]
  nhl_prop <- nhl_n/tot_n
  hl_prop <- hl_n/tot_n
  
  # Separate out ethnicities into separate dataframes
  nhl_df <- sums_df %>% slice(2:9) # Not Hispanic or Latino
  hl_df <- sums_df %>% slice(10:17) # Hispanic or Latino
  
  # Subtract out "Some other race alone" [row 7] from total population [row 1]
  nhl_df[9,2] = nhl_df[1,2] - nhl_df[7,2]
  hl_df[9,2] = hl_df[1,2] - hl_df[7,2]
  
  # Calculate (adjusted) population proportions
  adj_nhl_n <- nhl_df[9,2]
  adj_hl_n <- hl_df[9,2]
  nhl_prop_df <- nhl_df %>% mutate(adj_prop = n/adj_nhl_n*nhl_prop) %>% slice(-1,-7)
  hl_prop_df <- hl_df %>% mutate(adj_prop = n/adj_hl_n*hl_prop) %>% slice(-1, -7)
  
  # Bind dfs
  prop_df <- bind_rows(nhl_prop_df, hl_prop_df) %>% slice(-7,-14)
  
  # Return result
  return(prop_df)
}

# Generates distribution based on target N and proportions
generate_distribution <- function(prop_df, target_n, male_prop) {
  # Get target numbers of males and females
  n_male <- round(target_n*male_prop, 0)
  n_female <- (target_n-n_male)
  
  # Multiply target number by target proportion
  out_df <- prop_df %>% mutate(f = adj_prop * n_female,
                               m = adj_prop * n_male,
                               f_round = round(f,0),
                               m_round = round(m,0))
  
  # Check whether rounded numbers add to target
  female_sum <- sum(out_df$f_round)
  male_sum <- sum(out_df$m_round)
  female_diff <- n_female - female_sum
  male_diff <- n_male - male_sum
  
  if(female_diff != 0) {
    out_df[1,6] <- out_df[1,6] + female_diff # Adjust non-Hispanic/Latino white female category
  }
  
  if(male_diff != 0) {
    out_df[1,7] <- out_df[1,7] + male_diff # Adjust non-Hispanic/Latino white male category
  }
  
  # Sum across males and females and calculate percentage
  out_df <- out_df %>% mutate(f_round = as.integer(f_round),
                              m_round = as.integer(m_round),
                              total_n = as.integer(f_round + m_round),
                              raw_prop = total_n/target_n,
                              pretty_prop = paste(as.character(round(raw_prop*100, 2)), "%", sep = ""))
  
  # Return output
  return(out_df)
}

# Return the distribution in a nicer format
format_distribution <- function(out_df) {
  # Select desired columns
  subs <- out_df %>% select(category, female=f_round, male=m_round, total=total_n, proportion=pretty_prop)
  
  # Split ethnicity/race into two columns
  two_col <- subs %>% separate(category, c("ethnicity","race"), sep=":")
  
  # Return output df
  return(two_col)
}

# Return proportions in a nicer format
format_proportion <- function(prop_df) {
  # Format proportion
  prop_df2 <- prop_df %>% mutate(n = as.integer(n),
                                 proportion = paste(as.character(round(adj_prop*100,2)), "%", sep=""))
  
  # Select desired colums
  subs <- prop_df2 %>% select(category, proportion, n)
  
  # Split ethnicity/race into two columns
  two_col <- subs %>% separate(category, c("ethnicity","race"), sep=":")
  
  # Return output df
  return(two_col)
}