library(squire)
library(tidyverse)
library(readxl)

'
This file does the following:
  - From NCAER lockdown data, find average no. of total contacts (no. of 
    outside contacts + no. of inside contacts / no. of respondents) per 
    age bin.
  - Find average no. of total respondents per age bin in squire matrix 
    (2015 Haryana survey).
  - For each age bin, record percent change between squire & NCAER.
  - Make new matrix, modifying bins in squire matrix by appropriate
    change factor.
'


# Grab NCAER survey results (just outside contacts)
ncaer <- read_excel("data/DCVTS2_delhi_contacts.xlsx") %>%
  as.data.frame

# Change all "<= 10" values to 12 -- arbitrarily assuming avg 10+ is 12
ncaer$`Contacts outside`[!ncaer$`Contacts outside` %in% as.character(0:9)] <- 12

# Make `Contacts outside` values numeric
ncaer$`Contacts outside` <- as.numeric(ncaer$`Contacts outside`)

# Create new column for no. of contacts x rate of frequency
ncaer$weighted_sum <- ncaer$`Contacts outside` * ncaer$Freq

# Find avg outside contact number during lockdown 
# for each age bin (sum weighted counts & divide by total respondents
# per bin)

bin_18_29 <- sum(subset(ncaer, `Age of respondent` == '18-29', weighted_sum)) /
             sum(ncaer$Freq[ncaer$`Age of respondent` == '18-29'])
  
bin_30_39 <- sum(subset(ncaer, `Age of respondent` == '30-39', weighted_sum)) /
             sum(ncaer$Freq[ncaer$`Age of respondent` == '30-39'])

bin_40_49 <- sum(subset(ncaer, `Age of respondent` == '40-49', weighted_sum)) /
             sum(ncaer$Freq[ncaer$`Age of respondent` == '40-49'])

bin_50_59 <- sum(subset(ncaer, `Age of respondent` == '50-59', weighted_sum)) /
             sum(ncaer$Freq[ncaer$`Age of respondent` == '50-59'])

bin_60_plus <- sum(subset(ncaer, `Age of respondent` == '60+', weighted_sum)) /
               sum(ncaer$Freq[ncaer$`Age of respondent` == '60+'])

# Find average no. of contacts

# Grab squire's default India matrix
india_params_list <- parameters_explicit_SEEIR('India')