library(squire)
library(tidyverse)
library(readxl)

'
This file follows the following logic:

  - From NCAER lockdown data, find average no. of outside contacts per 
    age bin, as well as average no. of inside contacts. 
    
  - Sum these for "total contacts" under lockdown acc. to NCAER.
    
  - Find average no. of total contacts per age bin in squire matrix 
    (2015 Haryana survey).
      
  - Record percent change between squire total contacts & NCAER 
    total contacts.
    
  - Make new matrix, modifying bins in squire matrix by appropriate
    change factor.
    
Notes on matrix naming:

  - I call lockdown matrices phase1_matrix and phase3_matrix without
    phase2_matrix. This is because, in Delhi, phase 2 of lockdown
    was virtually the same as phase 1.
    
  - I am using a convention of naming the India default matrix out 
    of squire squire_matrix.
   
'

# Grab NCAER survey results (just outside contacts)
ncaer_outside <- as.data.frame(read_excel("data/DCVTS2_delhi_contacts.xlsx", 
                               sheet = "Outside")) 

ncaer_inside <- as.data.frame(read_excel("data/DCVTS2_delhi_contacts.xlsx", 
                              sheet = "Inside"))

' #DELETE
# Merge outside and inside datasets, first making unique id to merge on
ncaer_outside$id <- paste(ncaer_outside$`Age of respondent`, ncaer_outside$`Contacts outside`)
ncaer_inside$id  <- paste(ncaer_inside$`Age of respondent`, ncaer_inside$`Contacts inside`)
ncaer <- merge(x = ncaer_outside, y = ncaer_inside, by = "id", all = FALSE) 
'
# Change all "<= 10" values to 12 -- arbitrarily assuming avg 10+ is 12
ncaer_outside$`Contacts outside`[!ncaer_outside$`Contacts outside` %in% as.character(0:9)] <- 12
ncaer_inside$`Contacts inside`[!ncaer_inside$`Contacts inside` %in% as.character(0:9)] <- 12

# Make contact values numeric
ncaer_outside$`Contacts outside` <- as.numeric(ncaer_outside$`Contacts outside`)
ncaer_inside$`Contacts inside` <- as.numeric(ncaer_inside$`Contacts inside`)

# In each df, create new column for no. of contacts x frequency
ncaer_outside$weighted_sum <- ncaer_outside$`Contacts outside` * ncaer_outside$Freq
ncaer_inside$weighted_sum <- ncaer_inside$`Contacts inside` * ncaer_inside$Freq

# For each df, find avg. no. of contacts per age bin

outside_18_29 <- sum(subset(ncaer_outside, `Age of respondent` == '18-29', weighted_sum)) /
                 sum(ncaer_outside$Freq[ncaer_outside$`Age of respondent` == '18-29'])

outside_30_39 <- sum(subset(ncaer_outside, `Age of respondent` == '30-39', weighted_sum)) /
                 sum(ncaer_outside$Freq[ncaer_outside$`Age of respondent` == '30-39'])

outside_40_49 <- sum(subset(ncaer_outside, `Age of respondent` == '40-49', weighted_sum)) /
                 sum(ncaer_outside$Freq[ncaer_outside$`Age of respondent` == '40-49'])

outside_50_59 <- sum(subset(ncaer_outside, `Age of respondent` == '50-59', weighted_sum)) /
                 sum(ncaer_outside$Freq[ncaer_outside$`Age of respondent` == '50-59'])

outside_60_plus <- sum(subset(ncaer_outside, `Age of respondent` == '60+', weighted_sum)) /
                   sum(ncaer_outside$Freq[ncaer_outside$`Age of respondent` == '60+'])

inside_18_29 <- sum(subset(ncaer_inside, `Age of respondent` == '18-29', weighted_sum)) /
                sum(ncaer_inside$Freq[ncaer_inside$`Age of respondent` == '18-29'])

inside_30_39 <- sum(subset(ncaer_inside, `Age of respondent` == '30-39', weighted_sum)) /
                sum(ncaer_inside$Freq[ncaer_inside$`Age of respondent` == '30-39'])

inside_40_49 <- sum(subset(ncaer_inside, `Age of respondent` == '40-49', weighted_sum)) /
                sum(ncaer_inside$Freq[ncaer_inside$`Age of respondent` == '40-49'])

inside_50_59 <- sum(subset(ncaer_inside, `Age of respondent` == '50-59', weighted_sum)) /
                sum(ncaer_inside$Freq[ncaer_inside$`Age of respondent` == '50-59'])

inside_60_plus <- sum(subset(ncaer_inside, `Age of respondent` == '60+', weighted_sum)) /
                  sum(ncaer_inside$Freq[ncaer_inside$`Age of respondent` == '60+'])

# Create new "total contacts" number by summing inside & outside contact averages

total_18_29 <- inside_18_29 + outside_18_29

total_30_39 <- inside_30_39 + outside_30_39

total_40_49 <- inside_40_49 + outside_40_49 

total_50_59 <- inside_50_59 + outside_50_59

total_60_plus <- inside_60_plus + outside_60_plus


# Grab squire's default India matrix
india_params_list <- parameters_explicit_SEEIR('India')
squire_matrix <- as.data.frame(india_params_list$contact_matrix_set)

# In squire_matrix, each column refers to 5-year age bucket. 
# Determine factor by which to reduce values for each age bin.
# Factor equals total lockdown contacts for age bin divided by 
# total squire contacts for age bin.

reduction_0_4 <- inside_18_29 / sum(squire_matrix[1])   # assume only inside contacts
reduction_5_9 <- inside_18_29 / sum(squire_matrix[2])   # assume only inside contacts
reduction_10_14 <- inside_18_29 / sum(squire_matrix[3]) # assume only inside contacts
reduction_15_19 <- inside_18_29 / sum(squire_matrix[4]) # assume only inside contacts
reduction_20_24 <- total_18_29 / sum(squire_matrix[5]) 
reduction_25_29 <- total_18_29 / sum(squire_matrix[6]) 
reduction_30_34 <- total_30_39 / sum(squire_matrix[7]) 
reduction_35_39 <- total_30_39 / sum(squire_matrix[8]) 
reduction_40_44 <- total_40_49 / sum(squire_matrix[9]) 
reduction_45_49 <- total_40_49 / sum(squire_matrix[10]) 
reduction_50_54 <- total_50_59 / sum(squire_matrix[11]) 
reduction_55_59 <- total_50_59 / sum(squire_matrix[12]) 
reduction_60_64 <- total_60_plus / sum(squire_matrix[13]) 
reduction_65_69 <- total_60_plus / sum(squire_matrix[14]) 
reduction_70_74 <- total_60_plus / sum(squire_matrix[15])   # assume NCAER 60+ values hold
reduction_75_plus <- total_60_plus / sum(squire_matrix[16]) # assume NCAER 60+ values hold

# Make a dataframe of 16x16 in which each column contains the reduction 
# factor for the associated age bin (will multiply whole thing by squire_matrix).

phase1_discount_matrix <- cbind(
                                rep(reduction_0_4, times = 16),
                                rep(reduction_5_9, times = 16),
                                rep(reduction_10_14, times = 16),
                                rep(reduction_15_19, times = 16),
                                rep(reduction_20_24, times = 16),
                                rep(reduction_25_29, times = 16),
                                rep(reduction_30_34, times = 16),
                                rep(reduction_35_39, times = 16),
                                rep(reduction_40_44, times = 16),
                                rep(reduction_45_49, times = 16),
                                rep(reduction_50_54, times = 16),
                                rep(reduction_55_59, times = 16),
                                rep(reduction_60_64, times = 16),
                                rep(reduction_65_69, times = 16),
                                rep(reduction_70_74, times = 16),
                                rep(reduction_75_plus, times = 16)
                              )

# Start with copy of squire matrix and then change columns accordingly.
phase1_matrix <- squire_matrix * phase1_discount_matrix

# Make matrices for additional phases. Assume that phase 3 (May 3 to
# May 31) is 10% increase up from phase1_matrix.
phase3_matrix <- phase1_matrix * 1.1

# Save matrices
write.csv(phase1_matrix, "matrices/delhi/phase1_matrix.csv", row.names = FALSE)
write.csv(phase3_matrix, "matrices/delhi/phase3_matrix.csv", row.names = FALSE)



