## Author: Selene Banuelos
## Date: 6/18/2026
## Description: Check that no template control wells have Ct/Cq values >35
## in all telomere and 36B4 plates from both runs

# setup
library(dplyr)
options(scipen = 999)

# import data ------------------------------------------------------------------
# save telomere PCR raw data file names from Run 1 as list
data <- read.csv('data-processed/merged-runs-tel-36b4.csv')

# check no template controls ---------------------------------------------------
# check if any NTC wells show signs of contamination
cont_plates <- data %>% filter(grepl('NTC', Content), 
                         Cq < 35) %>% # Cq <35 indicates contamination
  # get vector of file names of plates with NTC Cq<35
  pull(file_name) %>%
  # remove triplicates
  unique(.)

# add variable to tel/36b4 dataset that indicates if Cq<35 for at least 1 NTC 
# on plate
data <- mutate(data,
               ntc_cont = case_when(file_name %in% cont_plates ~ 1,
                                    !file_name %in% cont_plates ~0))

# output -----------------------------------------------------------------------
# update original file with added NTC check variable
write.csv(data, 'data-processed/merged-runs-tel-36b4.csv', row.names = FALSE)