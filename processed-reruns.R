## Author: Selene Banuelos
## Date: 11/25/2025
## Description: Identify timepoint 5 participants that have telomere data and 
## those who were assayed more than once

# setup 
library(dplyr)

# save raw data file names as list
file_names <- list.files(path = 'data-raw/t5/',
                         pattern = '.csv',
                         full.names = TRUE)

# import all raw data files and combine into one dataframe
raw_data <- purrr::map_dfr(file_names, 
                   # import .csv as df and add column with original file name
                   function(x) read.csv(x) %>% mutate(file_name = x)
                   )

# identify reruns
reruns <- raw_data %>%
  # create run_date column
  mutate(run_date = stringr::str_extract(
    file_name, 
    'TEL_(.*?) - Quant')
    )