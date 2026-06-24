## Author: Selene Banuelos
## Date: 6/18/2026
## Description: Merge all raw data from telomere plates in run 1 with
## telomere and 36B4 plates in run 2

# setup
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
options(scipen = 999)

# import data ------------------------------------------------------------------
# save telomere PCR raw data file names from Run 1 as list
files_tel1 <- list.files(path = 'data-raw/t5-buccal/run1-tel/',
                         pattern = '.csv',
                         full.names = TRUE)

# save 36B4 PCR raw data file names from run 1 as list
files_36b41 <- list.files(path = 'data-raw/t5-buccal/run1-36B4/',
                          pattern = '.csv',
                          full.names = TRUE)

# save telomere PCR raw data file names from Run 2 as list
files_tel2 <- list.files(path = 'data-raw/t5-buccal/run2-tel/',
                         pattern = '.csv',
                         full.names = TRUE)

# save 36B4 PCR raw data file names from run 2 as list
files_36b42 <- list.files(path = 'data-raw/t5-buccal/run2-36B4/',
                         pattern = '.csv',
                         full.names = TRUE)

# import all raw data files from each folder and save as dataframes
data_tel1 <- map_dfr(files_tel1, 
                     # import .csv as df and add column with original file name
                     function(x) read.csv(x) %>% mutate(file_name = x, run = 1))

data_36b41 <- map_dfr(files_36b41, 
                      # import .csv as df and add column with original file name
                      function(x) read.csv(x) %>% mutate(file_name = x, run = 1))

data_tel2 <- map_dfr(files_tel2, 
                     # import .csv as df and add column with original file name
                     function(x) read.csv(x) %>% mutate(file_name = x, run = 2))

data_36b42 <- map_dfr(files_36b42, 
                     # import .csv as df and add column with original file name
                     function(x) read.csv(x) %>% mutate(file_name = x, run = 2))

# data wrangling ---------------------------------------------------------------
# combine all telomere and 36b4 data from run 2
merged <- rbind(data_tel1, data_36b41, data_tel2, data_36b42) %>%
  # remove relative path from file name
  mutate(file_name = str_extract(file_name,
                                 # keep group/string after 'run2-*/'
                                 'run[^/]+/(.*)', 
                                 group = 1)) %>%
  # create date of run column based on file name
  mutate(date = str_extract(file_name,
                            'Buccal_[^_]+_(.*)_S',
                            group = 1)) %>%
  # create sample set column based on file name
  mutate(set = str_extract(file_name,
                           '(?<=\\d{6}_)(.*)(?= - Quant)')) %>%
  # strip any leading zeros from sample names
  mutate(Sample = str_remove(Sample,'^0+')) %>%
  # remove empty columns
  remove_empty('cols')

# clean this up ----------------------------------------------------------------
## strange specimen ID investigation -------------------------------------------
# there is weird sample ID in 36B4 data...
# use batch & plate well location from telomere data to correct strange ID
# split 'batch' variable into 'date' and 'set' variables
# tel_run2[c('date', 'set')] <- str_split_fixed(tel_run2$batch, '_', 2)
# clean_36b4[c('date', 'set')] <- str_split_fixed(clean_36b4$batch, '_', 2)
# 
# # telomere data 
# tel <- select(tel_run2, pearls_id, specimenid, Well, date, set) %>%
#   rename_with(~ paste0('tel_', .x), .cols = c('date', 'specimenid'))
# 
# # 36B4 data
# three6b4 <- select(clean_36b4, specimenid, Well, date, set) %>%
#   rename_with(~ paste0('three6b4_', .x), .cols = c('date', 'specimenid'))
# 
# # check if specimen IDs match up by plate location (Well)
# set_well <- full_join(tel, three6b4, by = c('set', 'Well')) %>%
#   mutate(ids_match = case_when(tel_specimenid == three6b4_specimenid ~ 1,
#                                tel_specimenid != three6b4_specimenid ~ 0))


# change incorrectly entered ID to correct ID in 36b4 data, based on matching set 
# and plate location in telomere data
merged[merged == 'J00043'] <- '741'

# change incorrectly entered probe target for one sample in 36b4 data
merged$Target[merged$Target == '000741'] <- '36B4'

# change empty probe target for NTC in one plate of 36b4 data
merged$Target[merged$Target == ''] <- '36B4'

# change content of 364B well to 'Unkn' since it's currently 'Pos Ctrl-01'
merged$Content[merged$Sample == '790' & merged$date == '103123'] <- 'Unkn'

# check no template controls ---------------------------------------------------
# check if any NTC wells show signs of contamination
cont_plates <- merged %>% filter(grepl('NTC', Content), 
                                 Cq < 35) %>% # Cq <35 indicates contamination
  # get vector of file names of plates with NTC Cq<35
  pull(file_name) %>%
  # remove triplicates
  unique(.)

# add variable to tel/36b4 dataset that indicates if Cq<35 for at least 1 NTC 
# on plate
merged <- mutate(merged, 
                 ntc_cont = case_when(file_name %in% cont_plates ~ 1,
                                      !file_name %in% cont_plates ~0))

# output -----------------------------------------------------------------------
write.csv(merged, 'data-processed/merged-runs-tel-36b4.csv', row.names = FALSE)