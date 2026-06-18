## Author: Selene Banuelos
## Date: 1/30/2026
## Description: Make dataset of T5 PEARLS samples that need to be nanodropped
## and/or assayed for telomere length

# setup
library(dplyr)

# import data
################################################################################
# T5 data on samples assayed more than once and samples missing telomere data
reruns_missing <- read.csv('data-processed/reruns-missing-tel-T5.csv')

# T2 and T5 data on buccal DNA quality
dna_qc <- read.csv('data-processed/buccal-dna-qc.csv')

# data wrangling
################################################################################
# rename pearls ID for downstream joining
reruns_missing <- rename(reruns_missing,
                         pearls_id = subjectid)

# merge data on reruns, missingness, and DNA quality into one dataframe
merged <- dna_qc %>%
  # only look at visit number 5
  filter(visitnum == 5) %>%
  # keep the following variables from DNA QC dataset
  select(specimenid, 
         pearls_id, 
         visitnum, 
         os_dna_conc, 
         dna_pure, 
         dna_conc_5,
         dna_qc_passed) %>%
  # join rerun and missing information to DNA QC data frame
  left_join(.,
            reruns_missing,
            by = c('pearls_id', 
                   'specimenid', 
                   'visitnum', 
                   'os_dna_conc'))

# create data dictionary
################################################################################
# variable names
var_names <- names(merged)

# data types
data_type <- sapply(merged, class)

# variable descriptions
descriptions <- c("Sample ID",
                  "Participant PEARLS ID",
                  "Timepoint",
                  "DNA concentration (ng/ul)",
                  "At least 1 Nanodrop measurement within range: 1.7 <= 260/280 <= 2.0",
                  "At least 5ng/ul of DNA",
                  "Telomere data available",
                  "Sample assayed more than once"
                  )

# possible values for variables
values <- c("3 digit integer",
            "4 digit integer with 'P' prefix",
            "5",
            "Number >= 0",
            "1 = yes, 0 = no, NA = no nanodrop data",
            "1 = yes, 0 = no, NA = no concentration data",
            "1 = yes, 0 = no",
            "1 = yes, 0 = no"
            )
            
# create dictionary
dict <- data.frame(var_names, 
                   data_type, 
                   descriptions, 
                   values,
                   row.names = NULL)

# output 
################################################################################
write.csv(merged,
          "data-processed/samples-to-assay.csv",
          row.names = FALSE)

write.csv(dict,
          "data-processed/samples-to-assay-dictionary.csv",
          row.names = FALSE)
