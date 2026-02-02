## Author: Selene Banuelos
## Date: 1/30/2025
## Description: Make dataset of T5 PEARLS samples that need to be nanodropped
## and/or assayed for telomere length

# setup
library(dplyr)

# import data
################################################################################
# T5 data on samples assayed more than once and samples missing telomere data
reruns_missing <- read.csv('data-processed/reruns-missing-T5.csv')

# T2 and T5 data on sample DNA quality
dna_qc <- read.csv('data-processed/dna-qc.csv')

# data wrangling
################################################################################
# rename pearls ID for downstream joining
reruns_missing <- rename(reruns_missing,
                         pearls_id = subjectid)

# merge data on reruns, missingness, and DNA quality into one dataframe
merged <- dna_qc %>%
  filter(visitnum == 5) %>%
  select(specimenid, pearls_id, visitnum, dna_pure, dna_conc_5) %>%
  left_join(.,
            reruns_missing,
            by = c("pearls_id", "specimenid")
            )

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
                  "At least 1 Nanodrop measurement within range: 1.7 <= 260/280 <= 2.0",
                  "At least 5ng/ul of DNA",
                  "Telomere data available",
                  "Sample assayed more than once"
                  )

# possible values for variables
values <- c("3 digit integer",
            "4 digit integer with 'P' prefix",
            "5",
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
          row.names = FALSE
)

write.csv(dict,
          "data-processed/samples-to-assay-dictionary.csv",
          row.names = FALSE)
