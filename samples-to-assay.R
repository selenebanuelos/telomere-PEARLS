## Author: Selene Banuelos
## Date: 1/30/2025
## Description: Make dataset of T5 PEARLS samples that need to be nanodropped
## and/or assayed for telomere length

# setup
library(dplyr)
library(datadictionary) # create data dictionary

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
  select(specimenid, pearls_id, dna_pure, dna_conc_5) %>%
  left_join(.,
            reruns_missing,
            by = c("pearls_id", "specimenid")
            )

# create data dictionary
# variable descriptions
descriptions <- c(specimenid = "Sample ID",
                  pearls_id = "Participant PEARLS ID",
                  dna_pure = "Nanodrop: 1.7 <= 260/280 <= 2.0, 1 = yes, 0 = no, NA = no nanodrop data",
                  dna_conc_5 = "At least 5ng/ul of DNA, 1 = yes, 0 = no, NA = no concentration data",
                  tel_data = "Telomere data available, 1 = yes, 0 = no",
                  rerun = "Sample assayed more than once, 1 = yes, 0 = no"
                  )

dict <- create_dictionary(merged,
                          var_labels = descriptions
                          ) %>%
  select(item, label, class)

# output 
################################################################################
write.csv(merged,
          "data-processed/samples-to-assay.csv",
          row.names = FALSE
)

write.csv(dict,
          "data-processed/samples-to-assay-dictionary.csv",
          row.names = FALSE)
