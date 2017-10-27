# cond_dict.R
# Updates conde <---> condition hash table
# when you have to look up a new code by hand
# add it here

setwd("~/Documents/ORIE 4741 Big Messy Data/Project")
library(hash)

# Load dictionary
load(file='Condition_table.Rda')

# update/add keys
condition_dictionary["R63"] = "Abnormal Clinical and Lab Findings"
condition_dictionary["N28"] = "other genitourinary system"
condition_dictionary["N45"] = "other genitourinary system"

# top conditions
condition_dictionary["I46"] = "Cardiac Arrest"
condition_dictionary["I50"] = "Heart Failure"
condition_dictionary["C34"] = "Malignant neoplasm of Lung"
condition_dictionary["I25"] = "Ischemic Heart Diesease"
condition_dictionary["A41"] = "Septicimia"
condition_dictionary["I21"] = "Acute myocardial infarction "
condition_dictionary["J44"] = "Lower respiratory disease"
condition_dictionary["R09"] = "Other circulatory and respiratory"
condition_dictionary["J18"] = "Bronchopneumonia"
condition_dictionary["F03"] = "Dimentia"
condition_dictionary["I64"] = "Stroke"
condition_dictionary["C25"] = "Malignant neoplasm of pancreas"
condition_dictionary["N18"] = "Chronic Kidney Disease"
condition_dictionary["C18"] = "Malignant neoplasm of colon"
condition_dictionary["C50"] = "Malignant neoplasm of breast"

  
  

# check
condition_dictionary


# Save the updated dictionary
save(condition_dictionary, file='Condition_table2.Rda')
