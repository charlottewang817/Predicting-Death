# Kenneth Lipke
# ORIE 4741 Project

setwd("~/Documents/ORIE 4741 Big Messy Data/Project")

#### setup --------
# read in data
df = read.csv("2015_data.csv")

# get only a smaller portion of the data to play with
df2 = df[1:200000, ]

#### ages -----
# missings
sum(is.na(df2$detail_age)) # none (good)

#drop if detail_age_type != 1
df2 <- df2[df2$detail_age_type==1, ]

#make sure people aren't 1,000 years old:
df2 <- df2[df2$detail_age != 999, ] # some error codings

# histogram death age (years)
hist(df2$detail_age, col='gray')
summary(df2$detail_age)

#### educations -----
# how many missings:
sum(is.na(df2$education_2003_revision))

# drop the missing
df2 <- df2[!is.na(df2$education_2003_revision), ]

edu = c('<8th grade', '9 - 12th grade','high school', 'some college', 'Associate', 'Bachelor', 'Masters',
        'Doctorate/professional', 'unknown')
barplot(table(df2$education_2003_revision), main='Education Bar Plot', xlab='Education Level',
        names.arg = edu)
# the for loop with update the labels in data set
for (i in 1:9){
  df2$education_2003_revision[df2$education_2003_revision==i] <- edu[i]
}


#### Place of death ----
sum(is.na(df2$place_of_death_and_decedents_status)) # none

place_of_death = c('Hospital: inpatient', 'Hospital: outpatient', 'Hospital: DOA', 
                   'Home', 'Hospice', 'Nursing Home', 'Other', 'Unknown')
df2$place_of_death_and_decedents_status[df2$place_of_death_and_decedents_status==9] <- 8
for (i in (1:8)){
  df2$place_of_death_and_decedents_status[df2$place_of_death_and_decedents_status==i] <- place_of_death[i]
}
par(mar = c(9, 4, 4, 2))
barplot(table(df2$place_of_death_and_decedents_status), las=2, main='Place of Death')
par(mar = c(5, 4, 4, 2)) # reset parameters


#### Martial Status -----
sum(is.na(df2$marital_status)) # none 
m_status = c('Single', 'Married', 'Widowed', 'Divorced', 'Unknown')
m_code = c('S', 'M', 'W', 'D', 'U')
df2$marital_status <- as.character((df2$marital_status))
for (i in (1:5)){
  df2$marital_status[df2$marital_status==m_code[i]] <- m_status[i]
}
df2$marital_status <- as.factor(df2$marital_status)
barplot(table(df2$marital_status), main='Mariage Status', las=2)


#### Injury at work ----
sum(is.na(df2$injury_at_work)) # none
barplot(table(df2$injury_at_work), main='Injury at Work')
table(df2$injury_at_work)


### Manner of Death ----
sum(is.na(df2$manner_of_death)) # there are a lot of missings (but may not use this var)
barplot(table(df2$manner_of_death), main='Manner of Death\n(7 is natural causes)')

### Entity Condtion ----

# There are 20 of these fields (not all will be filled in)
# they describe the conditions the person had. They are coded...
# We only care about the letter and the following two numbers.
# For example "11I250" --> I25 which per the table is Ischemic heart diseases.
# Our challenge now is getting the code table into a useable formate and converting
# the entity conditions. 
# 
# Another possible thought is to not decode them untill AFTER the analysis
# and we can decode the relevant ones by hand, with the hope that there will
# not be too many of them.

# See API guide here
"https://clin-table-search.lhc.nlm.nih.gov/apidoc/icd10cm/v3/doc.html"

# 1. defactorize all condition variables
df2$entity_condition_1 <- as.character(df2$entity_condition_1)

# 2. split the strings --> substring(x, 3, 5)
df2$entity_condition_1 <- substr(df2$entity_condition_1, 3, 5)

# 3. figure out the damn API
library(httr)
library(jsonlite)


#### Functions -----
# Takes in the code, e.g., I25 and returns the condition from API
get_condition <- function(code){
  
  # Query API
  url <- "https://clin-table-search.lhc.nlm.nih.gov/api/icd10cm/v3/search?"
  path <- paste0("terms=", as.character(code))
  result <- GET(url = paste0(url, path))
  
  # Catch possible API errors
  if (result$status_code != 200){
    return("ERROR")
  }
  
  # unpack result
  content <- rawToChar(result$content)
  content <- fromJSON(content)
  
  # dcompose list
  l <- content[[4]]
  
  # Catch possible error
  if (length(l) == 0){
    return("ERROR")
  }
  
  condition_line1 <- l[,2][1]
  #x <- regexpr(' ', condition_line1)[1] - 1
  places <- unlist(gregexpr(' ', condition_line1))
  if (length(places) > 2 ){
    x <- places[3] - 1
  }else if (length(places) == 2){ 
    x <- places[2] - 1
  } else {
    x <- places[1]
  }
  
  #take first word to be condition
  condition <- substring(condition_line1, 0 , x)
  
  return(condition)
}

# Now write a new get condition function that checks the hash table first, then
# if the hash table does not have the key, then we go to the other function
get_hash_cond <- function(code){
  if (code == ""){ # degenerate empty case
    return("")
  }
  if ( has.key(eval(code), condition_dictionary)){
    return(condition_dictionary[eval(code)])
  }
  return(get_condition(code))
}


## FOR TESTING AND SETTING UP USE:
df2$condition_test <- df2$entity_condition_1

#### Build Dictionary ----
# That clearly takes forever, lets try to run through for a while and build up a dictionary,
# then update the get condition function to check the dictionary first
condition_dictionary <- hash() # empty dictionary
N = length(df2$condition_test)
codes <- vector("list", N)
conditions <- vector("list", N)
for (i in 1:N){
  print(paste("At", i, "of", N))
  code = df2$condition_test[i]
  cond <- get_condition(code)
  codes[i] <- code
  conditions[i] <- cond
  #condition_dictionary$eval(code) <- eval(cond)
  #.set(condition_dictionary, key=code, value=cond)
}
codes <- codes[1:i]
conditions <- conditions[1:i]
.set(condition_dictionary, keys=codes, values=conditions)

# Save the hashtable for later use
save(condition_dictionary, file='Condition_table.Rda')

#check if in with has.key(key, hash)


#### Fill in test columns ---- 
N = length(df2$condition_test)
for (i in 143845:N){
  print(paste("At", i, "of", N))
  code = df2$condition_test[i]
  cond <- get_hash_cond(code)
  if (class(cond)=='character'){
    df2$condition_test[i] <- cond
  }else { # else it came from dictionary so get value
    df2$condition_test[i] <- values(cond)
  }
}


#### Apply to ALL entity conditions:
prefix <- "entity_condition_"
for (i in (1:20)){
  var <- paste0(prefix, i)
  # 1. defactorize all condition variables
  df2[, var] <- as.character(df2[, var])
  # 2. split the strings --> substring(x, 3, 5)
  df2[, var] <- substr(df2[, var], 3, 5)
  print(paste("On variable", i, "of", "20"))
  
  # iterate over each entry
  N <- dim(df2)[1]
  for (n in 1:20000){
    
    # print update
    if (n %% 10000 == 0){
      print(paste("obs", n, "of", N, "for varialbe", i))
    }
    
    code <- df2[n,var]
    cond <- get_hash_cond(code)
    if (class(cond)=='character'){
      df2[n,var] <- cond
    }else { # else it came from dictionary so get value
      df2[n,var] <- values(cond)
    }
  }
}
