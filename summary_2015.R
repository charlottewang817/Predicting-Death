# Kenneth Lipke
# summary_2015.R

setwd("~/Documents/ORIE 4741 Big Messy Data/Project")

library(httr)
library(jsonlite)

# Read in data
df = read.csv("2015_data.csv")

# High level missings
colSums(is.na(df))

# Age ------------
summary(df$detail_age)
sum(df$detail_age< 18) + sum(df$detail_age== 999)
df <- df[df$detail_age != 999, ]
df <- df[df$detail_age >= 18, ]
hist(df$detail_age, xlab="Age", main='Distribution of Age at Death', freq=F,
     col='gray')

# Education --------
edu = c('<8th grade', '9 - 12th grade','high school', 'some college', 'Associate', 'Bachelor', 'Masters',
        'Doctorate/professional', 'unknown')
barplot(table(df$education_2003_revision), main='Education Bar Plot', xlab='Education Level',
        names.arg = edu)
# update labels in df
for (i in 1:9){
  df$education_2003_revision[df$education_2003_revision==i] <- edu[i]
}

table(df$education_2003_revision)

sum(is.na(df$education_2003_revision)) + 119831 #from the table?

# drop missing and unkown
df <- df[!is.na(df$education_2003_revision), ]
df <- df[df$education_2003_revision != 'unknown', ]


# Place of death -----
# no missings here
place_of_death = c('Hospital: inpatient', 'Hospital: outpatient', 'Hospital: DOA', 
                   'Home', 'Hospice', 'Nursing Home', 'Other', 'Unknown')
df$place_of_death_and_decedents_status[df$place_of_death_and_decedents_status==9] <- 8
for (i in (1:8)){
  df$place_of_death_and_decedents_status[df$place_of_death_and_decedents_status==i] <- place_of_death[i]
}
par(mar = c(9, 4, 4, 2))
barplot(table(df$place_of_death_and_decedents_status), las=2, main='Place of Death')
par(mar = c(5, 4, 4, 2)) # reset parameters

# drop the unknown place of death (1,290 of them)
sum(df$place_of_death_and_decedents_status=='Unknown') + sum(is.na(df$place_of_death_and_decedents_status))
df <- df[df$place_of_death_and_decedents_status!='Unknown', ]



# Martial Status -----
sum(is.na(df$marital_status)) # none 
df$marital_status <- as.factor(df$marital_status)
barplot(table(df$marital_status), main='Mariage Status')
table(df$marital_status) # 20,104 unknown
#drop the unknowns 'U'
sum(df$marital_status == 'U')
df <- df[df$marital_status!='U', ]
df$marital_status <- droplevels(df$marital_status)


# Injury at work ----
sum(is.na(df$injury_at_work)) # none
barplot(table(df$injury_at_work), main='Injury at Work')
table(df$injury_at_work)
# I don't think this is a relevant variable


# Race -----------
sum(is.na(df$race)) # none
table(df$race)
race_codes <- c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78)
races <- c("White", "Black", "American Indian", "Chinese", " Japanese",
           "Hawaiian", "Filipino", "Asian Indian", "Korean", "Samoan",
           "Vietnamese", "Guamanaian", "Other Asian", "Other Asian")
for (i in 1:length(races)){
  df$race[df$race==race_codes[i]] <- races[i]
}
table(df$race)
par(mar = c(9, 6, 4, 2))
barplot(table(df$race), main='Race Breakdown', las=2)
par(mar = c(5, 4, 4, 2))


# Hispanic Origin Race (recode) ---------
sum(is.na(df$hispanic_originrace_recode)) #none
hispanic <- c("Mexican", "Puerto Rican", "Cuban", "C./S. American", 
              "Other Hispanic", "N", "N", "N", "N")
for (i in 1:9){
  if (i <= 5){
    df$hispanic_originrace_recode[df$hispanic_originrace_recode == i] <- hispanic[i]
  }
  else{
    df$hispanic_originrace_recode[df$hispanic_originrace_recode == i] <- "Non-Hispanic"
  }
}
table(df$hispanic_originrace_recode)
par(mar = c(9, 6, 4, 2))
barplot(table(df$hispanic_originrace_recode), las=2)
par(mar = c(5, 4, 4, 2))


# pause ----- 
dim(df)


# Pre Decode Entity Summary -----
df$entity_condition_1 <- as.character(df$entity_condition_1)
sum(is.na(df$entity_condition_1))
sum(df$entity_condition_1=="")

# missing conditions
vars <- paste0("entity_condition_", 1:20)
num_missing <- vector("list", 20)
for (i in 1:20){
  num_missing[i] <- sum(df[, vars[i]]=="")
  df[, vars[i]] <- substr(df[,vars[i]], 3, 5) # extract codes
}
cbind(vars, num_missing)
plot(1:20, num_missing, type='b', xlab='Number of Conditions',
     ylab='Number without condition', 
     main='Number of Observations\nWithough Conditions')
#NA means they are all not there
# we will likely not use all of the condition variables, as they are so space
# we should make a decision on how many. Maybe the first 3 or 4?

# Drop if Suicide or some type of accident, as cannot be predicted
df <- df[!(substr(df$entity_condition_1,1,1) %in% c('X','Y','Z')), ] # not many of them (529)

# Deep Dive into condition 1 -----
con1 <- df$entity_condition_1 # vector for analysis
con2 <- df$entity_condition_2
con3 <- df$entity_condition_3
length(unique(c(con1, con2))) # number of unique conditions in first place
# 1,470 unique conditions in the dataset

# see the most common
top <- table(con1)[rev(order(table(con1)))][1:20]
barplot(table(con1)[rev(order(table(con1)))][1:20])
top_codes <- names(table(con1)[rev(order(table(con1)))][1:20])
top_conds <- c()
for (code in top_codes){
  top_conds <- c(top_conds, get_hash_cond(code))
}

# Note I64 is Cerebrovascular diseases
# R09 is sudden infant death syndrome --> we should remove under 18 yrs old
# Might want to drop if R09, as already removed infants?
# I64 (the ERROR) is stroke
barplot(top, col=rainbow(length(top)), names.arg=rep("", 20),
        main="Most Common Diseases", xlab='Disease', ylab='Frequency')
legend('topright', legend=top_conds, fill=rainbow(20), cex=.6)


# Distribution plots ---------------
par(mfrow=c(3,2))
# Age
hist(df$detail_age, xlab="Age", main='Distribution of Age at Death', freq=F,
     col='gray')

# Education
labs <- names(sort(table(df$education_2003_revision), decreasing=T))
x <- barplot(sort(table(df$education_2003_revision), decreasing=T), 
             xaxt='n', main='Education Bar Plot')
text(cex=1,x = x, 
     y = rep(0, length(x)) - 30000, labs, xpd=T, srt=45, adj=1)

# Sex
M <- sum(df$sex=='M')
Fe <- sum(df$sex=='F')
pcts <- round(c(M/(M+Fe), Fe/(M+Fe))*100, 2)
slices <- c(M, Fe)
lbls <- c("Male", "Female")
pie(slices, labels=paste(lbls, paste0(pcts, "%")), main='Gender Breakdown', r=1)


# Marital Status
labs <- c("Married", "Widowed", "Divorced", "Single")
barplot(sort(table(df$marital_status), decreasing=T), 
        names.arg=labs, main='Marital Status')

# Race (White, Black, Other)
race <- df$race
race[!(race %in% c("White", "Black"))] <- "Other"
y <- round((table(race))/sum(table(race))*100, 2)
labs <- c()
for (i in 1:3){
  lab <- paste(names(y)[i], paste0(y[i],"%")) 
  labs <- c(labs, lab)
}
pie(table(race), r=1, labels = labs, main='Race (highlevel)')

# Race (Other Breakdown)
race <- df$race
race <- race[!(race %in% c("White", "Black"))]
t <- sort(table(race), decreasing = T)
x <- barplot(t, xaxt = 'n', main='Breakdown of "Other" Race')
text(x= x, y = rep(0, length(x))-500, xpd=T,
     labels=names(t), srt=45, adj=1)


# Disease Plots (run disease code block first) -----
dev.off()
par(mfrow=c(1,2))

plot(1:20, num_missing, type='b', xlab='Number of Conditions',
     ylab='Number without condition', 
     main='Number of Observations\nWithough Conditions')

barplot(top, col=rainbow(length(top)), names.arg=rep("", 20),
        main="Most Common Diseases", xlab='Disease', ylab='Frequency')
legend('topright', legend=top_conds, fill=rainbow(20), cex=.6)


# Data set for analysis
vars <- c("education_2003_revision", "sex", "detail_age", "place_of_death_and_decedents_status",
          "marital_status", "injury_at_work", paste0("entity_condition_", 1:6), 
          "race", "hispanic_origin")
df2 <- df[, colnames(df) %in% vars]
save(df2, file="preliminary_data_2015.Rda")

# Decoding Functions and Data ----------
# load hash table
load(file='Condition_table2.Rda')


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
    return(values(condition_dictionary[eval(code)]))
  }
  return(get_condition(code))
}



