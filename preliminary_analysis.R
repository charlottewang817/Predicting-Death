# preliminary_analysis.R
# Kenneth Lipke

setwd("~/Documents/ORIE 4741 Big Messy Data/Project")

library(tree)

load("preliminary_data_2015.Rda") # imports as df2

# make appropriate factors
for (var in paste0("entity_condition_", 1:6)){
  df2[, var] <- as.factor(df2[,var])
}

df2$education_2003_revision <- as.factor(df2$education_2003_revision)
df2$place_of_death_and_decedents_status <- as.factor(df2$place_of_death_and_decedents_status)
df2$race <- as.factor(df2$race)
df2 <- df2[!(substr(df2$entity_condition_1,1,1) %in% c('X','Y','Z','S','V', 'T')), ] # not many of them (529)
# hispanic origin is messed up

# entity_condition_1 matrix
m <- model.matrix(~entity_condition_1  -1, df2)

library(dplyr)
# test making binary
mini <- df2[1:100, ]
s <- spread(data=mini, key=entity_condition_1, value=entity_condition_1)
s <- s[, !(colnames(s) %in% paste0("entity_condition_", 2:6))]
t <- tree(s$detail_age~.)

library(data.table)
s2 <- mini
s2 <- setDT(s2)[, c(levels(s2$entity_condition_1), "entity_condition_1") := 
            c(lapply(levels(c), function(x) as.integer(x == c)), .(NULL))]


# THIS WILL PROBABLY WORK WHEN WE SCALE IT UP!
# Model matrix approach and merge
mini <- df2[1:100, ]
mini_m <- model.matrix(~entity_condition_1 -1, mini) # make the binary matrix
mini_m <- lapply(data.frame(mini_m), as.factor) # make it data frame with factor columns
mini <- mini[, !(colnames(mini) %in% paste0("entity_condition_", 1:6))]
mini_2 <- cbind(mini, mini_m)

# try for realz
df3 <- df2[1:round(nrow(df2)/10),]
#drop unused or lightly uses factors
df3 <- df3[df3$entity_condition_1 %in% names(which(table(df3$entity_condition_1)>500)), ]
df3$entity_condition_1 <- droplevels(df3$entity_condition_1)

mat <- model.matrix(~entity_condition_1 -1, df3)
mat <- lapply(data.frame(mat), as.factor)
mat <- data.frame(mat)
df <- df3[, !(colnames(df3) %in% paste0("entity_condition_", 1:6))]
df <- cbind(df, mat)
df4 <- cbind(df$detail_age, mat)
names(df4)[1] <- "detail_age"

train <- sample(1:nrow(df4), nrow(df4)/2)
train_df <- df4[train, ]
test_df <- df4[-train,]
t <- tree(detail_age~., train_df,mindev=0.005,  split="deviance")
plot(t)
text(t)

prediction <- predict(t, test_df)
truth <- test_df$detail_age
mean((prediction-truth)^2) # MSE (meaningless in absense of other, but we could say that?)

# figure out how to re-label columns so they can be plotted
colnames(df4)

# you need to drop diseases that show up less than say 50 times
colnames(df)[9:length(colnames(df))] 
for (i in 9:length(colnames(df))){
  colnames(df)[i] <- substr(colnames(df)[i], 19, 21)
}


#THE TREE WITH DF IS FUCKING LITTT
t <- tree(detail_age~., df, subset=train, mindev=0.01, split="deviance")
plot(t)
text(t)
summary(t)

# rpart model, just a try
#library(rpart)
#library(rpart.plot)
t_r <- rpart(detail_age ~ ., data=df, subset=train, cp=0.001, maxdepth=5)
rpart.plot(t_r, cex=.7, type=2)
prediction_r <- predict(t_r, data=df, subset=test)
truth <- df$detail_age[-train]
mean((prediction_r - truth)^2)
# Great thing to talk about is the preliminary splits and the greediness
# How we see a lot of teh conditions cropping up in 3rd and 4th place
# this is RIPE for a random forset where at each step the number
# of variables selected at each step is limited. 

# drop hispanic origin
df <- df[, colnames(df) !="injury_at_work"]
df <- df[, colnames(df) !="place_of_death_and_decedents_status"]

set.seed(1)
# train and test indicies
train = sample(1:nrow(df2), nrow(df2)/1000)
m_train <- m[train, ]

t <- tree(df2$detail_age[train]~m_train)
summary(t)
plot(t)
text(t, pretty=0)


#tree.df2 = tree(detail_age~., df2, subset=train)
summary(tree.df2)

plot(tree.df2)
text(tree.df2, pretty=0)


##### ACTUAL PRELIMINARY ANALYSIS START TO FINISH------------------------
setwd("~/Documents/ORIE 4741 Big Messy Data/Project")

library(tree)
library(rpart)
library(rpart.plot)

# load in the data
load("preliminary_data_2015.Rda") # imports as df2

# make appropriate factors
for (var in paste0("entity_condition_", 1:6)){
  df2[, var] <- as.factor(df2[,var])
}
df2$education_2003_revision <- as.factor(df2$education_2003_revision)
df2$place_of_death_and_decedents_status <- as.factor(df2$place_of_death_and_decedents_status)
df2$race <- as.factor(df2$race)
df2 <- df2[!(substr(df2$entity_condition_1,1,1) %in% c('X','Y','Z','S','V', 'T')), ] # not many of them (529)
# hispanic origin is messed up


# clean the data
df3 <- df2[1:round(nrow(df2)/10),] # don't use all
#drop unused or lightly uses factors
df3 <- df3[df3$entity_condition_1 %in% names(which(table(df3$entity_condition_1)>500)), ]
df3$entity_condition_1 <- droplevels(df3$entity_condition_1)

# make the matrix
mat <- model.matrix(~entity_condition_1 -1, df3)
mat <- lapply(data.frame(mat), as.factor) # make the matrix factors
mat <- data.frame(mat) # make it a data frame
df <- df3[, !(colnames(df3) %in% paste0("entity_condition_", 1:6))] #drop columns
df <- cbind(df, mat) # combine with other data

# re-name some columns
colnames(df)[9:length(colnames(df))] 
for (i in 9:length(colnames(df))){
  colnames(df)[i] <- substr(colnames(df)[i], 19, 21)
}

# drop hispanic origin
df <- df[, colnames(df) != "hispanic_origin"]
df <- df[, colnames(df) !="injury_at_work"]
df <- df[, colnames(df) !="place_of_death_and_decedents_status"]

set.seed(1)
# train and test indicies
train = sample(1:nrow(df), round(.8*nrow(df),0))

t_r <- rpart(detail_age ~ ., data=df, subset=train, cp=0.001, maxdepth=5)
rpart.plot(t_r, tweak=2, type=2, main="Single Regreesion Tree")
df_test <- df[-train, ]
prediction_r <- predict(t_r, newdata=df_test)
truth <- df_test$detail_age
mean((prediction_r - truth)^2)
# Great thing to talk about is the preliminary splits and the greediness
# How we see a lot of teh conditions cropping up in 3rd and 4th place
# this is RIPE for a random forset where at each step the number
# of variables selected at each step is limited. 
# Note intesity of color ~ value 
