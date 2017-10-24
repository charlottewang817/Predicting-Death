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
# hispanic origin is messed up

# entity_condition_1 matrix
m <- model.matrix(~entity_condition_1  -1, df2)


# test making binary
mini <- df2[1:100, ]
s <- spread(data=mini, key=entity_condition_1, value=entity_condition_1)
s <- s[, !(colnames(s) %in% paste0("entity_condition_", 2:6))]
t <- tree(s$detail_age~.)

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
