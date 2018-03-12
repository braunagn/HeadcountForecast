library(ggplot2)
library(pROC)
library(devtools)
library(broom)

set.seed(1923874)
file_path = "C:/Users/nickb/Documents/Facebook/roster_only.csv"
df = read.table(file_path, sep=",", header=T, stringsAsFactors=T)

# categorize job satisfaction (0 or 1)
job_sat_yes <- c('Highly Satisfied')
job_sat_no <- c('Somewhat Satisfied','Not Satisfied','Satisfied')
df$TARGET[which(df$JOB_SATISFACTION %in% job_sat_yes)] <- 1
df$TARGET[which(df$JOB_SATISFACTION %in% job_sat_no)] <- 0

col = 'DISTANCE_FROM_HOME_MILES'
low <- df$DISTANCE_FROM_HOME_MILES <= quantile(df$DISTANCE_FROM_HOME_MILES, c(0.50))

med <- df$DISTANCE_FROM_HOME_MILES > quantile(df$DISTANCE_FROM_HOME_MILES, c(0.50)) &
  df$DISTANCE_FROM_HOME_MILES <= quantile(df$DISTANCE_FROM_HOME_MILES, c(0.75))

# med_high <- df$DISTANCE_FROM_HOME_MILES > quantile(df$DISTANCE_FROM_HOME_MILES, c(0.50)) &
#   df$DISTANCE_FROM_HOME_MILES <= quantile(df$DISTANCE_FROM_HOME_MILES, c(0.60))

high <- df$DISTANCE_FROM_HOME_MILES > quantile(df$DISTANCE_FROM_HOME_MILES, c(0.75)) &
  df$DISTANCE_FROM_HOME_MILES <= quantile(df$DISTANCE_FROM_HOME_MILES, c(1.0))

df$TEST_COL[low] <- 'low'
# df$TEST_COL[med_low] <- 'med-low'
df$TEST_COL[med] <- 'med'
df$TEST_COL[high] <- 'high'


# split train and test data set
train_size = .75
sample <- sample.int(n=nrow(df), size=floor(nrow(df)*train_size), replace=F)

# down select to only those columns to be used in model
these_cols <-   c('TARGET',
                  'TEST_COL'
                  )

train <- df[sample, these_cols]
test <- df[-sample, these_cols]

model <- glm(TARGET ~., family=binomial(link='logit'), data=train)
summary(model)


# prob=predict(object=model, type=c("response"), newdata=test)
# train$prob=prob
# g <- roc(TARGET ~ prob, data = test, plot=T, auc=T)
# plot(g)


# auc(g)

# write model output to csv file
# write.csv(tidy(model), "C:/Users/nickb/Documents/DataMining_RGA/Top Performers/model_coefficients.csv")
