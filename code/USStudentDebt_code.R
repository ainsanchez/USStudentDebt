#Include R libraries
library("tidyverse")
library("ggplot2")
library("openxlsx")
library("gridExtra")
library("viridis")
library("hrbrthemes")
library("sf")
library("terra")
library("spData")
library("rnaturalearth")
library("rnaturalearthhires")
library("car")
library("caret")

################################################################################
##
##                              INTRODUCTION                                ####
##
################################################################################

#Data Exploration of College Scorecard
collegeScorecard <- read.csv("BD/3.archive/Most-Recent-Cohorts-Scorecard-Elements.csv",
                             sep = ",")

#Data Exploration for the Most Recent NSLSD Elements part 1
nsldsElememntsP1 <- read.csv("BD/3.archive/Most-Recent-Cohorts-NSLDS-Elements_p1.csv",
                           sep = ",") #all variables

#Data Exploration for the Most Recent NSLSD Elements part 2
nsldsElememntsP2 <- read.csv("BD/3.archive/Most-Recent-Cohorts-NSLDS-Elements_p2.csv",
                           sep = ",") #all variables

#Data Exploration for the Most Recent NSLSD Elements part 3
nsldsElememntsP3 <- read.csv("BD/3.archive/Most-Recent-Cohorts-NSLDS-Elements_p3.csv",
                             sep = ",") #all variables

nsldsElememnts <- rbind(nsldsElememntsP1, nsldsElememntsP2, nsldsElememntsP3)


################################################################################
##
##                    SECTION 1: METHODS AND ANALYSIS                       ####
##
################################################################################

#Reduce the number of variables in the NSLDS dataset
varNamesDep <- read.xlsx("BD/varNamesDep.xlsx")
index <- which(names(nsldsElememnts) %in% varNamesDep$Varname)
nsldsElememnts <- nsldsElememnts[,index]
rm(varNamesDep, index)

#Include the variables from the Scorecard dataset
studentDebtDB <- nsldsElememnts %>% 
  left_join(collegeScorecard, by = "UNITID")

rm(collegeScorecard, nsldsElememnts)


################################################################################
##
##                             DATA-CLEANING                                ####
##
################################################################################

#Consistency Analysis 

#Delete SCH_DEG, REPAY_DT_MDN, REPAY_DT_N because of unconsistency and 
#incompleteness, and also delete variables repeated on the Scorecard dataset
studentDebtDB <- studentDebtDB[,-c(6,87:88,116:119,233:234)]

#Transform the numeric variables from the nslsdElements dataset
for (i in 6:112){
  studentDebtDB[,i] <- as.numeric(studentDebtDB[,i])
}

#Transform the numeric variables from the Scorecard dataset
for (i in (c(131:ncol(studentDebtDB)))){
  studentDebtDB[,i] <- as.numeric(studentDebtDB[,i])
}

#Transform the other variables to character
for (i in c(1:5, 113:130, 192, 204)){
  studentDebtDB[,i] <- as.character(studentDebtDB[,i])
}


#Completeness analysis

#Apply a filter to determine the currently operating institutions to
#compare it with the original dataset in terms of missing values. Also,
#include the institutions that have cohorts that entered repayment 7 years
#ago
studentDebtDBOpRpy7 <- studentDebtDB %>% 
  filter(CURROPER == "1" & !is.na(RPY_7YR_RT))

#Set a counter, to determine variables with less than 0.5% of missing values
counterDBOpRpy7 <- 0
indexNAOpRpy7 <- array()
counterDBOpRpy7Not <- 0
indexNotNAOpRpy7 <- array()

for (i in 1:ncol(studentDebtDBOpRpy7)) {
  if((sum(is.na(studentDebtDBOpRpy7[,i]))/nrow(studentDebtDBOpRpy7)) < 0.005){
    counterDBOpRpy7 <- counterDBOpRpy7 + 1
    indexNAOpRpy7[i] <- i
  }
}

indexNAOpRpy7 <- indexNAOpRpy7[!is.na(indexNAOpRpy7)]
studentDebtFinal <- studentDebtDBOpRpy7[,indexNAOpRpy7]


#Clarity Analysis - all variables seem well structured


################################################################################
##
##                             DATA-EXPLORING                               ####
##
################################################################################

# Apply summary statistics to numeric variables
for (i in 6:15){
  print(names(studentDebtFinal)[i])
  print(summary(studentDebtFinal[,i]))
}

# Construct frequency tables to non-numeric variables, when applicable

# Checking at the variable CITY
sum(duplicated(studentDebtFinal$CITY) == F)

for (i in c(17,20:23)){
  print(names(studentDebtFinal)[i])
  print(table(studentDebtFinal[,i], useNA = "always"))
}

################################################################################
##
##                             VISUALIZATION                                ####
##
################################################################################

# hist(studentDebtFinal$RPY_7YR_RT)
# qqPlot(studentDebtFinal$RPY_7YR_RT)

#Graph 1: relationship between the repayment rate and the state of the institution
tableGph1 <- studentDebtFinal %>%
  group_by(STABBR) %>%
  summarize(meanRpy7 = mean(RPY_7YR_RT, na.rm = T))

#Generate a heat map for the states in the US to show their repayment rate
usa <- st_as_sf(rnaturalearth::ne_states(country = "united states of america")) %>% 
  mutate(STABBR = postal) %>% 
  left_join(tableGph1, by = "STABBR") %>% 
  filter(postal != "AK" & postal != "HI") %>% 
  select(STABBR, meanRpy7, geometry)

graph1 <- ggplot(data = usa) +
  ggtitle("Repayment Rate across US States") +
  theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5, size = 20)) + 
  geom_sf( ) +
  aes(fill = meanRpy7, label = meanRpy7) +
  scale_fill_gradient(low = "gray", high = "blue") +
  geom_sf(alpha=0.8,col='white') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
graph1

# Graph 2: Relationship between family income and repayment rate
graph2 <- studentDebtFinal %>% 
  ggplot(aes(log(FAMINC), RPY_7YR_RT)) +
  ggtitle("Repayment Rate and Log of Family Income") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  geom_point()
graph2

# Graph 3: Relationship between median loan amount and repayment rate
graph3 <- studentDebtFinal %>% 
  ggplot(aes(log(INC_N))) +
  ggtitle("Distribution of students that report family income per institution") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  geom_histogram()
graph3

################################################################################
##
##                                INSIGHTS                                  ####
##
################################################################################

#Variable selection
predictorsIndM1 <- c(2:4)
predictorsIndM2 <- c(2:7)


################################################################################
##
##                           MODELING APPROACH                              ####
##
################################################################################

#Create the data partition for the modeling techniques

# Final hold-out test set will be 10% of MovieLens data
set.seed(1990, sample.kind="Rounding") # if using R 3.6 or later

studentDebtFinal <- studentDebtFinal %>% 
  group_by(STABBR) %>% 
  filter(n() >= 30) %>% 
  ungroup()

studentDebtFinal <- as.data.frame(studentDebtFinal)

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = studentDebtFinal$RPY_7YR_RT, times = 1, 
                                  p = 0.2, list = FALSE)
trainDB <- studentDebtFinal[-test_index,]
testDB <- studentDebtFinal[test_index,]

#Define the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm = T))
}


################################################################################
##
##                           SECTION 2: RESULTS                             ####
##
################################################################################

#Model 1
#(Repayment on average income, number of students reporting income and state)

#Prepare the training data
trainDBM1 <- trainDB %>%
  select(RPY_7YR_RT, INC_N, FAMINC, STABBR) %>%
  mutate(INC_N = as.numeric(scale(log(INC_N))),
         FAMINC = as.numeric(scale(log(FAMINC))),
         STABBR = as.factor(STABBR))

#Prepare the test data
testDBM1 <- testDB %>%
  select(RPY_7YR_RT, INC_N, FAMINC, STABBR) %>%
  mutate(INC_N = as.numeric(scale(log(INC_N))),
         FAMINC = as.numeric(scale(log(FAMINC))),
         STABBR = as.factor(STABBR))

#Run the glmboost model on the train dataset
train_glmboost <- train(trainDBM1[,predictorsIndM1], trainDBM1[,1],
                        method = "glmboost")

#Predict repayment on the test dataset
y_hat_glmboost <- predict(train_glmboost,
                          testDBM1[, predictorsIndM1])

#Display the performance metric of the model
results_glmboost <- RMSE(y_hat_glmboost, testDBM1$RPY_7YR_RT)

rmse_results <- data_frame(method = "Model 1: GLMBOOST - repayment on income and state",
                           RMSE = results_glmboost)

#------------------------------------------------------------------------------------------

#Model 2
#(Repayment on average income, number of students reporting income and state,
#average age of entry, type of school financing, and predominant degree awarded)

#Prepare the training data
trainDBM2 <- trainDB %>%
  select(RPY_7YR_RT, INC_N, FAMINC, STABBR, AGE_ENTRY, PREDDEG, CONTROL) %>%
  mutate(INC_N = as.numeric(scale(log(INC_N))),
         FAMINC = as.numeric(scale(log(FAMINC))),
         STABBR = as.factor(STABBR),
         AGE_ENTRY = as.numeric(scale(AGE_ENTRY)),
         PREDDEG = as.factor(PREDDEG),
         CONTROL = as.factor(CONTROL))

#Prepare the test data
testDBM2 <- testDB %>%
  select(RPY_7YR_RT, INC_N, FAMINC, STABBR, AGE_ENTRY, PREDDEG, CONTROL) %>%
  mutate(INC_N = as.numeric(scale(log(INC_N))),
         FAMINC = as.numeric(scale(log(FAMINC))),
         STABBR = as.factor(STABBR),
         AGE_ENTRY = as.numeric(scale(AGE_ENTRY)),
         PREDDEG = as.factor(PREDDEG),
         CONTROL = as.factor(CONTROL))

#Run the glmboost model on the train dataset
train_glmboost2 <- train(trainDBM2[,predictorsIndM2], trainDBM2[,1],
                         method = "glmboost")

#Predict repayment on the test dataset
y_hat_glmboost2 <- predict(train_glmboost2,
                           testDBM2[, predictorsIndM2])

#Display the performance metric of the model
results_glmboost2 <- RMSE(y_hat_glmboost2, testDBM2$RPY_7YR_RT)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2: GLMBOOST - repayment on income, state, age, school financing, and main degree type",
                                     RMSE = results_glmboost2))
rmse_results %>% knitr::kable()
