library(rstatix)
library(tidyverse)
library(HDoutliers)
library(nnet)
library(broom)

ddata <- read.csv("pivoted_data.csv")

#Removes NA values and gives mean per term for student
AverageChecker<- function (x){
  rowMeans(x, na.rm = TRUE)
}

# Removes NA Values and sums rows for each student if over 1
BinaryChecker <- function(x) { 
  x[is.na(x)] <- 0
  as.integer(rowSums(x) > 0)
}
# Removes NA Values and sums rows for each student
SummaryChecker <- function(x) { 
  x[is.na(x)] <- 0
  as.integer(rowSums(x))
}
# Recodes Values of gender to binary
ddata$Gender <- as.character(ddata$Gender)
ddata$Gender_Binary <- recode(ddata$Gender,
                              "Man" = "0",
                              "Woman" = "1",
                              .default = "1")

unique(ddata$Did_Graduate)
ddata$Did_Graduate_Binary <- recode(ddata$Did_Graduate,
                                    "No" = "0",
                                    "Yes, with another major" = "0",
                                    "Yes, with CS/IT/EMEC" = "1",
)


#Runs Binary/Summary/Average Checker 
ddata$Units_All <- SummaryChecker(ddata[, grep("_Unit_Load$", names(ddata))])
ddata$Housing_All <- BinaryChecker(ddata[, grep("_Lived_On_Campus$", names(ddata))])
ddata$Tutoring_All <- SummaryChecker(ddata[, grep("_Tutoring_Visits$", names(ddata))])
ddata$AverageUnits <- AverageChecker(ddata [,grep("_Unit_Load$", names(ddata))])
ddata$AverageTutoring <- AverageChecker(ddata[, grep("_Tutoring_Visits$", names(ddata))])
ddata$Tutoring_Binary <- BinaryChecker(ddata[, grep("_Tutoring_Visits$", names(ddata))])

# Adds & Calculates Terms Attended
ddata <- ddata %>% 
  rowwise() %>% 
  mutate(Terms_Attended = sum(!is.na(c_across(ends_with("_Unit_Load"))))) %>% 
  ungroup()

#Recode Variables to Factors


ddata$Is_Hispanic <- factor(ddata$Is_Hispanic,
                            levels = c(0,1),
                            labels = c("Non Hispanic", "Hispanic")
)

ddata$Gender_Binary <- factor(ddata$Gender_Binary,
                              levels = c(0,1),
                              labels = c("Male", "Female"))

ddata$Did_Graduate_Binary <- factor(ddata$Did_Graduate_Binary,
                                    levels = c(0, 1),
                                    labels = c("Did Not Graduate", "Graduated"))

ddata$Is_Transfer <- factor(ddata$Is_Transfer,
                            levels = c(0,1),
                            labels = c("Non Transfer", "Transfer"))

ddata$Housing_All <- factor(ddata$Housing_All,
                            levels = c(0,1),
                            labels = c("Non Housing", "Housing"))


ddata$Tutoring_Binary <- factor(ddata$Tutoring_Binary,
                            levels = c(0,1),
                            labels = c("No Tutoring", "Yes Tutoring"))


#Create new df with relevant variables
var_df <- select(ddata,Student,Is_Hispanic,Gender_Binary, Is_Transfer,
                       Did_Graduate_Binary, Units_All, Housing_All, 
                       Tutoring_All,AverageUnits, AverageTutoring, 
                       Tutoring_Binary,Terms_Attended)


#Detect Outliers: Percentile trimming
#Terms_Attended, Tutoring_All, AverageTutoring, Units_All, AverageUnits

trim_percentile <- function(x, lower = 0.01, upper = 0.99) {
  q <- quantile(x, c(lower, upper), na.rm = TRUE)
  x[x >= q[1] & x <= q[2]]
}

trimmed_df <- var_df %>%
  filter(
    between(Terms_Attended,
            quantile(Terms_Attended, .01, na.rm = TRUE),
            quantile(Terms_Attended, .99, na.rm = TRUE)),
    
    between(Tutoring_All,
            quantile(Tutoring_All, .01, na.rm = TRUE),
            quantile(Tutoring_All, .99, na.rm = TRUE)),
    
    between(AverageTutoring,
            quantile(AverageTutoring, .01, na.rm = TRUE),
            quantile(AverageTutoring, .99, na.rm = TRUE)),
    
    between(Units_All,
            quantile(Units_All, .01, na.rm = TRUE),
            quantile(Units_All, .99, na.rm = TRUE)),
    
    between(AverageUnits,
            quantile(AverageUnits, .01, na.rm = TRUE),
            quantile(AverageUnits, .99, na.rm = TRUE))
  )

nrow(var_df) #1080
nrow(trimmed_df) #1032
nrow(var_df) - nrow(trimmed_df) #48 rows removed

hist(trimmed_df$Tutoring_All)
hist(trimmed_df$Terms_Attended)

#HERE YOU GO!
library(readxl)
write.csv(trimmed_df, "trimmed_pivoted_df.csv", row.names = FALSE)

#Logistic Regression 
#ALL VARIABLES: Student,Is_Hispanic,Is_Transfer,Gender_Binary,
#Did_Graduate_Binary,Units_All, Housing_All,Tutoring_All,AverageUnits,
#AverageTutoring,Tutoring_Binary,Terms_Attended

#Model 1: Logistic Regression on Graduation
#Is_Hispanic, Gender_Binary, Is_Transfer, Housing_All, Tutoring_Binary

logdf1 <- select(trimmed_df, Is_Hispanic,Gender_Binary, Is_Transfer,
                 Housing_All, Tutoring_Binary, Did_Graduate_Binary)

lr1 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Is_Transfer
           + Housing_All + Tutoring_Binary,
           data = logdf1,
           family = binomial())
summary(lr1)
#Hispanic: Less likely to graduate
#Transfer: More likely to graduate
#Tutoring: More likely to graduate

#Convert to odds ratio (OR)
exp(coef(lr1))

#NO CONTI VARIABLES = NO ASSUMPTION OF LINEARITY

#Multicollinarity
library(car)
vif(lr1) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
library(lmtest)
lrtest(lr1)
#Model is significant

#Variance the model explains
install.packages("pscl")
library(pscl)
pR2(lr1)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 18% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr1) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf1$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf1$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#67.6% predictive accuracy



#Model 2: Logistic Regression on Graduation by Non-Transfer
#Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 

# Filter for non-transfer students first, then select relevant columns
logdf2 <- trimmed_df %>%
  filter(Is_Transfer == "Non Transfer") %>% # keep only non-transfer students
  select(Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 
         Did_Graduate_Binary)

lr2 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
           Tutoring_Binary,
           data = logdf2,
           family = binomial())
summary(lr2)
#Hispanic
#Housing
#Tutoring

#Convert to odds ratio (OR)
exp(coef(lr2))


#NO CONTI VARIABLES = NO ASSUMPTION OF LINEARITY

#Multicollinarity
vif(lr2) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr2)
#Model is significant

#Variance the model explains
pR2(lr2)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 14% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr2) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf2$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf2$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#72.15 predictive accuracy


#Model 3: Logistic Regression on Graduation by Transfer
#Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary 

# Filter for non-transfer students first, then select relevant columns
logdf3 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% # keep only Transfer students
  select(Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 
         Did_Graduate_Binary)

lr3 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
             Tutoring_Binary,
           data = logdf3,
           family = binomial())
summary(lr3)
#Tutoring: More likely 

#Convert to odds ratio (OR)
exp(coef(lr3))

#NO CONTI VARIABLES = NO ASSUMPTION OF LINEARITY

#Multicollinarity
vif(lr3) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr3)
#Model is significant

#Variance the model explains
pR2(lr3)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains .08% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr3) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf3$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf3$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#67% predictive accuracy


#Model 4: Logistic Regression on Graduation
#Is_Hispanic, Is_Transfer, Gender_Binary, Housing_All
#+Average Units

logdf4 <- select(trimmed_df, Is_Hispanic,Gender_Binary, Is_Transfer, 
                 Housing_All, Tutoring_Binary, Did_Graduate_Binary, 
                 AverageUnits)

lr4 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
             Tutoring_Binary + AverageUnits,
           data = logdf4,
           family = binomial())
summary(lr4)
#Hispanic: Less likely
#Female: Less likely
#Housing: Less likely
#Tutoring: More likely
#AverageUnits: More likely

#Convert to odds ratio (OR)
exp(coef(lr4))

#Linearity assumption
#Get predicted probabilities & logit
probabilities <- predict(lr4, type = "response")

linear_cols <- logdf4 %>% 
  select_if(is.numeric)

predictor <- colnames(linear_cols)  

linearity_assump <- linear_cols%>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity_assump, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Assumption violated?!

#Multicollinarity
vif(lr4) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr4)
#Model is significant

#Variance the model explains
pR2(lr4)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 11% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr4) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf4$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf4$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#64% predictive accuracy

#Model 5: Logistic Regression on Graduation by Transfer
#Is_Hispanic, Is_Transfer, Gender_Binary, Housing_All
#+Average Units
logdf5 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% # keep only Transfer students
  select(Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 
         Did_Graduate_Binary,AverageUnits)

lr5 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
             Tutoring_Binary + AverageUnits,
           data = logdf5,
           family = binomial())
summary(lr5)
#Tutoring: More likely
#AverageUnits: More likely

#Convert to odds ratio (OR)
exp(coef(lr5))

#Linearity assumption
#Get predicted probabilities & logit
probabilities <- predict(lr5, type = "response")

linear_cols <- logdf5 %>% 
  select_if(is.numeric)

predictor <- colnames(linear_cols)  

linearity_assump <- linear_cols%>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity_assump, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Assumption violated?!
#Boxtidwell

logdf5$Did_Graduate_Binary_num <- ifelse(logdf5$Did_Graduate_Binary == "Graduated", 1, 0)

boxTidwell(Did_Graduate_Binary_num ~ AverageUnits, data = logdf5)
#Violated, sigh...

#Multicollinarity
vif(lr5) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr5)
#Model is significant

#Variance the model explains
pR2(lr5)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 13% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr5) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf5$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf5$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#68% predictive accuracy


#Model 6: Logistic Regression on Graduation by NonTransfer
#Is_Hispanic, Is_Transfer, Gender_Binary, Housing_All
#+Average Units
logdf6 <- trimmed_df %>%
  filter(Is_Transfer == "Non Transfer") %>% # keep only Transfer students
  select(Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 
         Did_Graduate_Binary,AverageUnits)

lr6 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
             Tutoring_Binary + AverageUnits,
           data = logdf6,
           family = binomial())
summary(lr6)
#Hispanic: less likely
#Housing: Less likely
#AverageUnits: More likely

#Convert to odds ratio (OR)
exp(coef(lr6))

#Linearity assumption
#Get predicted probabilities & logit
probabilities <- predict(lr6, type = "response")

linear_cols <- logdf6 %>% 
  select_if(is.numeric)

predictor <- colnames(linear_cols)  

linearity_assump <- linear_cols%>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity_assump, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Assumption violated?!
#Boxtidwell

logdf6$Did_Graduate_Binary_num <- ifelse(logdf6$Did_Graduate_Binary == "Graduated", 1, 0)

boxTidwell(Did_Graduate_Binary_num ~ AverageUnits, data = logdf6)
#Not violated! WOOO!

#Multicollinarity
vif(lr6) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr6)
#Model is significant

#Variance the model explains
pR2(lr6)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 15% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr6) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf6$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf6$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#73% predictive accuracy

#Model 7: Logistic Regression on Graduation by Transfer
#Is_Hispanic, Is_Transfer, Gender_Binary, Housing_All
#+Average Units
logdf7 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% # keep only Transfer students
  select(Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 
         Did_Graduate_Binary,AverageUnits)

lr7 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
             Tutoring_Binary + AverageUnits,
           data = logdf7,
           family = binomial())
summary(lr7)
#Tutoring: More likely
#AverageUnits: Less likely

#Convert to odds ratio (OR)
exp(coef(lr7))

#Linearity assumption
#Get predicted probabilities & logit
probabilities <- predict(lr7, type = "response")

linear_cols <- logdf7 %>% 
  select_if(is.numeric)

predictor <- colnames(linear_cols)  

linearity_assump <- linear_cols%>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity_assump, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Assumption violated?!
#Boxtidwell

logdf7$Did_Graduate_Binary_num <- ifelse(logdf7$Did_Graduate_Binary == "Graduated", 1, 0)

boxTidwell(Did_Graduate_Binary_num ~ AverageUnits, data = logdf7)
#Violated! 

#Multicollinarity
vif(lr7) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr7)
#Model is significant

#Variance the model explains
pR2(lr7)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 13% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr7) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf7$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf7$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#68% predictive accuracy


#Model 8: Logistic Regression on Graduation
#Is_Hispanic, Is_Transfer, Gender_Binary, Housing_All
#+Average Units (Categorical Predictor)

logdf8 <- trimmed_df %>%
  select(Is_Hispanic, Gender_Binary, Housing_All, Tutoring_Binary, 
         Did_Graduate_Binary,AverageUnits)

logdf8$AverageUnits_bin <- cut(logdf8$AverageUnits,
                               breaks = c(0, 8, 11, 15, 18),
                               include.lowest = TRUE,
                               labels = c("Very Low", "Low", 
                                          "Medium", "High"))

#Very Low: 6-8, Low: 9-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf8$AverageUnits_bin)# Check counts

#Medium as reference
logdf8$AverageUnits_bin <- factor(logdf8$AverageUnits_bin,
                                  levels = c("Medium", "Very Low", "Low", "High"))


lr8 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All + 
             Tutoring_Binary + AverageUnits_bin,
           data = logdf8,
           family = binomial())
summary(lr8)
#Hispanic: Less likely
#Female: Less
#Housing:Less
#Very low: Less

#Convert to odds ratio (OR)
exp(coef(lr8))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr8) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr8)
#Model is significant

#Variance the model explains
pR2(lr8)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 17% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr8) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf8$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf8$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#65% predictive accuracy

#Model 9: Logistic Regression on Graduation by Transfer
#Is_Hispanic, Gender_Binary, Housing_All
#+Average Units (Categorical Predictor)

logdf9 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All, 
         Did_Graduate_Binary,AverageUnits)

logdf9$AverageUnits_bin <- cut(logdf9$AverageUnits,
                               breaks = c(0, 8, 11, 15, 18),
                               include.lowest = TRUE,
                               labels = c("Very Low", "Low", 
                                          "Medium", "High"))

#Very Low: 6-8, Low: 9-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf9$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf9$AverageUnits_bin <- factor(logdf9$AverageUnits_bin,
                                  levels = c("Medium", "Very Low", "Low", "High"))


lr9 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All +
           AverageUnits_bin,
           data = logdf9,
           family = binomial())
summary(lr9)
#Very low: Less
#High: less

#Convert to odds ratio (OR)
exp(coef(lr9))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr9) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr9)
#Model is significant

#Variance the model explains
pR2(lr9)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 12% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr9) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf9$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf9$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#67% predictive accuracy


#Model 10: Logistic Regression on Graduation by NonTransfer
#Is_Hispanic, Gender_Binary, Housing_All
#+Average Units (Categorical Predictor)

table(trimmed_df$Tutoring_All)
logdf10 <- trimmed_df %>%
  filter(Is_Transfer == "Non Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All, 
         Did_Graduate_Binary,AverageUnits)

logdf10$AverageUnits_bin <- cut(logdf10$AverageUnits,
                               breaks = c(0, 11, 15, 18),
                               include.lowest = TRUE,
                               labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf10$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf10$AverageUnits_bin <- factor(logdf10$AverageUnits_bin,
                                  levels = c("Fulltime", "Parttime", "Heavy"))


lr10 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All +
             AverageUnits_bin,
           data = logdf10,
           family = binomial())
summary(lr10)
#Hispanic: low
#Housing: low
#Partime: low

#Convert to odds ratio (OR)
exp(coef(lr10))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr10) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr10)
#Model is significant

#Variance the model explains
pR2(lr10)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 7% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr10) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf10$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf10$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#70% predictive accuracy


#Model 11: Logistic Regression on Graduation
#Is_Hispanic, Gender_Binary, Housing_All, Is_Transfer
#Tutoring_All (Categorical Predictor)

logdf11 <- select(trimmed_df, Is_Hispanic,Gender_Binary, Is_Transfer, 
                 Housing_All, Tutoring_All, Did_Graduate_Binary)

logdf11$Tutoring_bin <- cut(logdf11$Tutoring_All,
                          breaks = c(-1, 0, 5, 10, 20, Inf),
                          include.lowest = TRUE,
                          labels = c("No Tutoring", "Low", "Moderate", "High", "Very High"))

#No tutoring: 
#low: 1-5
#moderate: 6-10
#high: 11-20
#very high:
table(logdf11$Tutoring_bin)
levels(logdf11$Tutoring_bin)

lr11 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All +
              Tutoring_bin +Is_Transfer,
            data = logdf11,
            family = binomial())
summary(lr11)
#Hispanic: low
#Moderate: high
#Low: High
#Very High: high
#IS_Transfer: High

#Convert to odds ratio (OR)
exp(coef(lr11))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr11) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr11)
#Model is significant

#Variance the model explains
pR2(lr11)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 12% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr11) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf11$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf11$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#69% predictive accuracy


#Model 12: Logistic Regression on Graduation
#Is_Hispanic, Gender_Binary, Housing_All, Is_Transfer
#Tutoring_All (Categorical Predictor) + AverageUnits (Categorical)

logdf12 <- trimmed_df %>%
  select(Is_Hispanic, Gender_Binary, Housing_All, Is_Transfer,
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf12$AverageUnits_bin <- cut(logdf12$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf12$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf12$AverageUnits_bin <- factor(logdf12$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf12$Tutoring_bin <- cut(logdf12$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf12$Tutoring_bin)
levels(logdf12$Tutoring_bin)

lr12 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All +
              Tutoring_bin +AverageUnits_bin + Is_Transfer,
            data = logdf12,
            family = binomial())
summary(lr12)
#Hispanic: low
#Gender: low
#Housing: low
#Moderate: high
#High: high
#Low: High
#Parttime: Low
#Transfer: High


#Convert to odds ratio (OR)
exp(coef(lr12))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr12) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr12)
#Model is significant

#Variance the model explains
pR2(lr12)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 26% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr12) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf12$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf12$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#70% predictive accuracy



#Model 13: Logistic Regression on Graduation by NonTransfer
#Is_Hispanic, Gender_Binary, Housing_All
#Tutoring_All (Categorical Predictor) + AverageUnits (Categorical)

logdf13 <- trimmed_df %>%
  filter(Is_Transfer == "Non Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All, 
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf13$AverageUnits_bin <- cut(logdf13$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf13$AverageUnits_bin)# Check counts
#High islow :/

#Medium as reference
logdf13$AverageUnits_bin <- factor(logdf13$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf13$Tutoring_bin <- cut(logdf13$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf13$Tutoring_bin)
levels(logdf13$Tutoring_bin)

lr13 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All +
              Tutoring_bin +AverageUnits_bin,
            data = logdf13,
            family = binomial())
summary(lr13)
#Hispanic: low
#Housing: low
#Moderate: high
#High: high
#Low: High
#Parttime: low

#Convert to odds ratio (OR)
exp(coef(lr13))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr13) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr13)
#Model is significant

#Variance the model explains
pR2(lr13)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 20% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr13) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf13$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf13$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#74% predictive accuracy


#Model 14: Logistic Regression on Graduation by Transfer
#Is_Hispanic, Gender_Binary, Housing_All
#Tutoring_All (Categorical Predictor) + AverageUnits (Categorical)

logdf14 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All, 
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf14$AverageUnits_bin <- cut(logdf14$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf14$AverageUnits_bin)# Check counts
#High islow :/

#Medium as reference
logdf14$AverageUnits_bin <- factor(logdf14$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf14$Tutoring_bin <- cut(logdf14$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf14$Tutoring_bin)
levels(logdf14$Tutoring_bin)

lr14 <- glm(Did_Graduate_Binary ~ Is_Hispanic + Gender_Binary + Housing_All +
              Tutoring_bin +AverageUnits_bin,
            data = logdf14,
            family = binomial())
summary(lr14)
#Moderate: high
#High: high
#Low: High
#Parttime: low
#heavy: low

#Convert to odds ratio (OR)
exp(coef(lr14))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr14) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr14)
#Model is significant

#Variance the model explains
pR2(lr14)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 14% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr14) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf14$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf14$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#74% predictive accuracy

#Model 15: Logistic Regression on Graduation (Interaction)
#Is_Hispanic, Gender_Binary, Housing_All, Is_Transfer, Tutoring_All (Categorical Predictor) AverageUnits (Categorical)
#Is_Hispanic * Tutoring_bin

logdf15 <- trimmed_df %>%
  select(Is_Hispanic, Gender_Binary, Housing_All, Is_Transfer,
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf15$AverageUnits_bin <- cut(logdf15$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf15$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf15$AverageUnits_bin <- factor(logdf15$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf15$Tutoring_bin <- cut(logdf15$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf15$Tutoring_bin)
levels(logdf15$Tutoring_bin)

lr15 <- glm(Did_Graduate_Binary ~ Is_Hispanic * Tutoring_bin + Gender_Binary +
              AverageUnits_bin +Is_Transfer,
            data = logdf15, 
            binomial()
            )

summary(lr15)


#Convert to odds ratio (OR)
exp(coef(lr15))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr15) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr15)
#Model is significant

#Variance the model explains
pR2(lr15)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 30% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr15) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf15$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf15$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#70% predictive accuracy


#Model 16: Logistic Regression on Graduation (Interaction) by Transfer
#Gender_Binary, Average Units
#Is_Hispanic * Tutoring_bin, Tutoring_bin *Housing_All,

logdf16 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All,
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf16$AverageUnits_bin <- cut(logdf16$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf16$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf16$AverageUnits_bin <- factor(logdf16$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf16$Tutoring_bin <- cut(logdf16$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf16$Tutoring_bin)
levels(logdf16$Tutoring_bin)

lr16 <- glm(Did_Graduate_Binary ~ Is_Hispanic * Tutoring_bin + Gender_Binary +
              AverageUnits_bin + Housing_All,
            data = logdf16, 
            binomial()
)

summary(lr16)


#Convert to odds ratio (OR)
exp(coef(lr16))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr16) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr16)
#Model is significant

#Variance the model explains
pR2(lr16)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 16% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr16) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf16$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf16$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#65% predictive accuracy

#Model 16: Logistic Regression on Graduation (Interaction) by Transfer
#Gender_Binary, Average Units
#Is_Hispanic * Tutoring_bin, Tutoring_bin *Housing_All,

logdf16 <- trimmed_df %>%
  filter(Is_Transfer == "Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All,
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf16$AverageUnits_bin <- cut(logdf16$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf16$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf16$AverageUnits_bin <- factor(logdf16$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf16$Tutoring_bin <- cut(logdf16$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf16$Tutoring_bin)
levels(logdf16$Tutoring_bin)

lr16 <- glm(Did_Graduate_Binary ~ Is_Hispanic * Tutoring_bin + Gender_Binary +
              AverageUnits_bin,
            data = logdf16, 
            binomial()
)

summary(lr16)


#Convert to odds ratio (OR)
exp(coef(lr16))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr16) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr16)
#Model is significant

#Variance the model explains
pR2(lr16)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 14% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr16) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf16$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf16$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#65% predictive accuracy



#Model 17: Logistic Regression on Graduation (Interaction) by NonTransfer
#Gender_Binary, Average Units
#Is_Hispanic * Tutoring_bin, Tutoring_bin *Housing_All,

logdf17 <- trimmed_df %>%
  filter(Is_Transfer == "Non Transfer") %>% 
  select(Is_Hispanic, Gender_Binary, Housing_All,
         Did_Graduate_Binary,AverageUnits,Tutoring_All)

#AverageUnits Binning
logdf17$AverageUnits_bin <- cut(logdf17$AverageUnits,
                                breaks = c(0, 11, 15, 18),
                                include.lowest = TRUE,
                                labels = c("Parttime", "Fulltime", "Heavy"))

#Part-time: 6-11, Medium(Full Time): 12-15, Heavy: 16-17
table(logdf17$AverageUnits_bin)# Check counts
#High is super low :/

#Medium as reference
logdf17$AverageUnits_bin <- factor(logdf17$AverageUnits_bin,
                                   levels = c("Fulltime", "Parttime", "Heavy"))

#Tutoring_All binning

logdf17$Tutoring_bin <- cut(logdf17$Tutoring_All,
                            breaks = c(-1, 0, 5, 10,Inf),
                            include.lowest = TRUE,
                            labels = c("No Tutoring", "Low", "Moderate",
                                       "High"))

#No tutoring, low: 1-5, moderate: 6-10, high: over 11?
table(logdf17$Tutoring_bin)
levels(logdf17$Tutoring_bin)

lr17 <- glm(Did_Graduate_Binary ~ Is_Hispanic * Tutoring_bin + Gender_Binary +
              AverageUnits_bin + Housing_All,
            data = logdf17, 
            binomial()
)

summary(lr17)


#Convert to odds ratio (OR)
exp(coef(lr17))

#NO CONTINOUS VARIABLES 

#Multicollinarity
vif(lr17) #VIF < 5

#Omnibus test
#Check if the model with predictors fits better than null model
lrtest(lr17)
#Model is significant

#Variance the model explains
pR2(lr17)
#r2ML = Cox & Snell
#r2CU = Nagelkerke
#Model explains 20% of variation

#Percent Correct Classification
#Predicted probabilities
pred <- ifelse(fitted(lr17) > 0.5, 1, 0)

#Actual vs predicted
table(Predicted = pred, Actual = ifelse(logdf17$Did_Graduate_Binary == "Graduated", 1, 0))

# Percent correct classification
mean(pred == ifelse(logdf17$Did_Graduate_Binary == "Graduated", 1, 0)) * 100
#75% predictive accuracy
