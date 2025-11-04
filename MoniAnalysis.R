library(tidyverse)
library(car)
library(clintools)

ddata <- read.csv("DeDuped_Data.csv")

#################
# Data Recoding #
#################

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


# Recodes Values of gender 
ddata$Gender <- as.character(ddata$Gender)
ddata$Gender_Binary <- recode(ddata$Gender,
                              "Man" = "0",
                              "Woman" = "1",
                              .default = "1")

#Runs Binary/Summary Checker 
ddata$Did_Graduate_All <- BinaryChecker(ddata[, grep("_Graduate$", names(ddata))])
ddata$Housing_All <- BinaryChecker(ddata[, grep("_Lived_On_Campus$", names(ddata))])
ddata$Tutoring_All <- SummaryChecker(ddata[, grep("_Tutoring_Visits$", names(ddata))])

#Average units per term
ddata$Units_Avg <- rowMeans(ddata[, grep("_Unit_Load$", names(ddata))], na.rm = TRUE)


# Adds & Calculates Terms Attended
ddata <- ddata %>% 
  rowwise() %>% 
  mutate(Terms_Attended = sum(!is.na(c_across(ends_with("_Unit_Load"))))) %>% 
  ungroup()

#######################
# Logistic Regression #
#######################

#Select variables
lr1.df <- ddata %>% 
  select(Did_Graduate_All, Is_Hispanic, Gender_Binary, Is_Transfer, Housing_All,Tutoring_All, Units_Avg)

sum(is.na(lr1.df)) #Check for missing values
str(lr1.df) #Check data types
lr1.df$Gender_Binary <- as.integer(lr1.df$Gender_Binary) # Convert to integar


#Fit model predicting graduation
################################
#Outcome: Did_Graduate_All (binary: 0 = did not graduate, 1 = graduated)
#Predictors:
#Is_Hispanic (binary)
#Gender_Binary (0 = man, 1 = woman)
#Is_Transfer (binary)
#Housing_All (0 = did not live on campus, 1 = lived on campus at least once)
#Tutoring_All (total tutoring visits, continous)
#Units_Avg (average units per term, continuous)

lr1<- glm(Did_Graduate_All ~ Is_Hispanic + Gender_Binary + Is_Transfer + Housing_All + Tutoring_All + Units_Avg,
           data = lr1.df,
           family = binomial())

summary(lr1)

#Ortable: Odds ratios, confidence intervals, and p-values
ortable(lr1, d = 2, d_p = 3, intercept = TRUE, simple = TRUE)

# Linearity of the logit for cont. predictors
#############################################
#Cont. Predictors: Tutoring_All, Units_Avg

boxTidwell(Did_Graduate_All ~ Units_Avg, data = lr1.df) #Violated

#Manual Box-Tidwell Test due to Zeros
Tutoring <- glm(Did_Graduate_All ~ Tutoring_All + I(Tutoring_All * log1p(Tutoring_All)), 
    family = binomial, data = lr1.df)
summary(Tutoring) #Violated

#Linearity Plots
#################
#Predicted logit (log-odds)
lr1.df$pred_logit <- predict(lr1, type = "link")  # type = "link" gives logit

#Plot: Predicted logit vs Tutoring_All
ggplot(lr1.df, aes(x = Tutoring_All, y = pred_logit)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(x = "Total Tutoring Visits", y = "Predicted Logit")

#This plot is really weird, time to investigate!
sum(lr1.df$Tutoring_All == 0) #538 
sum(lr1.df$Tutoring_All > 0) #1028
summary(lr1.df$Tutoring_All) #min: 0 , mean: 12.92, max: 822 

#Boxplot to reveal outliers
ggplot(lr1.df, aes(y = Tutoring_All)) +
  geom_boxplot() +
  labs(y = "Tutoring Visits") 

#^Realized this is what Cook's D is for
cooksd <- cooks.distance(lr1)
lr1.df$cooks <- cooksd

n <- nrow(lr1.df)
ip <- which(cooksd > (4 / n)) # 4/n common cutoff in regression 
ip

plot(cooksd, type = "h", main = "Cook's Distance", #h = histogram-like vertical lines
     ylab = "Cook's D", xlab = "Observation Index")
abline(h = 4/n, col = "red")

#Plot: Predicted logit vs Units_Avg
ggplot(lr1.df, aes(x = Units_Avg, y = pred_logit)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Average Units Per Semester", y = "Predicted Logit")


#Assumption of Multicollinearity
################################
vif(lr1)

#Model fit
nmodel <- glm(Did_Graduate_All ~ 1, data = ddata, family = binomial)
anova(nmodel, lr1, test = "Chisq")


