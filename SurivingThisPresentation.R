# Initialization
###################
library(survival) # For Survival
library(survminer) # For Suvival Plots
library(ggsurvfit)  # For Suvival Plots
library(timereg) # 
library(tidyverse)
# Others (Dont Know Which I need)
library(lubridate)
library(gtsummary)
library(tidycmprsk)



ddata <- read.csv("clean_reorder.csv")

# Filter the data frame
ddata <- ddata %>%
  filter(!Student %in% exclude_ids)

##########################
# Cleaned and Reordered  #
##########################
  # Re-codes Values of Male/Female to Binary
    ddata$Gender_Binary <- as.character(recode(ddata$Gender,
                                "Man" = "0",
                                "Woman" = "1",
                                .default = "1"))
  
  # Re-codes Values of Graduation to Binary
    ddata$Did_Graduate_Binary <- ifelse(ddata$Did_Graduate == ddata$Term_Code, 1, 0)



str(ddata)  
ddata <- select(ddata, Student, Is_Hispanic, Is_Transfer, Lived_On_Campus, Num_Tutoring_Visits, Gender_Binary, Did_Graduate_Binary)

# Adds Start/Stop Sequences
ddata <- ddata %>%
  group_by(Student) %>% 
  mutate(
    start = seq(0, n()-1),
    stop = seq(1, n())
  )
 
ddata$Is_Hispanic <- factor(ddata$Is_Hispanic)




ddata$Gender_Binary <- factor(ddata$Gender_Binary)
ddata$Did_Graduate_Binary <- factor(ddata$Did_Graduate_Binary)
ddata$Is_Transfer <- factor(ddata$Is_Transfer)
ddata$Lived_On_Campus <- factor(ddata$Is_Transfer)
  
ddata$Num_Tutoring_Visits <- as.numeric(ddata$Num_Tutoring_Visits)  

sdata <- ddata




######################
# Survival Analysis  #
######################

# Cox Proportional Hazards Model w/All Variables 
CoxAll <- coxph(Surv(time = start, time2 = stop, event = Did_Graduate_Binary) 
                ~  Num_Tutoring_Visits + Is_Hispanic + Gender_Binary + Lived_On_Campus + Is_Transfer, 
                data = sdata) 
CoxAll |> 
  tbl_regression(exp = TRUE)
summary(CoxAll) 

# Checking the Proportional Hazards Assumption
PHA <- cox.zph(CoxAll, global = FALSE)
print(PHA)
plot(PHA) 


# All Variables Fit Graph
survival_fit <- survfit(CoxAll)
ggsurvplot(survival_fit, data = sdata, conf.int = TRUE, color = "#2E9FDF",
          xlab = "Time", ylab = "Survival Probability",
          title = "Survival Curves from Cox Proportional Hazards Model")

# Transfer
CoxTran <- survfit(Surv(start, stop, Did_Graduate_Binary) ~ Is_Transfer, data = sdata)
ggsurvplot(CoxTran, data = sdata, conf.int = TRUE, 
            xlab = "Time", ylab = "Survival Probability",
            title = "Survival Curves (Gender)")

# Gender
CoxGen <- survfit(Surv(start, stop, Did_Graduate_Binary) ~ Gender_Binary, data = sdata)
ggsurvplot(CoxGen, data = sdata, conf.int = TRUE, 
           xlab = "Time", ylab = "Survival Probability",
           title = "Survival Curves (Gender)")

  