# Initialization
###################
library(survival) # For Survival
library(survminer) # For Suvival Plots
library(ggsurvfit)  # For Suvival Plots
library(timereg) # 
# Others (Dont Know Which I need)
library(lubridate)
library(gtsummary)
library(tidycmprsk)


ddata <- read.csv("clean_reorder.csv")



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

# Adds Start/Stop Sequences
  ddata <- ddata %>%
    group_by(Student) %>% 
    mutate(
      start = seq(0, n()-1),
      stop = seq(1, n())
    )



######################
# Survival Analysis  #
######################

# Cox Proportional Hazards Model w/All Variables 
  CoxAll <- coxph(Surv(time = start, time2 = stop, event = Did_Graduate_Binary) ~ Lived_On_Campus + Num_Tutoring_Visits + Is_Hispanic + Is_Transfer + Gender_Binary, 
                        data = ddata) 
  
  CoxAll |> 
    tbl_regression(exp = TRUE)
  summary(CoxAll) # Summary Information


  
# Checking the Proportional Hazards Assumption
  PHA <- cox.zph(CoxAll, global = FALSE)
  
  # View the results
    print(PHA)
    plot(PHA)

# All Variables Fit
  survival_fit <- survfit(CoxAll)
  ggsurvplot(survival_fit, data = ddata, conf.int = TRUE, color = "#2E9FDF",
             xlab = "Time", ylab = "Survival Probability",
             title = "Survival Curves from Cox Proportional Hazards Model")

# Gender Fit
  CoxGen <- survfit(Surv(start, stop, Did_Graduate_Binary) ~ Gender_Binary, data = ddata)
  
  summary(CoxGen)
  ggsurvplot(CoxGen, data = ddata, conf.int = TRUE, 
             xlab = "Time", ylab = "Survival Probability",
             title = "Survival Curves (Gender)")
