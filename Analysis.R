library(tidyverse)
library(plotly)
ddata <- read.csv("DeDuped_Data.csv")

ddata <- pivoted

####################
#   Demographics   #
####################

# Gender Plot
  ggplot(data = ddata,
        mapping = aes(x = fct_infreq(Gender), fill = Gender)) + 
        geom_bar() +
        scale_fill_manual(values= c("#E798A1","#A90806","#A90806","#E798A1"), guide="none") +
        labs(x = "Gender",
             y = "Number of Students") + 
        geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

# Hispanic Plot
  dfhis <- as.factor(ddata$Is_Hispanic) # Converts to readable format for ggplot
  dfhis <- as.data.frame(dfhis)
  ggplot(data = dfhis,
        mapping = aes(x = dfhis, fill = dfhis)) + 
        geom_bar() +
        labs(x = "Hispanic Staus",
             y = "Number of Students") +
        scale_fill_manual(values= c("#A90806","#E798A1"), guide="none") +
        geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
        scale_x_discrete(labels=c("Non-Hispanic", "Hispanic"))

# Transfer Plot
  dftrans <- as.factor(ddata$Is_Transfer) # Converts to readable format for ggplot
  dftrans <- as.data.frame(dftrans)
  ggplot(data = dftrans,
        mapping = aes(x = fct_infreq(dftrans), fill = dftrans, group = dftrans)) +
        geom_bar() +
        labs(x = "Transfer Status",
             y = "Number of Students") +
        scale_fill_manual(values= c("#E798A1","#A90806"), guide="none") +
        scale_x_discrete(labels=c("Non-Transfer","Transfer")) +
        geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) 

# Lived on Campus
  dfcamp <- ddata %>% # Creates data frame with lived on campus rows
          group_by(Student) %>% 
          summarise(across(ends_with("Lived_On_Campus"), ~ max(.x, na.rm = TRUE), .names = "Max_{col}")) %>%
          ungroup()

  dfcampanal <- dfcamp %>% # Creates a single column with binary variable if they lived on campus at any point
              group_by(Student) %>%
              summarise(Has_One = any(c_across() == 1)) %>%
              ungroup()
  
  ggplot(data = dfcampanal,
        mapping = aes(x = Has_One, fill = Has_One)) +
        geom_bar() + 
        labs(x = "Lived On Campus (Throughout Education)",
             y = "Number of Students") +
        scale_fill_manual(values= c("#A90806","#E798A1"), guide="none") +
        scale_x_discrete(labels=c("Off-Campus","On-Campus")) +
        geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) 
  


# Major List
  dfmajor <- as.data.frame(ddata$major1_Descr) # Combines student's majors into a DF
  colnames(dfmajor) <- as.character("Major") # Renames Column
  dfmajor <- dfmajor %>% # Creates count of majors
            count(Major)
  ggplot(data = dfmajor,
        mapping = aes(x = Major, y = n, fill = Major, group = Major)) + 
        geom_bar(stat="identity") + 
        scale_fill_manual(values= c("#A90806","#E798A1", "#A90806","#E798A1"), guide="none") +
        geom_text(stat = "Identity", aes(label = n), vjust = -0.5) + 
        labs(y = "Number of Students")


####################
#   Data Analysis  #
####################


# Logistic Regression #1 (Graduation)
#####################################
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
  # Recodes Values of gender to ##############################################
  GenderBinary <- ddata$Gender %>%
              mutate(ddata, recode(GenderBin,
                                   "Male" = 0,
                                   "Female" = 1))
  ddata$Gender <- as.character(ddata$Gender)
  ddata$Gender_Binary <- recode(ddata$Gender,
                                "Man" = "0",
                                "Woman" = "1",
                                .default = "1"
  )
  
  unique(ddata$Did_Graduate)
  ddata$Did_Graduate <- recode(ddata$Did_Graduate,
                                "No" = "0",
                                "Yes, with another major" = "0",
                                "Yes, with CS/IT/EMEC" = "1",
  )
  ddata$Did_Graduate <- as.integer(ddata$Did_Graduate)
  
  
  #Runs Binary/Summary Checker 
  ddata$Did_Graduate <- BinaryChecker(ddata[, grep("_Graduate$", names(ddata))])
  ddata$Units_All <- SummaryChecker(ddata[, grep("_Unit_Load$", names(ddata))])
  ddata$Housing_All <- BinaryChecker(ddata[, grep("_Lived_On_Campus$", names(ddata))])
  ddata$Tutoring_All <- SummaryChecker(ddata[, grep("_Tutoring_Visits$", names(ddata))])
  
  
  
  
  
  lr1 <- glm(Did_Graduate ~ Is_Hispanic + Gender_Binary + Is_Transfer + Housing_All + Tutoring_All,
             data = ddata,
             family = binomial())
  
  summary(lr1)


  Terms_Attended





##############################
#   Descriptive Statistics   #
##############################

  # Adds & Calculates Terms Attended
  ddata <- ddata %>% 
    rowwise() %>% 
    mutate(Terms_Attended = sum(!is.na(c_across(ends_with("_Unit_Load"))))) %>% 
    ungroup()

  
# Hispanic Exploration
#####################
  
  # Sets up Variables
  nhisdata <- ddata[ddata$Is_Hispanic == 0, ]
  hisdata <- ddata[ddata$Is_Hispanic == 1, ]
  
  # Terms Attended
  #####################
    mean(nhisdata$Terms_Attended) # Non-Hispanic Terms; Mean: 5.638
    mean(hisdata$Terms_Attended) # Hispanic Terms: Mean: 5.37
    
    # T-Test; Non-Significant
    t.test(hisdata$Terms_Attended, nhisdata$Terms_Attended, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
  
  # Graduation
  #####################
    summary(nhisdata$Did_Graduate_All) # Non-Hispanic Graduation Mean: 0.4934
    summary(hisdata$Did_Graduate_All) # Hispanic Graduation Mean: 0.3746
    
    # T-Test; Significant
    t.test(nhisdata$Did_Graduate_All, hisdata$Did_Graduate_All, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
    
  
# Transfer Status Exploration
##########################################
  
  # Sets up Variables
  hdata <- ddata[ddata$Is_Transfer == 0, ]
  tdata <- ddata[ddata$Is_Transfer == 1, ]
  
  # Terms Attended
  #####################
    summary(hdata$Terms_Attended) # Home Terms: Mean: 5.956
    summary(tdata$Terms_Attended) # Transfer Terms; Mean: 5.145
    
    # Normality Plot for Home Students
    x <- hdata$Terms_Attended
    y = dnorm(x, mean(x), sd(x))
    plot(x, y)
    
    # Normality Plot for Transfer Students
    x <- tdata$Terms_Attended 
    y = dnorm(x, mean(x), sd(x))
    plot(x, y)
    
    # T-Test; Significant
    t.test(tdata$Terms_Attended, hdata$Terms_Attended, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
    
  # Graduation
  #####################
    summary(hdata$Did_Graduate_All) # Home Graduation Mean: 0.3561
    summary(tdata$Did_Graduate_All) # Transfer Graduation Mean: 0.5198
    
    # T-Test; Significant
    t.test(tdata$Did_Graduate_All, hdata$Did_Graduate_All, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
    
    
    

# Gender Exploration
##################################
  # Sets up Variables
  mdata <- ddata[ddata$Gender_Binary == 0, ] 
  fdata <- ddata[ddata$Gender_Binary == 1, ]
  
  mhisdata <- mdata[mdata$Is_Hispanic == 1, ] # Male Hispanic 
  fhisdata <- fdata[fdata$Is_Hispanic == 1, ] # Female Hispanic
  
  mnhisdata <- mdata[mdata$Is_Hispanic == 0, ] # Male Not Hispanic
  fnhisdata <- fdata[fdata$Is_Hispanic == 0, ] # Female Not Hispanic
  
  # Gender by Hispanic Status
    summary(mhisdata$Did_Graduate_All) # Male Hispanic 0.3944
    summary(fhisdata$Did_Graduate_All) # Female Hispanic 0.2913
    
    summary(mnhisdata$Did_Graduate_All) # Male Not Hispanic: 0.4854
    summary(fnhisdata$Did_Graduate_All) # Female Not Hispanic: 0.5333
  
  # Terms Attended
  #####################
    summary(mdata$Terms_Attended) # Male Mean: 5.511
    summary(fdata$Terms_Attended) # Female Mean: 5.588
    
    # T-Test; Non-Significant
    t.test(mdata$Terms_Attended, fdata$Terms_Attended, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
  
  # Graduation
  #######################
    summary(mdata$Did_Graduate_All) #Male Graduation Mean: 0.4476
    summary(fdata$Did_Graduate_All) #Female Graduation Mean: 0.4224
    
    # T-Test; Non-Significant
    t.test(mdata$Did_Graduate_All, fdata$Did_Graduate_All, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
    
    # Plot of Proportion of Male/Female and Graduate/Non-Graduate
    g <- ggplot(ddata, aes(Gender_Binary))
    p <-  g + geom_bar(aes(fill = as.factor(Did_Graduate_All)))
    ggplotly(p)


# Graduation
##################################
    # Sets up Variables
    ngdata <- ddata[ddata$Did_Graduate_All == 0, ] # Didnt Graduate
    gdata <- ddata[ddata$Did_Graduate_All == 1, ] # Did Graduate
    
    hngdata <- ngdata[ngdata$Is_Transfer == 0, ] # Home Did not Graduate
    tngdata <- ngdata[ngdata$Is_Transfer == 1, ] # Transfer Did not Graduate
    
    hgdata <- gdata[gdata$Is_Transfer == 0, ] # Home Graduate
    tgdata <- gdata[gdata$Is_Transfer == 1, ] # Transfer Graduate
    
    # Terms Attended
    #####################
      nrow(ngdata) # How many did not Graduate
      nrow(gdata) # How many did graduate
      
      summary(hngdata$Terms_Attended) # Home did not Graduate Mean Terms: 4.068
      summary(tngdata$Terms_Attended) # Transfer did not Graduate Mean Terms: 3.595
      summary(hngdata$Units_All) # Home did not Graduate Mean Units (Overall): 49.61
      summary(tngdata$Units_All) # Transfer did not Graduate Mean Units (Overall): 36.45
      
      summary(hgdata$Terms_Attended) # Home Graduate Mean Terms: 9.372
      summary(tgdata$Terms_Attended) # Transfer Graduate Mean Terms: 6.577
      summary(hgdata$Units_All) # Home Graduate Mean Units (Overall): 120
      summary(tgdata$Units_All) # Transfer Graduate Mean Units (Overall): 74.4
      
      
      # T-Test; Significant
      t.test(hgdata$Terms_Attended, tgdata$Terms_Attended, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
  
# Dependent Code
#################
    # Splitting By Data Code
    ####################
      umwdata <- ddata[ddata$Dependent_Income_Code %in% c(1, 2), ] # Under-Minimum Wage
      omwdata <- ddata[ddata$Dependent_Income_Code %in% c(3, 4, 5), ] # Over-Minimum Wage
      over75data <- ddata[ddata$Dependent_Income_Code == 6, ]
      
      summary(umwdata$Did_Graduate_All) # Under Federal Minimum Wage: 0.3552
      summary(omwdata$Did_Graduate_All) # Up to Above 35 to 72k: 0.4432
      summary(over75data$Did_Graduate_All) # 72k and above: 0.4786
      
      

# Survival Analysis
#######################  
library(survival)
library(survminer)
library(ggsurvfit)   
      pdata <- read.csv("clean_reorder.csv")
      pdata <- pdata %>%
        group_by(Student) %>% 
        mutate(
          start = seq(0, n()-1),
          stop = seq(1, n())
        )

      pdata$Gender <- as.character(pdata$Gender)
      pdata$Gender_Binary <- recode(pdata$Gender,
                                    "Man" = "0",
                                    "Woman" = "1",
                                    .default = "1"
      )
      
      
      pdata$Graduate <- ifelse(pdata$Did_Graduate == pdata$Term_Code, 1, 0)



     survmodelcox <- coxph(Surv(time = start, time2 = stop, event = Graduate) ~ Unit_Load + Lived_On_Campus + Num_Tutoring_Visits + Is_Hispanic + Is_Transfer + Gender_Binary, 
                      data = data = pdata)
      surv_fit <- survfit(survmodelcox)
      summary(survmodelcox)
      ggsurvplot(surv_fit, data = pdata,
                 xlab = "Time",
                 ylab = "Survival Probability",
                 title = "Survival Curves from Cox Proportional Hazards Model",
                 conf.int = TRUE,    # Confidence intervals
                 risk.table = TRUE,   # Show risk table
                 palette = "Dark2")   # Color palette
  
      

# if Term code = graduate put a 1
# Make start and stop colum that takes into account grouped by student (how many rows)

      
      
      survival_object <- Surv(time = ddata$Terms_Attended, event = ddata$Did_Graduate) 
      surv_model <- survfit(survival_object ~ ddata$Is_Hispanic, data = ddata)
      ggsurvplot(surv_model, data = ddata)
      summary(surv_model)
      
      
      #############
#   Other   #
#############
    
    ## IGNORE
    # Normality Plot for Home Students
    x <- mdata$Terms_Attended
    y = dnorm(x, mean(x), sd(x))
    plot(x, y)
    
    # Normality Plot for Transfer Students
    x <- fdata$Terms_Attended 
    y = dnorm(x, mean(x), sd(x))
    plot(x, y)
    
    
  write.csv(ddata, file = "ddata.csv") # Writes Data to CSV