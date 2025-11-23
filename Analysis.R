library(tidyverse)
library(plotly)
sdata <- read.csv("trimmed_pivoted_df.csv")
s1data <- sdata

sdata <- s1data


#####
dfcamp <- sdata %>% # Creates data frame with lived on campus rows
  group_by(Student) %>% 
  summarise(across(ends_with("Lived_On_Campus"), ~ max(.x, na.rm = TRUE), .names = "Max_{col}")) %>%
  ungroup()
dfcampanal <- dfcamp %>% # Creates a single column with binary variable if they lived on campus at any point
  group_by(Student) %>%
  summarise(Has_One = any(c_across() == 1)) %>%
  ungroup()

sdata$Tutoring_Binned <- cut(sdata$Tutoring_All,
                    breaks = c(-1, 0, 5, 10,Inf),
                    include.lowest = TRUE,
                    labels = c("No Tutoring", "Low (0-5)", "Moderate (5-10)",
                               "> 10"))

sdata$Binned <- cut(sdata$AverageUnits,
                    breaks = c(0, 11, 15, 18),
                    include.lowest = TRUE,
                    labels = c("Parttime (< 11)", "Fulltime (11-18)", "Heavy (> 18)"))


####################
#   Demographics   #
####################

# Gender Plot
ggplot(sdata, mapping = aes(x = fct_infreq(Gender_Binary), fill = Gender_Binary)) + 
      geom_bar() +
      scale_fill_manual(values= c("#E798A1","#A90806","#A90806","#E798A1"), guide="none") +
      labs(x = "Gender", y = "Number of Students") + 
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

# Hispanic Plot
ggplot(sdata, mapping = aes(x = Is_Hispanic, fill = Is_Hispanic)) + 
      geom_bar() +
      labs(x = "Hispanic Staus", y = "Number of Students") +
      scale_fill_manual(values= c("#A90806","#E798A1"), guide="none") +
      scale_x_discrete(labels=c("Non-Hispanic", "Hispanic")) +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

# Transfer Plot
ggplot(sdata, mapping = aes(x = fct_infreq(Is_Transfer), fill = Is_Transfer, group = Is_Transfer)) +
      geom_bar() +
      labs(x = "Transfer Status", y = "Number of Students") +
      scale_fill_manual(values= c("#E798A1","#A90806"), guide="none") +
      scale_x_discrete(labels=c("Non-Transfer","Transfer")) +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) 

# Lived on Campus
ggplot(sdata, mapping = aes(x = Housing_All, fill = Housing_All)) +
      geom_bar() +
      labs(x = "Lived On Campus (Throughout Education)", y = "Number of Students") +
      scale_fill_manual(values= c("#A90806","#E798A1"), guide="none") +
      scale_x_discrete(labels=c("Off-Campus","On-Campus")) +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) 
  
# Graduation 
ggplot(sdata, mapping = aes(x = Did_Graduate_Binary, fill = Did_Graduate_Binary)) +
      geom_bar() +
      scale_fill_manual(values= c("#A90806","#E798A1"), guide="none") + 
      labs(x = "Graduation Status", y = "Number of Students") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) 

# Tutoring Binned
ggplot(sdata, mapping = aes(x = Tutoring_Binned, fill = Tutoring_Binned)) +
      geom_bar() +
      labs(x = "Student Status (Based on Units)", y = "Number of Students") +
      scale_fill_manual(values = c("#E798A1", "#A90806", "#E798A1", "#A90806"), guide = "none") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)   
  
# Student Status (Units)
ggplot(sdata, mapping = aes(x = Binned, fill = Binned)) +
      geom_bar() +
      scale_fill_manual(values= c("#E798A1", "#A90806", "#E798A1"), guide = "none") + 
      labs(x = "Student Status (Based off Units)", y = "Density",) + 
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

# Units Density Plot  
ggplot(sdata, mapping = aes(x = Units_All)) +
      geom_density(adjust = 1L, fill = "#F6A6B0") +
      labs(x = "Units Taken", y = "Density",
      title = "Density of Units Taken") +
      theme(plot.title = element_text(size = 18L, hjust = 0.5),
            axis.title.y = element_text(size = 15L),
            axis.title.x = element_text(size = 15L))

# Major List////////////// Will need recode
dfmajor <- as.data.frame(sdata$major1_Descr) # Combines student's majors into a DF
colnames(dfmajor) <- as.character("Major") # Renames Column
dfmajor <- dfmajor %>% # Creates count of majors
            count(Major)
ggplot(data = sdata, mapping = aes(x = Major, fill = Major)) + 
      geom_bar(stat="identity") + 
      scale_fill_manual(values= c("#A90806","#E798A1", "#A90806","#E798A1"), guide="none") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  
      labs(y = "Number of Students")


####################
#   Data Cleaning  #
####################

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
  GenderBinary <- sdata$Gender %>%
              mutate(sdata, recode(GenderBin,
                                   "Male" = 0,
                                   "Female" = 1))
  sdata$Gender <- as.character(sdata$Gender)
  sdata$Gender_Binary <- recode(sdata$Gender,
                                "Man" = "0",
                                "Woman" = "1",
                                .default = "1"
  )
  
  unique(sdata$Did_Graduate)
  sdata$Did_Graduate <- recode(sdata$Did_Graduate,
                                "No" = "0",
                                "Yes, with another major" = "0",
                                "Yes, with CS/IT/EMEC" = "1",
  )
  sdata$Did_Graduate <- as.integer(sdata$Did_Graduate)
  
  
  #Runs Binary/Summary Checker 
  sdata$Did_Graduate <- BinaryChecker(sdata[, grep("_Graduate$", names(sdata))])
  sdata$Units_All <- SummaryChecker(sdata[, grep("_Unit_Load$", names(sdata))])
  sdata$Housing_All <- BinaryChecker(sdata[, grep("_Lived_On_Campus$", names(sdata))])
  sdata$Tutoring_All <- SummaryChecker(sdata[, grep("_Tutoring_Visits$", names(sdata))])


  
  # Recodes Hispanic Status
  sdata$Is_Hispanic <- as.character(sdata$Is_Hispanic)
  sdata$Is_Hispanic <- recode(sdata$Is_Hispanic,
                                "Non Hispanic" = "0",
                                "Hispanic" = "1",
                                .default = "1"
  )

  
  
  # Adds & Calculates Terms Attended
  sdata <- sdata %>% 
    rowwise() %>% 
    mutate(Terms_Attended = sum(!is.na(c_across(ends_with("_Unit_Load"))))) %>% 
    ungroup()
  
##############################
#   Descriptive Statistics   #
##############################


  
  
  
  
# Hispanic Exploration
#####################
  t.test(Terms_Attended ~ Is_Hispanic, data = sdata )
  # Sets up Variables
  nhisdata <- sdata[sdata$Is_Hispanic == "Non Hispanic", ]
  hisdata <- sdata[sdata$Is_Hispanic == "Hispanic", ]
  
  # Terms Attended
  #####################
    mean(nhisdata$Terms_Attended) # Non-Hispanic Terms; Mean: 5.638
    mean(hisdata$Terms_Attended) # Hispanic Terms: Mean: 5.37
    
    # T-Test; Non-Significant
    t.test(hisdata$Terms_Attended, nhisdata$Terms_Attended, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
  
  # Graduation
  #####################
    chisq.test()
    summary(nhisdata$Did_Graduate_All) # Non-Hispanic Graduation Mean: 0.4934
    summary(hisdata$Did_Graduate_All) # Hispanic Graduation Mean: 0.3746
    
    # T-Test; Significant
    t.test(nhisdata$Did_Graduate_Binary, hisdata$Did_Graduate_All, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
    t.test(Did_Graduate_Binary ~ Is_Hispanic, data = sdata )
    
  
# Transfer Status Exploration
##########################################
  
  # Sets up Variables
  hdata <- sdata[sdata$Is_Transfer == 0, ]
  tdata <- sdata[sdata$Is_Transfer == 1, ]
  
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
  mdata <- sdata[sdata$Gender_Binary == 0, ] 
  fdata <- sdata[sdata$Gender_Binary == 1, ]
  
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
    g <- ggplot(sdata, aes(Gender_Binary))
    p <-  g + geom_bar(aes(fill = as.factor(Did_Graduate_All)))
    ggplotly(p)


# Graduation
##################################
    # Sets up Variables
    ngdata <- sdata[sdata$Did_Graduate_All == 0, ] # Didnt Graduate
    gdata <- sdata[sdata$Did_Graduate_All == 1, ] # Did Graduate
    
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
      umwdata <- sdata[sdata$Dependent_Income_Code %in% c(1, 2), ] # Under-Minimum Wage
      omwdata <- sdata[sdata$Dependent_Income_Code %in% c(3, 4, 5), ] # Over-Minimum Wage
      over75data <- sdata[sdata$Dependent_Income_Code == 6, ]
      
      summary(umwdata$Did_Graduate_All) # Under Federal Minimum Wage: 0.3552
      summary(omwdata$Did_Graduate_All) # Up to Above 35 to 72k: 0.4432
      summary(over75data$Did_Graduate_All) # 72k and above: 0.4786
      
      
      
      
# Chi-Square Tests      

      

      
data <- sdata
      
# Monique's Chi-Test Stuff
##########################
      #Hispanic and Graduation
      His_grad <- table(data$Is_Hispanic, data$Did_Graduate_Binary)
      His_grad
      chisq.test(His_grad) #sig
      
      mean(data$Is_Hispanic)
      
      sdata <- read.csv("trimmed_pivoted_df.csv")
      
      mean(hisdata$Terms_Attended)
      nhisdata <- sdata[sdata$Is_Hispanic == "Non Hispanic", ]
      hisdata <- sdata[sdata$Is_Hispanic == "Hispanic", ]      
      
      femaledata <- sdata[sdata$Gender_Binary == "Female", ]
      maledata <- sdata[sdata$Gender_Binary == "Male", ]
      
      ### Roberts HSist
      His_grad <- table(sdata$Is_Hispanic, sdata$Did_Graduate_Binary)
      His_grad
      chisq.test(His_grad)
      #Non Hispanic Students
        #          Did Not Graduate Graduated
        # Female               49        45
        # Male                261       267
      #Hispanic Students
        #          Did Not Graduate Graduated
        # Female               44        22
        # Male                193       151
193 + 151
151/344      
#Gender and Graduation
      Gender <- table(data$Did_Graduate_Binary, data$Gender_Binary)
      Gender
      chisq.test(Gender) #non sig
      
      #Transfer and Graduation
      Trans_grad <- table(data$Did_Graduate_Binary, data$Is_Transfer)
      Trans_grad
      chisq.test(Trans_grad) #sig
      
      #Hispanic and Transfer
      His_Tran <- table(data$Is_Hispanic, data$Is_Transfer)
      His_Tran
      chisq.test(His_Tran) #non sig
      
      #Housing and graduation
      Hous_Grad <- table(data$Did_Graduate_Binary, data$Housing_All)
      Hous_Grad
      chisq.test(Hous_Grad) #sig
      #students not in housing = higher grad rates
      
      #Tutoring & graduation
      tut_grad <- table(data$Tutoring_bin, data$Did_Graduate_Binary)
      chisq.test(tut_grad) #sig
      
      #Avg Units & graduation
      units_grad <- table(data$UnitsBins, data$Did_Graduate_Binary)
      units_grad
      
      chisq.test(units_grad)
      