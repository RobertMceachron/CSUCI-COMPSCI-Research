# Load necessary libraries
library(tidyverse)
library(readxl)

# Read excel data into dataframe
orig_data <- read_excel("CS Major Data 2025-10-10.xlsx", guess_max = min(8400, n_max = NULL))

# Get rid of term description columns and major1, major2, and major3 columns (we have description columns for these instead)
removed_term_descs_and_major_codes <- orig_data %>% select(-one_of("Term_Desc", "First_CI_Term_Desc", "First_CS_Term_Desc", "major1", "major2", "major3"))

# Remove duplicates, retain last of near-duplicate rows (1 row per term per student)
de_duped_reduced <- removed_term_descs_and_major_codes %>% group_by(Student, Term_Code) %>% slice_tail(n = 1) %>% ungroup()

# Rename major description columns to include underscores (makes them easier to work with)
names(de_duped_reduced)[which(colnames(de_duped_reduced) == "major1 Descr")] <- "major1_Descr"
names(de_duped_reduced)[which(colnames(de_duped_reduced) == "major2 Descr")] <- "major2_Descr"
names(de_duped_reduced)[which(colnames(de_duped_reduced) == "major3 Descr")] <- "major3_Descr"

#Remove students whose last term row doesn't match their graduation code
mismatch <- de_duped_reduced %>% 
  filter(Did_Graduate != 0) %>% 
  group_by(Student) %>% 
  slice_tail(n = 1) %>% 
  filter(Term_Code != Did_Graduate) %>% 
  pull(Student)

ddr_filtered_mismatch <- de_duped_reduced %>% 
  filter(!Student %in% mismatch)

#Identify current students and remove them
ddrfm_removed_current_students <- ddr_filtered_mismatch %>% 
  group_by(Student) %>% 
  filter(!any(Term_Code == 2258)) %>% 
  ungroup()

# Identify and keep single majors, exclude everyone else
only_single_majors <- ddrfm_removed_current_students %>% 
  group_by(Student) %>% 
  filter((all(is.na(major2_Descr)) == TRUE) && (all(is.na(major3_Descr)) == TRUE)) %>%
  ungroup()

# Remove students who graduated with a master's program in computer science
only_undergrads <- only_single_majors %>% group_by(Student) %>% filter(!any(major1_Descr == "MS: Computer Science")) %>% ungroup()

# Recode Did_Graduate to the following scheme:
# 0 --> No
# nonzero AND major1_Descr == one of: "BS: Computer Science," "BS: Information Technology," "BS: Mechatronics Engineering," "BS: Mechatronics" --> Yes, with CS/IT/EMEC
# nonzero AND major1_Decr is anything else --> Yes, with another major
cs_dept_majors <- c("BS: Computer Science", "BS: Information Technology", "BS: Mechatronics Engineering", "BS: Mechatronics")
students_wo_cs <- only_undergrads %>% 
  group_by(Student) %>% slice_tail(n = 1) %>% 
  filter(!(major1_Descr %in% cs_dept_majors)) %>% ungroup() %>% pull(Student)

recoded_graduation <- only_undergrads %>% 
  group_by(Student) %>% mutate(Did_Graduate = case_when(Did_Graduate == 0 ~ "No",
                                                        (Did_Graduate != 0) & (Student %in% students_wo_cs) ~ "Yes, with another major",
                                                        TRUE ~ "Yes, with CS/IT/EMEC")) %>% ungroup()

# Exclude all rows outside of when students were enrolled in a program under the department of computer science
only_cs_rows <- recoded_graduation %>% group_by(major1_Descr) %>% filter(major1_Descr %in% cs_dept_majors)

# Reorder columns to put static data in one area of the dataframe, and time-series in the other
cleaned_and_reordered <- only_cs_rows %>% select(Student, Is_Hispanic, Gender, 
                                                     Is_Transfer, Dependent_Income_Code, 
                                                     First_CI_Term, First_CS_Term, major1_Descr, 
                                                     major2_Descr, major3_Descr, Term_Code, 
                                                     Unit_Load, Lived_On_Campus, Num_Tutoring_Visits, 
                                                     Did_Graduate)