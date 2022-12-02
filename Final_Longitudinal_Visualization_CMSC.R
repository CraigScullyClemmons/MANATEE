setwd("//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("reshape2")
install.packages("viridis")
install.packages("esquisse")
install.packages("ggpubr")

library(dplyr)
library(ggplot2)
library(corrplot)
library(ggplot2)
library(reshape2)
library(viridis)
library(esquisse)
library(ggpubr)

# import files

data_baseline <- read.csv("Baseline_Summary_CMSC.csv", stringsAsFactors = F, na.strings = "NA")

data_longitudinal <- read.csv("Longitudinal_Summary_CMSC.csv", stringsAsFactors = F, na.strings = "NA")

# Now we remove the ESP column from the longitudinal data since it doesn't appear in the baseline data and it will make the code more error prone if the tabels are of different length

data_longitudinal <- data_longitudinal[,c(-10)]

# Bind these two together to form a giant long form table of every data entry and call it data_visualization

data_visualization <- bind_rows(data_baseline, data_longitudinal)

data_visualization <- arrange(data_visualization, Repeat_ID)

#Label all semesters numerically so we can do math using the differences between semester (1 ~ 6 months) 0 corresponds to fall 2019, increments of 1.
data_visualization$Semester[which(data_visualization$Semester == "f2019")] <- 0
data_visualization$Semester[which(data_visualization$Semester == "s2020")] <- 1
data_visualization$Semester[which(data_visualization$Semester == "f2020")] <- 2
data_visualization$Semester[which(data_visualization$Semester == "f2021")] <- 4 # because this is in intervals of 6 months!
data_visualization$Semester <- as.numeric(data_visualization$Semester)

#This is the final dataset to which all subdata differences will be rbound
data_differences <- data.frame(matrix(data=NA,0,13))

#All unique repeat ids (this will be used in the creation of subdata)
repeat_ids <- unique(data_visualization$Repeat_ID)

#This is the final dataset to which all subdata differences will be rbound
data_differences <- data.frame(matrix(data=NA,0,13))

interval <- NA

#Count of responses per ID
response_counts <- data.frame(matrix(data=NA, 0, 2))

#All unique repeat ids (this will be used in the creation of subdata)
repeat_ids <- unique(data_visualization$Repeat_ID)

preallocated_df <- data.frame(matrix(data=NA, 0, 13))

for (id in repeat_ids) {
  
  subdata <- data_visualization[which(data_visualization$Repeat_ID == id),]
  subdata <- arrange(subdata, desc(Semester))
  baseline_semester <- min(c(subdata$Semester))
  with_semesters <- cbind(subdata, baseline_semester)
  preallocated_df <- rbind(preallocated_df, with_semesters)
}

for (id in repeat_ids) {
  
  #This holds differences, will be rbound after na.omit()
  all_subtractions <- data.frame(matrix(data=NA, 10, 12)) #is this right? shouldn't we be ending with data in the all_subtractions table?
  
  #This holds number of responses by counting rows in the subdata set. This is just a handy variable to tell us the # of times a participant responded.
  response_count_hopper <- data.frame(matrix(data=NA, 1, 2))
  
  interval <- NA
  
  #This holds differences, will be rbound after na.omit()
  all_subtractions <- data.frame(matrix(data=NA, 10, 12))
  
  subdata <- data_visualization[which(data_visualization$Repeat_ID == id),]
  subdata <- arrange(subdata, desc(Semester))
  
  if (nrow(subdata) < 2) {
    
    next
    
  } else {
    
    cplacer <- 1
    
    for (col in 1:ncol(subdata)) {
      rplacer <- 1
      row <- 1
      
      #count <- row+1
      #print(seq(count, nrow(subdata)))
      
      for (nextrow in seq(row+1, nrow(subdata))) {
        
        # print(rplacer)
        # cat(rplacer) 
        # all_subtractions[row,col] <- newval
        # cat(subdata[row, col] , "-", subdata[nextrow, col], "\n")
        # cat(row, col, "\n")
        
        newval <-  subdata[row, col] - subdata[nextrow, col]
        all_subtractions[rplacer,cplacer] <- newval
        rplacer <- rplacer+1
        
        # print(":::::::::")
        # print(paste0("df1:", subdata$Semester[row]))
        # print(paste0("df2:", subdata$Semester[nextrow]))
        # print(":::::::::")
        
        start <- subdata[nextrow, "Semester"]
        end <- subdata[row, "Semester"]
        interval <- na.omit(c(interval,start,end))
        
      }
      
      interval <- matrix(data=interval,nrow(subdata)-1,2, by = col)
      colnames(interval) <- c("Start", "End")
      cplacer <- cplacer + 1
      
      
    }
    
    # How many impossible bargains are you sitting on top of? On this park bench? There is guilt woven into every thing.
    #longitudinal_plot <- cbind(subdata, interval)
    
    
    
    all_subtractions <- na.omit(all_subtractions)
    
    colnames(all_subtractions) <- colnames(subdata)
    
    all_subtractions <- cbind(all_subtractions, interval)
    
    all_subtractions <- cbind(id, all_subtractions)
    
    data_differences <- rbind(data_differences, all_subtractions)
  }
}

#subcategories based solely on time interval from baseline

six_month <- data_differences[which(data_differences$Semester == 1),]
one_year <- data_differences[which(data_differences$Semester == 2),]
eighteen_month <- data_differences[which(data_differences$Semester == 3),]
two_year <- data_differences[which(data_differences$Semester == 4),]

#subcategories on total number of repeat responses

IDs_baseline <- data_visualization[which(data_visualization$Semester == 0),]
IDs_six_month <- data_visualization[which(data_visualization$Semester == 1),]
IDs_one_year <- data_visualization[which(data_visualization$Semester == 2),]
IDs_eighteen_month <- data_visualization[which(data_visualization$Semester == 3),]
IDs_two_year <- data_visualization[which(data_visualization$Semester == 4),]

repeats_six_month <- IDs_six_month[which(IDs_six_month$Repeat_ID %in% IDs_baseline$Repeat_ID),]
repeats_one_year <- IDs_one_year[which(IDs_one_year$Repeat_ID %in% IDs_baseline$Repeat_ID & IDs_six_month$Repeat_ID),]
repeats_eighteen_month <- IDs_eighteen_month[which(IDs_eighteen_month$Repeat_ID %in% IDs_baseline$Repeat_ID & IDs_six_month$Repeat_ID & IDs_one_year$Repeat_ID),]
repeats_two_year <- IDs_two_year[which(IDs_eighteen_month$Repeat_ID %in% IDs_baseline$Repeat_ID & IDs_six_month$Repeat_ID & IDs_one_year$Repeat_ID),]


################################# CREATE T-TESTS ############################################

t_six_month_GAD7 <- t.test(six_month$GAD7_Sum)
t_one_year_GAD7 <- t.test(one_year$GAD7_Sum)
t_eighteen_month_GAD7 <- t.test(eighteen_month$GAD7_Sum)
t_two_year_GAD7 <- t.test(two_year$GAD7_Sum)

t_six_month_PHQ9 <- t.test(six_month$PHQ9_Sum)
t_one_year_PHQ9 <- t.test(one_year$PHQ9_Sum)
t_eighteen_month_PHQ9 <- t.test(eighteen_month$PHQ9_Sum)
t_two_year_PHQ9 <- t.test(two_year$PHQ9_Sum)

t_six_month_Alcohol <- t.test(six_month$Alcohol_Sum)
t_one_year_Alcohol <- t.test(one_year$Alcohol_Sum)
t_eighteen_month_Alcohol <- t.test(eighteen_month$Alcohol_Sum)
t_two_year_Alcohol <- t.test(two_year$Alcohol_Sum)

t_six_month_SIAS <- t.test(six_month$SIAS_Sum)
t_one_year_SIAS <- t.test(one_year$SIAS_Sum)
t_eighteen_month_SIAS <- t.test(eighteen_month$SIAS_Sum)
t_two_year_SIAS <- t.test(two_year$SIAS_Sum)

t_six_month_Drugs <- t.test(six_month$Drugs_Sum)
t_one_year_Drugs <- t.test(one_year$Drugs_Sum)
t_eighteen_month_Drugs <- t.test(eighteen_month$Drugs_Sum)
t_two_year_Drugs <- t.test(two_year$Drugs_Sum)

t_six_month_OCIR <- t.test(six_month$OCIR_Sum)
t_one_year_OCIR <- t.test(one_year$OCIR_Sum)
t_eighteen_month_OCIR <- t.test(eighteen_month$OCIR_Sum)
t_two_year_OCIR <- t.test(two_year$OCIR_Sum)

t_six_month_ASRM <- t.test(six_month$ASRM_Sum)
t_one_year_ASRM <- t.test(one_year$ASRM_Sum)
t_eighteen_month_ASRM <- t.test(eighteen_month$ASRM_Sum)
t_two_year_ASRM <- t.test(two_year$ASRM_Sum)

###################### CREATE TABLE OF T, CI, MEAN FOR ALL ###################################

T_Six_Month <- c(t_six_month_GAD7$statistic, t_six_month_PHQ9$statistic, t_six_month_SIAS$statistic, t_six_month_Alcohol$statistic, t_six_month_Drugs$statistic, t_six_month_OCIR$statistic, t_six_month_ASRM$statistic)
T_One_Year <- c(t_one_year_GAD7$statistic, t_one_year_PHQ9$statistic, t_one_year_SIAS$statistic, t_one_year_Alcohol$statistic, t_one_year_Drugs$statistic, t_one_year_OCIR$statistic, t_one_year_ASRM$statistic)
T_Eighteen_Month <- c(t_eighteen_month_GAD7$statistic, t_eighteen_month_PHQ9$statistic, t_eighteen_month_SIAS$statistic, t_eighteen_month_Alcohol$statistic, t_eighteen_month_Drugs$statistic, t_eighteen_month_OCIR$statistic, t_eighteen_month_ASRM$statistic)
T_Two_Year <- c(t_two_year_GAD7$statistic, t_two_year_PHQ9$statistic, t_two_year_SIAS$statistic, t_two_year_Alcohol$statistic, t_two_year_Drugs$statistic, t_two_year_OCIR$statistic, t_two_year_ASRM$statistic)

T_Six_Month <- round(T_Six_Month,2)
T_One_Year <- round(T_One_Year,2)
T_Eighteen_Month <- round(T_Eighteen_Month,2)
T_Two_Year <- round(T_Two_Year,2)

CI_Six_Month <- c(paste0("[", round(t_six_month_GAD7$conf.int[1],2), ", ", round(t_six_month_GAD7$conf.int[2],2), "]"), paste0("[", round(t_six_month_PHQ9$conf.int[1],2), ", ", round(t_six_month_PHQ9$conf.int[2],2), "]"), paste0("[", round(t_six_month_SIAS$conf.int[1],2), ", ", round(t_six_month_SIAS$conf.int[2],2), "]"), paste0("[", round(t_six_month_Alcohol$conf.int[1],2), ", ", round(t_six_month_Alcohol$conf.int[2],2), "]"), paste0("[", round(t_six_month_Drugs$conf.int[1],2), ", ", round(t_six_month_Drugs$conf.int[2],2), "]"), paste0("[", round(t_six_month_OCIR$conf.int[1],2), ", ", round(t_six_month_OCIR$conf.int[2],2), "]"), paste0("[", round(t_six_month_ASRM$conf.int[1],2), ", ", round(t_six_month_ASRM$conf.int[2],2), "]"))
CI_One_Year <- c(paste0("[", round(t_one_year_GAD7$conf.int[1],2), ", ", round(t_one_year_GAD7$conf.int[2],2), "]"), paste0("[", round(t_one_year_PHQ9$conf.int[1],2), ", ", round(t_one_year_PHQ9$conf.int[2],2), "]"), paste0("[", round(t_one_year_SIAS$conf.int[1],2), ", ", round(t_one_year_SIAS$conf.int[2],2), "]"), paste0("[", round(t_one_year_Alcohol$conf.int[1],2), ", ", round(t_one_year_Alcohol$conf.int[2],2), "]"), paste0("[", round(t_one_year_Drugs$conf.int[1],2), ", ", round(t_one_year_Drugs$conf.int[2],2), "]"), paste0("[", round(t_one_year_OCIR$conf.int[1],2), ", ", round(t_one_year_OCIR$conf.int[2],2), "]"), paste0("[", round(t_one_year_ASRM$conf.int[1],2), ", ", round(t_one_year_ASRM$conf.int[2],2), "]"))
CI_Eighteen_Month <- c(paste0("[", round(t_eighteen_month_GAD7$conf.int[1],2), ", ", round(t_eighteen_month_GAD7$conf.int[2],2), "]"), paste0("[", round(t_eighteen_month_PHQ9$conf.int[1],2), ", ", round(t_eighteen_month_PHQ9$conf.int[2],2), "]"), paste0("[", round(t_eighteen_month_SIAS$conf.int[1],2), ", ", round(t_eighteen_month_SIAS$conf.int[2],2), "]"), paste0("[", round(t_eighteen_month_Alcohol$conf.int[1],2), ", ", round(t_eighteen_month_Alcohol$conf.int[2],2), "]"), paste0("[", round(t_eighteen_month_Drugs$conf.int[1],2), ", ", round(t_eighteen_month_Drugs$conf.int[2],2), "]"), paste0("[", round(t_eighteen_month_OCIR$conf.int[1],2), ", ", round(t_eighteen_month_OCIR$conf.int[2],2), "]"), paste0("[", round(t_eighteen_month_ASRM$conf.int[1],2), ", ", round(t_eighteen_month_ASRM$conf.int[2],2), "]"))
CI_Two_Year <- c(paste0("[", round(t_two_year_GAD7$conf.int[1],2), ", ", round(t_two_year_GAD7$conf.int[2],2), "]"), paste0("[", round(t_two_year_PHQ9$conf.int[1],2), ", ", round(t_two_year_PHQ9$conf.int[2],2), "]"), paste0("[", round(t_two_year_SIAS$conf.int[1],2), ", ", round(t_two_year_SIAS$conf.int[2],2), "]"), paste0("[", round(t_two_year_Alcohol$conf.int[1],2), ", ", round(t_two_year_Alcohol$conf.int[2],2), "]"), paste0("[", round(t_two_year_Drugs$conf.int[1],2), ", ", round(t_two_year_Drugs$conf.int[2],2), "]"), paste0("[", round(t_two_year_OCIR$conf.int[1],2), ", ", round(t_two_year_OCIR$conf.int[2],2), "]"), paste0("[", round(t_two_year_ASRM$conf.int[1],2), ", ", round(t_two_year_ASRM$conf.int[2],2), "]"))

Differences_Means_Six_Month <- c(t_six_month_GAD7$estimate, t_six_month_PHQ9$estimate, t_six_month_SIAS$estimate, t_six_month_Alcohol$estimate, t_six_month_Drugs$estimate, t_six_month_OCIR$estimate, t_six_month_ASRM$estimate)
Differences_Means_One_Year <- c(t_one_year_GAD7$estimate, t_one_year_PHQ9$estimate, t_one_year_SIAS$estimate, t_one_year_Alcohol$estimate, t_one_year_Drugs$estimate, t_one_year_OCIR$estimate, t_one_year_ASRM$estimate)
Differences_Means_Eighteen_Month <- c(t_eighteen_month_GAD7$estimate, t_eighteen_month_PHQ9$estimate, t_eighteen_month_SIAS$estimate, t_eighteen_month_Alcohol$estimate, t_eighteen_month_Drugs$estimate, t_eighteen_month_OCIR$estimate, t_eighteen_month_ASRM$estimate)
Differences_Means_Two_Year <- c(t_two_year_GAD7$estimate, t_two_year_PHQ9$estimate, t_two_year_SIAS$estimate, t_two_year_Alcohol$estimate, t_two_year_Drugs$estimate, t_two_year_OCIR$estimate, t_two_year_ASRM$estimate)

Differences_Means_Six_Month <- round(Differences_Means_Six_Month,2)
Differences_Means_One_Year <- round(Differences_Means_One_Year,2)
Differences_Means_Eighteen_Month <- round(Differences_Means_Eighteen_Month,2)
Differences_Means_Two_Year <- round(Differences_Means_Two_Year,2)

t_tests <- data.frame(cbind(T_Six_Month, T_One_Year, T_Eighteen_Month, T_Two_Year))
CI <- data.frame(cbind(CI_Six_Month, CI_One_Year, CI_Eighteen_Month, CI_Two_Year))
differences_means <- data.frame(cbind(Differences_Means_Six_Month, Differences_Means_One_Year, Differences_Means_Eighteen_Month, Differences_Means_Two_Year))

row_names <- rownames <- c("GAD7", "PHQ9", "SIAS", "Alcohol", "Drugs", "OCIR", "ASRM")
rownames(t_tests) <- row_names
rownames(CI) <- row_names
rownames(differences_means) <- row_names

#"I want to cry" "desire is the cause of all suffering just cry"

############## SIX MONTH PLOTS #########################

GAD7_Histogram_six_month <- ggplot() +
  aes(x = GAD7_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="GAD7 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["GAD7", "T_Six_Month"], "; ", "CI = ", CI["GAD7", "CI_Six_Month"], "; ", "Mean = ", differences_means["GAD7", "Differences_Means_Six_Month"]))

PHQ9_Histogram_six_month <- ggplot() +
  aes(x = PHQ9_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="PHQ9 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["PHQ9", "T_Six_Month"], "; ", "CI = ", CI["PHQ9", "CI_Six_Month"], "; ", "Mean = ", differences_means["PHQ9", "Differences_Means_Six_Month"]))

SIAS_Histogram_six_month <- ggplot() +
  aes(x = SIAS_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="SIAS Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["SIAS", "T_Six_Month"], "; ", "CI = ", CI["SIAS", "CI_Six_Month"], "; ", "Mean = ", differences_means["SIAS", "Differences_Means_Six_Month"]))

Alcohol_Histogram_six_month <- ggplot() +
  aes(x = Alcohol_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Alcohol Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Alcohol", "T_Six_Month"], "; ", "CI = ", CI["Alcohol", "CI_Six_Month"], "; ", "Mean = ", differences_means["Alcohol", "Differences_Means_Six_Month"]))

#we're gonna find the sadboi cohort
#Eric Nook have you found the papers of the people Aaron is going to kneecap?

Drugs_Histogram_six_month <- ggplot() +
  aes(x = Drugs_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Drugs Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Drugs", "T_Six_Month"], "; ", "CI = ", CI["Drugs", "CI_Six_Month"], "; ", "Mean = ", differences_means["Drugs", "Differences_Means_Six_Month"]))

OCIR_Histogram_six_month <- ggplot() +
  aes(x = OCIR_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="OCIR Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["OCIR", "T_Six_Month"], "; ", "CI = ", CI["OCIR", "CI_Six_Month"], "; ", "Mean = ", differences_means["OCIR", "Differences_Means_Six_Month"]))

ASRM_Histogram_six_month <- ggplot() +
  aes(x = ASRM_Sum) +
  geom_histogram(data=six_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="ASRM Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["ASRM", "T_Six_Month"], "; ", "CI = ", CI["ASRM", "CI_Six_Month"], "; ", "Mean = ", differences_means["ASRM", "Differences_Means_Six_Month"]))

difference_histograms_six_month <- ggarrange(GAD7_Histogram_six_month, PHQ9_Histogram_six_month, Alcohol_Histogram_six_month, Drugs_Histogram_six_month, OCIR_Histogram_six_month, SIAS_Histogram_six_month, ASRM_Histogram_six_month,
                                   ncol = 3, nrow = 3)

################################# ONE YEAR PLOTS ################################################################


GAD7_Histogram_year <- ggplot() +
  aes(x = GAD7_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="GAD7 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["GAD7", "T_One_Year"], "; ", "CI = ", CI["GAD7", "CI_One_Year"], "; ", "Mean = ", differences_means["GAD7", "Differences_Means_One_Year"]))

PHQ9_Histogram_year <- ggplot() +
  aes(x = PHQ9_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="PHQ9 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["PHQ9", "T_One_Year"], "; ", "CI = ", CI["PHQ9", "CI_One_Year"], "; ", "Mean = ", differences_means["PHQ9", "Differences_Means_One_Year"]))

SIAS_Histogram_year <- ggplot() +
  aes(x = SIAS_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="SIAS Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["SIAS", "T_One_Year"], "; ", "CI = ", CI["SIAS", "CI_One_Year"], "; ", "Mean = ", differences_means["SIAS", "Differences_Means_One_Year"]))

Alcohol_Histogram_year <- ggplot() +
  aes(x = Alcohol_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Alcohol Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Alcohol", "T_One_Year"], "; ", "CI = ", CI["Alcohol", "CI_One_Year"], "; ", "Mean = ", differences_means["Alcohol", "Differences_Means_One_Year"]))

Drugs_Histogram_year <- ggplot() +
  aes(x = Drugs_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Drugs Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Drugs", "T_One_Year"], "; ", "CI = ", CI["Drugs", "CI_One_Year"], "; ", "Mean = ", differences_means["Drugs", "Differences_Means_One_Year"]))

OCIR_Histogram_year <- ggplot() +
  aes(x = OCIR_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="OCIR Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["OCIR", "T_One_Year"], "; ", "CI = ", CI["OCIR", "CI_One_Year"], "; ", "Mean = ", differences_means["OCIR", "Differences_Means_One_Year"]))

ASRM_Histogram_year <- ggplot() +
  aes(x = ASRM_Sum) +
  geom_histogram(data=one_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="ASRM Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["ASRM", "T_One_Year"], "; ", "CI = ", CI["ASRM", "CI_One_Year"], "; ", "Mean = ", differences_means["ASRM", "Differences_Means_One_Year"]))


difference_histograms_year <- ggarrange(GAD7_Histogram_year, PHQ9_Histogram_year, Alcohol_Histogram_year, Drugs_Histogram_year, OCIR_Histogram_year, SIAS_Histogram_year, ASRM_Histogram_year,
                                        ncol = 3, nrow = 3)

################################# EIGHTEEN MONTH PLOTS ##########################################################

GAD7_Histogram_eighteen <- ggplot() +
  aes(x = GAD7_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="GAD7 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["GAD7", "T_Eighteen_Month"], "; ", "CI = ", CI["GAD7", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["GAD7", "Differences_Means_Eighteen_Month"]))

PHQ9_Histogram_eighteen <- ggplot() +
  aes(x = PHQ9_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="PHQ9 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["PHQ9", "T_Eighteen_Month"], "; ", "CI = ", CI["PHQ9", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["PHQ9", "Differences_Means_Eighteen_Month"]))

SIAS_Histogram_eighteen <- ggplot() +
  aes(x = SIAS_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="SIAS Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["SIAS", "T_Eighteen_Month"], "; ", "CI = ", CI["SIAS", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["SIAS", "Differences_Means_Eighteen_Month"]))

Alcohol_Histogram_eighteen <- ggplot() +
  aes(x = Alcohol_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Alcohol Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Alcohol", "T_Eighteen_Month"], "; ", "CI = ", CI["Alcohol", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["Alcohol", "Differences_Means_Eighteen_Month"]))

Drugs_Histogram_eighteen <- ggplot() +
  aes(x = Drugs_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Drugs Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Drugs", "T_Eighteen_Month"], "; ", "CI = ", CI["Drugs", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["Drugs", "Differences_Means_Eighteen_Month"]))

OCIR_Histogram_eighteen <- ggplot() +
  aes(x = OCIR_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="OCIR Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["OCIR", "T_Eighteen_Month"], "; ", "CI = ", CI["OCIR", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["OCIR", "Differences_Means_Eighteen_Month"]))

ASRM_Histogram_eighteen <- ggplot() +
  aes(x = ASRM_Sum) +
  geom_histogram(data=eighteen_month, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="ASRM Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["ASRM", "T_Eighteen_Month"], "; ", "CI = ", CI["ASRM", "CI_Eighteen_Month"], "; ", "Mean = ", differences_means["ASRM", "Differences_Means_Eighteen_Month"]))

difference_histograms_eighteen <- ggarrange(GAD7_Histogram_eighteen, PHQ9_Histogram_eighteen, Alcohol_Histogram_eighteen, Drugs_Histogram_eighteen, OCIR_Histogram_eighteen, SIAS_Histogram_eighteen, ASRM_Histogram_eighteen,
                                            ncol = 3, nrow = 3)

################################ TWO YEAR #####################################################

GAD7_Histogram_two_year <- ggplot() +
  aes(x = GAD7_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="GAD7 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["GAD7", "T_Two_Year"], "; ", "CI = ", CI["GAD7", "CI_Two_Year"], "; ", "Mean = ", differences_means["GAD7", "Differences_Means_Two_Year"]))

PHQ9_Histogram_two_year <- ggplot() +
  aes(x = PHQ9_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="PHQ9 Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["PHQ9", "T_Two_Year"], "; ", "CI = ", CI["PHQ9", "CI_Two_Year"], "; ", "Mean = ", differences_means["PHQ9", "Differences_Means_Two_Year"]))

SIAS_Histogram_two_year <- ggplot() + 
  aes(x = SIAS_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="SIAS Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["SIAS", "T_Two_Year"], "; ", "CI = ", CI["SIAS", "CI_Two_Year"], "; ", "Mean = ", differences_means["SIAS", "Differences_Means_Two_Year"]))

Alcohol_Histogram_two_year <- ggplot() +
  aes(x = Alcohol_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Alcohol Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Alcohol", "T_Two_Year"], "; ", "CI = ", CI["Alcohol", "CI_Two_Year"], "; ", "Mean = ", differences_means["Alcohol", "Differences_Means_Two_Year"]))

Drugs_Histogram_two_year <- ggplot() +
  aes(x = Drugs_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="Drugs Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["Drugs", "T_Two_Year"], "; ", "CI = ", CI["Drugs", "CI_Two_Year"], "; ", "Mean = ", differences_means["Drugs", "Differences_Means_Two_Year"]))

OCIR_Histogram_two_year <- ggplot() +
  aes(x = OCIR_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="OCIR Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["OCIR", "T_Two_Year"], "; ", "CI = ", CI["OCIR", "CI_Two_Year"], "; ", "Mean = ", differences_means["OCIR", "Differences_Means_Two_Year"]))

ASRM_Histogram_two_year <- ggplot() +
  aes(x = ASRM_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="ASRM Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["ASRM", "T_Two_Year"], "; ", "CI = ", CI["ASRM", "CI_Two_Year"], "; ", "Mean = ", differences_means["ASRM", "Differences_Means_Two_Year"]))


difference_histograms_two_year <- ggarrange(GAD7_Histogram_two_year, PHQ9_Histogram_two_year, Alcohol_Histogram_two_year, Drugs_Histogram_two_year, OCIR_Histogram_two_year, SIAS_Histogram_two_year, ASRM_Histogram_two_year,
                                            ncol = 3, nrow = 3)

#snoremality (how much you snore during sleep)

##################################### TRACKY TRACKIES (following participants symptomology across multiple semesters) ####################

GAD7_5UP <- ggplot(data = data_visualization, aes(x=Semester, y=GAD7_Sum, group=Repeat_ID)) + geom_line(aes(color=as.factor(Repeat_ID)), show.legend = FALSE)  

first <- data.frame(preallocated_df[which(preallocated_df$baseline_semester == 0),])
second <- data.frame(first[which(first$PHQ9_Sum > 10 & first$Semester == 0),])
first_semester_anxious_cohort_IDs <- c(second$Repeat_ID)
first_semester_anxious_cohort <- data_visualization[which(data_visualization$Repeat_ID %in% first_semester_anxious_cohort_IDs),]

f2019 <- data.frame(preallocated_df[which(preallocated_df$baseline_semester == 1),])
f2019_mod_dep <- data.frame(f2019[which(f2019$PHQ9_Sum > 10 & f2019$Semester == 1),])
f2019_mod_dep_ids <- c(f2019_mod_dep$Repeat_ID)
f2019_mod_dep_long <- data_visualization[which(data_visualization$Repeat_ID %in% f2019_mod_dep_ids),]

PHQ9_Plot_F2019 <- ggplot() + geom_line(data=f2019, aes(x=Semester, y=PHQ9_Sum, group=Repeat_ID), color='blue', show.legend = FALSE) + 
                              geom_line(data=f2019_mod_dep_long, aes(x=Semester, y=PHQ9_Sum, group=Repeat_ID), color='red', size=1, show.legend = FALSE) + labs(title="PHQ9 Scores vs. Time", y="PHQ9 Sum") 

f2019 <- data.frame(preallocated_df[which(preallocated_df$baseline_semester == 0),])
f2019_mod_anx <- data.frame(f2019[which(f2019$GAD7_Sum > 10 & f2019$Semester == 0),])
f2019_mod_anx_ids <- c(f2019_mod_anx$Repeat_ID)
f2019_mod_anx_long <- data_visualization[which(data_visualization$Repeat_ID %in% f2019_mod_anx_ids),]

GAD7_Plot_F2019 <- ggplot() + geom_line(data=f2019, aes(x=Semester, y=GAD7_Sum, group=Repeat_ID), color='blue', show.legend = FALSE) + 
                              geom_line(data=f2019_mod_anx_long, aes(x=Semester, y=GAD7_Sum, group=Repeat_ID), color='red', size=1, show.legend = FALSE) + labs(title="GAD7 Scores vs. Time", y="GAD7 Sum") 

objecttest <- ggplot() + geom_line(data=data_visualization, x=Semester, y=GAD7_Sum)

# first <- data.frame(preallocated_df[which(preallocated_df$baseline_semester == 1),])
# second <- data.frame(first[which(first$PHQ9_Sum > 10),])


# GAD7_10UP <- ggplot(data = data_visualization[which(data_visualization$GAD7_Sum > 10),], aes(x=Semester, y=GAD7_Sum, group=Repeat_ID)) + geom_line(aes(color=as.factor(Repeat_ID)), show.legend = FALSE) + labs(title="Participants Starting at GAD7 10+") + geom_line(data=data_visualization[which(data_visualization$PHQ9_Sum > 10),], aes(color=as.factor(Repeat_ID))
# 
# PHQ9_5UP <- ggplot(data = data_visualization, aes(x=Semester, y=PHQ9_Sum, group=Repeat_ID)) + geom_line(aes(color=as.factor(Repeat_ID)), show.legend = FALSE) + labs(title="All Participants' PHQ9")
# PHQ9_10UP <- ggplot(data = data_visualization[which(data_visualization$PHQ9_Sum > 10),], aes(x=Semester, y=PHQ9_Sum, group=Repeat_ID)) + geom_jitter() + labs(title="Participants Starting at PHQ9 10+")

Trend_Tracking <- ggarrange(GAD7_5UP, GAD7_10UP, PHQ9_5UP, PHQ9_10UP, ncol=2, nrow=2)


## Making it red!
flagged_ids <- c(data_visualization$Repeat_ID[which(data_visualization$PHQ9_Sum > 10)])
flagged_database_PHQ9 <- data_visualization[which(data_visualization$Repeat_ID %in% flagged_ids & data_visualization$First == 1),]
PHQ9_Flagged <- ggplot() + geom_line(data=data_visualization, aes(x=Semester, y=PHQ9_Sum, group=Repeat_ID), color="blue", alpha=0.5, show.legend = FALSE) + geom_line(data=flagged_database_PHQ9, aes(x=Semester, y=PHQ9_Sum, group=Repeat_ID), color="red", show.legend = FALSE)

ASRM_Histogram_two_year <- ggplot() +
  aes(x = ASRM_Sum) +
  geom_histogram(data=two_year, fill = "#ff0000", alpha=0.5) +
  theme_minimal() + labs(x="ASRM Score", y="Count") + labs(subtitle=paste0("t = ", t_tests["ASRM", "T_Two_Year"], "; ", "CI = ", CI["ASRM", "CI_Two_Year"], "; ", "Mean = ", differences_means["ASRM", "Differences_Means_Two_Year"]))

hist(data_visualization$GAD7_Sum[which(data_visualization$Baseline==1)],breaks=20)