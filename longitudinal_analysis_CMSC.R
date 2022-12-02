setwd("//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/Final_Cleaned_Dataset")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(corrplot)
#import files
data <- read.csv("Longitudinal_cln_CMSC.csv", stringsAsFactors = F, na.strings = "NA")

#Participant 94131 logged two responses? This did weird things with the code so I took one of the responses out.
data <- data[c(-14),]

#Adding in the semester and is.baseline (F) labels
Semester_Keybind_Longitudinal <- data.frame(cbind(Repeat_ID=data$Repeat_ID, Semester="f2021"))
Baseline_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, Baseline=0))

#Converting the  responses to a numeric scale
SIAS_DF <- data.frame(SIAS1=data$SIAS1, 
                      SIAS2=data$SIAS2, 
                      SIAS3=data$SIAS3, 
                      SIAS4=data$SIAS4, 
                      SIAS5=data$SIAS5, 
                      SIAS6=data$SIAS6, 
                      SIAS7=data$SIAS7, 
                      SIAS8=data$SIAS8, 
                      SIAS9=data$SIAS9, 
                      SIAS10=data$SIAS10, 
                      SIAS11=data$SIAS11, 
                      SIAS12=data$SIAS12, 
                      SIAS13=data$SIAS13, 
                      SIAS14=data$SIAS14, 
                      SIAS15=data$SIAS15, 
                      SIAS16=data$SIAS16, 
                      SIAS17=data$SIAS17, 
                      SIAS18=data$SIAS18, 
                      SIAS19=data$SIAS19, 
                      SIAS20=data$SIAS20)

for (m in 1:ncol(SIAS_DF)) {
  
  for (n in 1:nrow(SIAS_DF)) {
    
    if (SIAS_DF[n,m] == "Not at all") {
      
      as.numeric(SIAS_DF[n,m] <- 0)
      
    }
    
    if (SIAS_DF[n,m] == "Slightly") {
      
      as.numeric(SIAS_DF[n,m] <- 1)
      
    }
    
    if (SIAS_DF[n,m] == "Moderately") {
      
      as.numeric(SIAS_DF[n,m] <- 2)
      
    }
    
    if (SIAS_DF[n,m] == "Very") {
      
      as.numeric(SIAS_DF[n,m] <- 3)
      
    }
    
    if (SIAS_DF[n,m] == "Extremely") {
      
      as.numeric(SIAS_DF[n,m] <- 4)
      
    }
   
  }
}

SIAS_DF <- mutate_all(SIAS_DF, function(SIAS_DF) as.numeric(as.character(SIAS_DF)))

SIAS_DF_Sum <- rowSums(SIAS_DF) 

SIAS_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, SIAS_Sum=SIAS_DF_Sum))

GAD7_DF <- data.frame(GAD1=data$GAD1, 
                      GAD2=data$GAD2, 
                      GAD3=data$GAD3, 
                      GAD4=data$GAD4, 
                      GAD5=data$GAD5, 
                      GAD6=data$GAD6, 
                      GAD7=data$GAD7)


for (m in 1:ncol(GAD7_DF)) {
  
  for (n in 1:nrow(GAD7_DF)) {
    
    if (GAD7_DF[n,m] == "Not at all") {
      
      as.numeric(GAD7_DF[n,m] <- 0)
      
    }
    
    if (GAD7_DF[n,m] == "Several days") {
      
      as.numeric(GAD7_DF[n,m] <- 1)
      
    }
    
    if (GAD7_DF[n,m] == "More than half the days") {
      
      as.numeric(GAD7_DF[n,m] <- 2)
      
    }
    
    if (GAD7_DF[n,m] == "Nearly every day") {
      
      as.numeric(GAD7_DF[n,m] <- 3)
      
    }
  } 
}

GAD7_DF <- mutate_all(GAD7_DF, function(GAD7_DF) as.numeric(as.character(GAD7_DF)))

GAD7_DF_Sum <- rowSums(GAD7_DF)

GAD7_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, GAD7_Sum=GAD7_DF_Sum))

GAD7Diff_DF <- data.frame(GAD7_Sev=data$GADDifficulty)

for (m in 1:ncol(GAD7Diff_DF)) {
  
  for (n in 1:nrow(GAD7Diff_DF)) {
    
    if (GAD7Diff_DF[n,m] == "Not difficult at all") {
      
      as.numeric(GAD7Diff_DF[n,m] <- 0)
      
    }
    
    if (GAD7Diff_DF[n,m] == "Somewhat difficult") {
      
      as.numeric(GAD7Diff_DF[n,m] <- 1)
      
    }
    
    if (GAD7Diff_DF[n,m] == "Very difficult") {
      
      as.numeric(GAD7Diff_DF[n,m] <- 2)
      
    }
    
    if (GAD7Diff_DF[n,m] == "Extremely difficult") {
      
      as.numeric(GAD7Diff_DF[n,m] <- 3)
      
    }
  } 
}

GAD7Diff_DF <- mutate_all(GAD7Diff_DF, function(GAD7Diff_DF) as.numeric(as.character(GAD7Diff_DF)))

GAD7Diff_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, GAD7_Sev=GAD7Diff_DF))

PHQ9_DF <- data.frame(PHQ1=data$DEP1, 
                      PHQ2=data$DEP2, 
                      PHQ3=data$DEP3, 
                      PHQ4=data$DEP4, 
                      PHQ5=data$DEP5, 
                      PHQ6=data$DEP6, 
                      PHQ7=data$DEP7, 
                      PHQ8=data$DEP8, 
                      PHQ9=data$DEP9) 

for (m in 1:ncol(PHQ9_DF)) {
  
  for (n in 1:nrow(PHQ9_DF)) {
    
    if (PHQ9_DF[n,m] == "Not at all\n0") {
      
      PHQ9_DF[n,m] <- 0
      
    }
    
    if (PHQ9_DF[n,m] == "Several Days\n1") {
      
      PHQ9_DF[n,m] <- 1
      
    }
    
    if (PHQ9_DF[n,m] == "More than half the days\n2") {
      
      PHQ9_DF[n,m] <- 2
      
    }
    
    if (PHQ9_DF[n,m] == "Nearly Every Day\n3") {
      
      PHQ9_DF[n,m] <- 3
      
    }
    
    if (PHQ9_DF[n,m] == "Not Difficult at all") {
      
      PHQ9_DF[n,m] <- 0
      
    }
    
    if (PHQ9_DF[n,m] == "Somewhat difficult") {
      
      PHQ9_DF[n,m] <- 1
      
    }
    
    if (PHQ9_DF[n,m] == "Very difficult") {
      
      PHQ9_DF[n,m] <- 2
      
    }
    
    if (PHQ9_DF[n,m] == "Extremely difficult") {
      
      PHQ9_DF[n,m] <- 3
      
    }
  } 
}

PHQ9_DF <- mutate_all(PHQ9_DF, function(PHQ9_DF) as.numeric(as.character(PHQ9_DF)))

PHQ9_DF_Sum <- rowSums(PHQ9_DF)

PHQ9_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, PHQ9_Sum=PHQ9_DF_Sum))

PHQ9diff_DF <- data.frame(PHQ9_Sev=data$DepDifficulty)

for (m in 1:ncol(PHQ9diff_DF)) {
  
  for (n in 1:nrow(PHQ9diff_DF)) {
    
    if (PHQ9diff_DF[n,m] == "Not at all\n0") {
      
      PHQ9diff_DF[n,m] <- 0
      
    }
    
    if (PHQ9diff_DF[n,m] == "Several Days\n1") {
      
      PHQ9diff_DF[n,m] <- 1
      
    }
    
    if (PHQ9diff_DF[n,m] == "More than half the days\n2") {
      
      PHQ9diff_DF[n,m] <- 2
      
    }
    
    if (PHQ9diff_DF[n,m] == "Nearly Every Day\n3") {
      
      PHQ9diff_DF[n,m] <- 3
      
    }
    
    if (PHQ9diff_DF[n,m] == "Not Difficult at all") {
      
      PHQ9diff_DF[n,m] <- 0
      
    }
    
    if (PHQ9diff_DF[n,m] == "Somewhat difficult") {
      
      PHQ9diff_DF[n,m] <- 1
      
    }
    
    if (PHQ9diff_DF[n,m] == "Very difficult") {
      
      PHQ9diff_DF[n,m] <- 2
      
    }
    
    if (PHQ9diff_DF[n,m] == "Extremely difficult") {
      
      PHQ9diff_DF[n,m] <- 3
      
    }
  } 
}

PHQ9diff_DF <- mutate_all(PHQ9diff_DF, function(PHQ9diff_DF) as.numeric(as.character(PHQ9diff_DF)))

PHQ9Diff_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, PHQ9_Dif=PHQ9diff_DF))

Alcohol_DF <- data.frame(Alcohol=data$AlcoholYear,
                              ALC1=data$NIDA_ALC_1,
                              ALC2=data$NIDA_ALC_2,
                              ALC3=data$NIDA_ALC_3)

for (m in 1:ncol(Alcohol_DF)) {
  
  for (n in 1:nrow(Alcohol_DF)) {
    
    if (Alcohol_DF[n,m] == "Never") {
      
      Alcohol_DF[n,m] <- 0
      
    }
    
    if (Alcohol_DF[n,m] == "Once or Twice") {
      
      Alcohol_DF[n,m] <- 1
      
    }
    
    if (Alcohol_DF[n,m] == "Monthly") {
      
      Alcohol_DF[n,m] <- 2
      
    }
    
    if (Alcohol_DF[n,m] == "Weekly") {
      
      Alcohol_DF[n,m] <- 3
      
    }
    
    if (Alcohol_DF[n,m] == "Daily or Almost Daily") {
      
      Alcohol_DF[n,m] <- 4
      
      
    }
  } 
}

Alcohol_DF <- mutate_all(Alcohol_DF, function(Alcohol_DF) as.numeric(as.character(Alcohol_DF)))

Alcohol_DF_Sum <- rowSums(Alcohol_DF)

Alcohol_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, Alcohol_Sum=Alcohol_DF_Sum))

Drugs_DF <- data.frame(Drugs=data$IllegalDrugs,
                       DRUGS1=data$NIDA_DRUGS_1,
                       DRUGS2=data$NIDA_DRUGS_2,
                       DRUGS3=data$NIDA_DRUGS_3)

for (m in 1:ncol(Drugs_DF)) {
  
  for (n in 1:nrow(Drugs_DF)) {
    
    if (Drugs_DF[n,m] == "Never") {
      
      Drugs_DF[n,m] <- 0
      
    }
    
    if (Drugs_DF[n,m] == "Once or Twice") {
      
      Drugs_DF[n,m] <- 1
      
    }
    
    if (Drugs_DF[n,m] == "Monthly") {
      
      Drugs_DF[n,m] <- 2
      
    }
    
    if (Drugs_DF[n,m] == "Weekly") {
      
      Drugs_DF[n,m] <- 3
      
    }
    
    if (Drugs_DF[n,m] == "Daily or Almost Daily") {
      
      Drugs_DF[n,m] <- 4
      
      
    }
  } 
}

Drugs_DF <- mutate_all(Drugs_DF, function(Drugs_DF) as.numeric(as.character(Drugs_DF)))

Drugs_DF <- rowSums(Drugs_DF)

Drugs_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, Drugs_Sum=Drugs_DF))

EatingDisorder_DF <- data.frame(ESP1=data$ESP_1,
                                ESP2=data$ESP_2,
                                ESP3=data$ESP_3,
                                ESP4=data$ESP_4,
                                ESP5=data$ESP_5)

for (m in 1:ncol(EatingDisorder_DF)) {
  
  for (n in 1:nrow(EatingDisorder_DF)) {
    
    if (EatingDisorder_DF[n,m] == "No") {
      
      numeric(EatingDisorder_DF[n,m] <- 0)
      
    }
    
    if (EatingDisorder_DF[n,m] == "Yes") {
      
      numeric(EatingDisorder_DF[n,m] <- 1)
      
    }
    
  }
}

EatingDisorder_DF <- mutate_all(EatingDisorder_DF, function(EatingDisorder_DF) as.numeric(as.character(EatingDisorder_DF)))

EatingDisorder_Sum <- rowSums(EatingDisorder_DF)

EatingDisorder_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, ESP_Sum=EatingDisorder_Sum))

ASRM_DF <- data.frame(ASRM1=data$ASRM_1,
                      ASRM2=data$ASRM_2,
                      ASRM3=data$ASRM_3,
                      ASRM4=data$ASRM_4,
                      ASRM5=data$ASRM_5)

for (m in 1:ncol(ASRM_DF)) {
  
  for (n in 1:nrow(ASRM_DF)) {
    
    if (startsWith(ASRM_DF[n,m], "0")) {
      
      ASRM_DF[n,m] <- 0
      
    }
    
    if (startsWith(ASRM_DF[n,m], "1")) {
      
      ASRM_DF[n,m] <- 1
      
    }
    
    if (startsWith(ASRM_DF[n,m], "2")) {
      
      ASRM_DF[n,m] <- 2
      
    }
    
    if (startsWith(ASRM_DF[n,m], "3")) {
      
      ASRM_DF[n,m] <- 3
      
    }
    
    if (startsWith(ASRM_DF[n,m], "4")) {
      
      ASRM_DF[n,m] <- 4
      
      
    }
  } 
}

ASRM_DF <- mutate_all(ASRM_DF, function(ASRM_DF) as.numeric(as.character(ASRM_DF)))

ASRM_DF_Sum <- rowSums(ASRM_DF)

ASRM_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, ASRM_Sum=ASRM_DF_Sum))

OCIR_DF <- data.frame(OCIR2=data$OCIR2,
                      OCIR3=data$OCIR3,
                      OCIR5=data$OCIR5,
                      OCIR6=data$OCIR6,
                      OCIR8=data$OCIR8,
                      OCIR9=data$OCIR9,
                      OCIR11=data$OCIR11,
                      OCIR12=data$OCIR12,
                      OCIR14=data$OCIR14,
                      OCIR15=data$OCIR15,
                      OCIR17=data$OCIR17,
                      OCIR18=data$OCIR18)

for (m in 1:ncol(OCIR_DF)) {
  
  for (n in 1:nrow(OCIR_DF)) {
    
    if (startsWith(OCIR_DF[n,m], "0")) {
      
      OCIR_DF[n,m] <- 0
      
    }
    
    if (startsWith(OCIR_DF[n,m], "1")) {
      
      OCIR_DF[n,m] <- 1
      
    }
    
    if (startsWith(OCIR_DF[n,m], "2")) {
      
      OCIR_DF[n,m] <- 2
      
    }
    
    if (startsWith(OCIR_DF[n,m], "3")) {
      
      OCIR_DF[n,m] <- 3
      
    }
    
    if (startsWith(OCIR_DF[n,m], "4")) {
      
      OCIR_DF[n,m] <- 4
      
      
    }
  } 
}

OCIR_DF <- mutate_all(OCIR_DF, function(OCIR_DF) as.numeric(as.character(OCIR_DF)))

OCIR_DF_Sum <- rowSums(OCIR_DF)

OCIR_DF_Keybind <- data.frame(cbind(Repeat_ID=data$Repeat_ID, OCIR_Sum=OCIR_DF_Sum))

#Recombining all the converted, numeric subdata

Summary_List <- list(Semester_Keybind_Longitudinal, GAD7_DF_Keybind, GAD7Diff_DF_Keybind, PHQ9_DF_Keybind, PHQ9Diff_DF_Keybind, SIAS_DF_Keybind, Alcohol_DF_Keybind, Drugs_DF_Keybind, EatingDisorder_Keybind, OCIR_DF_Keybind, ASRM_DF_Keybind, Baseline_Keybind)

Summary_DF <- Reduce(function(x, y) merge(x, y, all=TRUE), Summary_List)  

# Hardcoding this because it's 9:45 and I have no idea why baseline won't call itself baseline based off of the above line
#Summary_DF[12] <- colnames("Baseline") #if code is broken just comment this out

# Write to .csv called Longitudinal_Summary_CMSC.csv

write.csv(Summary_DF,"//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/Longitudinal_Summary_CMSC.csv", row.names = FALSE)
