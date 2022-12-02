setwd("//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/Final_Cleaned_Dataset")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(corrplot)
#import files
datab <- read.csv("symptom_measures_baseline_3-6000s_04-22-22.csv", stringsAsFactors = F, na.strings = "NA")
baseline_repeats <- c(unique(datab$repeat_ID))

#Participant (someone that goes to party city)
#Import the MEGA SUB LOG to match the semester with the repeat ID
megasublog <- read.csv("Mega_sublog_3-6000s_02_25_22.csv", stringsAsFactors = F, na.strings = "NA")
megasublog <- megasublog[which(megasublog$Repeat_ID %in% baseline_repeats),]
megasublog_in_baseline <- megasublog
write.csv(megasublog,"//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/Megasublog_in_Baseline.csv", row.names = FALSE)

#does this preserve the order of the repeat ID and semester columns?
Semester_Keybind <- data.frame(cbind(Repeat_ID=megasublog$Repeat_ID, Semester=megasublog$semester, EMA_ID=megasublog$EMA_ID))
Semester_Keybind$Repeat_ID <- as.numeric(Semester_Keybind$Repeat_ID)
Semester_Keybind$EMA_ID <- as.numeric(Semester_Keybind$EMA_ID)

#We are about to only take the unique values from this list. Why? Won't that mess it up? No. We were getting an error wherein this keybind had 1223 rows and the summary data
#frame from the baseline had 1218. The cause is that there are 5 duplicated values in the mega subject log with identical information. I manually confirmed that they were 
#the same and will use "unique" to rectify the error.

Semester_Keybind <- unique(Semester_Keybind)

#OCIR is 12 item, ESP is EPSI not ESP

#Take sums from baseline data set and combine
Baselineb <- c(1)
Summaryb_DF <- data.frame(cbind(Repeat_ID=datab$repeat_ID,
                                GAD7_Sum=datab$GAD_total,
                                GAD7_Sev=datab$GAD_sev,
                                PHQ9_Sum=datab$PHQ_total,
                                PHQ9_Sev=datab$PHQ_sev,
                                SIAS_Sum=datab$SIAS_total,
                                Alcohol_Sum=datab$NIDA_ALC_total,
                                Drugs_Sum=datab$NIDA_DRUGS_total,
                                OCIR_Sum=datab$OCIR_12item_total,
                                ASRM_Sum=datab$ASRM_total,
                                Baseline=Baselineb,
                                EMA_ID=as.numeric(datab$EMA_ID)))

#Bill - is there a way to make sure all the rows went to the right entries? Can we check this?
Summaryb_DF <- merge(Semester_Keybind, Summaryb_DF, by=c("Repeat_ID","EMA_ID"), all =TRUE)
Summaryb_DF <- Summaryb_DF[,c(-2)]

write.csv(Summaryb_DF,"//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/Baseline_Summary_CMSC.csv", row.names = FALSE)
