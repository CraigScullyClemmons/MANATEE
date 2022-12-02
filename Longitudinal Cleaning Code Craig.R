library(lubridate)
library(ggplot2)
library(stats)
library(corrplot)
library(esquisse)

#set working directory
setwd("//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC")

#import files
datafall <- read.csv("EMA_followup_fall_2021_9-15-2021.csv", stringsAsFactors = F, na.strings = "NA")
colnames(datafall)[20] <- "Cnum"
datawinter <- read.csv("EMA_followup_fall_2021_December 14, 2021_10.04.csv", stringsAsFactors = F, na.strings = "NA")
colnames(datawinter)[20] <- "Cnum"
data <- rbind(datafall, datawinter)
megasublog <- read.csv("Mega_sublog_3-6000s_02_25_22.csv", stringsAsFactors = F, na.strings = "NA")

#concept:
df_1 <- data.frame(cbind(cnumber=c(1,2,NA,4,5), unique_id=c(23, 19, 88, 50, 12), third_column=c(500, 600, 700, 100, 300)))
df_2 <- data.frame(cbind(cnumber=c(1,2,3,4,5), remaining_data=c("aa", "ab", "ac", "ad", "ae")))
df_3 <- merge(df_2, df_1, by = "cnumber")

#application
repeatid_dataframe <- data.frame(Repeat_ID=megasublog$Repeat_ID, Cnum=megasublog$Cnum)

combined_data <- merge(repeatid_dataframe, data, by = "Cnum", all.y = TRUE)

View(combined_data)

combined_data <- combined_data[c(-1,-2),]

combined_data$Repeat_ID[which(combined_data$Repeat_ID == "NA")] <- NA

subdata <- data.frame(combined_data[which(is.na(combined_data$Repeat_ID)),])

phone_and_bad_cnum <- data.frame(cbind(phone_dashed=subdata$Q3, Cnum=subdata$Cnum))

phone_and_repeat_id <- data.frame(cbind(found_id=megasublog$EMA_ID, phone_dashed=megasublog$phone))

for (n in 1:nrow(phone_and_bad_cnum)) {
  
  raw_phone <- phone_and_bad_cnum$phone_dashed[n]
  undashed_phone <- gsub("-", "", raw_phone)
  phone_and_bad_cnum$phone_dashed[n] <- undashed_phone
  
}

for (n in 1:nrow(combined_data)) {
  
  raw_phone <- combined_data$Q3[n]
  undashed_phone <- gsub("-", "", raw_phone)
  combined_data$Q3[n] <- undashed_phone
  
}



missing_data <- merge(phone_and_bad_cnum, phone_and_repeat_id, by = "phone_dashed")
combined_data <- merge(missing_data, combined_data, by = "Cnum", all.y = TRUE)

for (n in 1:nrow(combined_data)) {
  
  if (is.na(combined_data$Repeat_ID[n]) == TRUE) {
    
    combined_data$Repeat_ID[n] <- combined_data$found_id[n]
    
  }
  
}


write.csv(combined_data,"//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/combineddata.csv", row.names = FALSE)
final <- read.csv("Longitudinal_cln_WIP_CMSC_v2.csv", stringsAsFactors = F, na.strings = "NA")
final <- unique(final)
write.csv(final,"//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/CScully-Clemmons/LDC/Longitudinal_cln_WIP_CMSC_v2.csv", row.names = FALSE)
