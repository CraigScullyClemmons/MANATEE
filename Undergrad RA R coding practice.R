### Library ###
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("stats")
library(lubridate)
library(ggplot2)
library(stats)
library(corrplot)
library(dplyr)

# install.packages("esquisse")
library(esquisse)

###### Set Working Directories to where Data are stored######

#for WINDOWS:
setwd("//datastore01.psy.miami.edu/Groups/AHeller_Lab/Undergrad/NKraus")


# read in data
data <- read.csv("Test_data1.csv", stringsAsFactors = F, na.strings = "NA")


##### TASK 1 #####
# Using a for loop, number each line from 1 to # of rows 
# Call this new column "Index"

data$Index <- NA

for (n in 1:nrow(data)) {
  
  data$Index[n] <- n

}

# BONUS: add row numbers without using a for loop 
# Call this new column "Index2"

Index <- c(1:nrow(data))
dim(Index) <- c(nrow(data),1)
cbind(data, Index)

##### TASK 2 #####
# Using a for loop, calculate Positive Affect (PA) and Negative Affect (NA)
# Call these new columns "PA" and "NA"
# PA (average of all positive items) includes the following items: "Happy" "Excited" "Content" "Attentive" "Relaxed"
# NA (average of all negative items) includes the following items: "Sad" "Tired" "Upset" "Irritable" "Stressed" "Anxious"
data$PA_score <- NA
data$NA_score <- NA

for (n in 1:nrow(data)) {
  
  data$PA_score[n] <- (data$Happy[n] + data$Excited[n] + data$Content[n] + data$Attentive[n] + data$Relaxed[n]) / 5
  
  data$NA_score[n] <- (data$Sad[n] + data$Tired[n] + data$Upset[n] + data$Irritable[n] + data$Stressed[n] + data$Anxious[n]) / 6
  
  
}

#### TASK 3 #####
# Create 2 histograms of affect scores, one of PA scores and one of NA scores
# please title x-axis "Positive Affect Score" or "Negative Affect Score"
# You may use the native histogram function in R, 
# But I encourage you to try ggplot

# hint: use ggplot histograms can be created with geom_histogram()
# if you need extra help, use the esquisser() function from the 'esquisse' package
# only use esquisser() if you have not given ggplot or native histogram functions first

ggplot(data) +
  aes(x = PA_score) +
  geom_histogram() +
  labs(x = "PA Score", title = "Histogram of Positive Affect") +
  theme_minimal()

ggplot(data) +
  aes(x = NA_score) +
  geom_histogram() +
  labs(x = "PA Score", title = "Histogram of Positive Affect") +
  theme_minimal()

#### TASK 4 ####
# Create a new data frame that summarizes each participants scores.
# Call dataframe "Summary_DF"
# Dataframe should include: ID, mean scores for each item (happy, sad, tired, etc.), mean PA, mean NA

Summary_DF <- cbind(unique(data$ID), matrix(NA, length(unique(data$ID)), length(column_names)))
Summary_DF <- data.frame(Summary_DF)
column_names <- c("ID", "Happy", "Excited", "Content", "Attentive", "Relaxed", "PA_score", "Sad", "Tired", "Upset", "Irritable","Stressed", "Anxious", "NA_score")
colnames(Summary_DF) <- column_names
  
for (m in unique(data$ID)) {
  
  subdata <- data[which(data$ID == m),]
  
  for (n in column_names[2:length(column_names)]) {
  
    mean_placeholder <- mean(subdata[,n], na.rm = T)
    
    Summary_DF[which(Summary_DF$ID == m),n] <- mean_placeholder
  
  }

} 


#### TASK 5 #####
# Utilizing the  "Summary_DF" dataframe:
# Create 2 histograms per individual affect scores, one of PA scores and one of NA scores
# please title x-axis "Mean Positive Affect Score" or "Mean Negative Affect Score"
# please title plot "Mean PA per Subject" or "Mean NA per Subject"

for (n in column_names[2:length(column_names)]) {
  
  mean_placeholder <- mean(subdata[,n], na.rm = T)
  
  Summary_DF[which(Summary_DF$ID == m),n] <- mean_placeholder
  
  plot_mean_PA <- ggplot(Summary_DF) +
    aes(x = PA_score, colour = ID, fill = ID) +
    geom_histogram() + stat_bin(binwidth = 1) +
    labs(x = "Mean Positive Affect Score", title = "Mean PA per Subject") +
    theme_bw()
  
  plot_mean_NA <- ggplot(Summary_DF) +
    aes(x = NA_score, colour = ID, fill = ID) +
    geom_histogram() + stat_bin(binwidth = 1) +
    labs(x = "Mean Negative Affect Score", title = "Mean NA per Subject") +
    theme_bw()

}



#### TASK 6 #####
# Utilizing the  "Summary_DF" dataframe:
# please add Depression Scores in a new column from the Sx_data_test dataframe below.
# the depression scores should correspond to the correct subject
# call the new column "DEP_Sum"

Sx_data_test <- read.csv("Sx_data_test.csv", stringsAsFactors = F)

Summary_DF <- cbind(Summary_DF, DEP_Sum=Sx_data_test$DEP_Sum)


#### TASK 7 #####
# Utilizing the  "Summary_DF" dataframe:
# Create 2 scatterplots, with Depression Score on X axis and Mean Affect on Y Axis 
# one plot of PA scores and one plot of NA scores
# please title x-axis "Depression Score"
# please title y-axis "Mean Positive Affect Score" or "Mean Negative Affect Score"
# please title plot "Mean PA vs Depression" or "Mean NA  vs Depression"

#hint: use ggplot scatter plots can be created with geom_point()


ggplot(Summary_DF) +
  aes(x = DEP_Sum, y = NA_score, colour = ID, fill = ID) +
  geom_point() + geom_smooth() +
  labs(x = "Depression Score", y = "Mean NA Score", title = "Mean NA vs. Depression Score") +
  theme_bw()

ggplot(Summary_DF) +
  aes(x = PA_score, y = NA_score, colour = ID, fill = ID) +
  geom_point() + geom_smooth() +
  labs(x = "Mean NA Score", y = "Mean PA Score", title = "Mean NA per Subject") +
  theme_bw()


#### TASK 8 #####
# Utilizing the  "Summary_DF" dataframe:
# Categorize Individuals's depression severity using if statements in a for loop
# Name the new column "DEP_Sev":

# scores between 0-4 --> "None"
# scores between 5-9 --> "Mild"
# scores between 10-14 --> "Moderate"
# scores between 15-19 --> "Moderately_Severe"
# scores between 20-27 --> "Severe"

none <- NA
mild <- NA
moderate <- NA
moderately_severe <- NA
wild <- NA

depression_scores <- c(Summary_DF$DEP_Sum)
for (n in 1:36) {
  
  if (depression_scores[n] <= 4 & depression_scores[n] >= 0) {
    
    none <- c(none, n)
    
  } 
  
  if (depression_scores[n] <= 9 & depression_scores[n] >= 5) {
    
    mild <- c(mild, n)
    
  } 
  
  if (depression_scores[n] <= 14 & depression_scores[n] >= 10) {
    
    moderate <- c(moderate, n)
    
  } 
  
  if (depression_scores[n] <= 19 & depression_scores[n] >= 15) {
    
    moderately_severe <- c(moderately_severe, n)
    
  } 
  
  if (depression_scores[n] <= 27 & depression_scores[n] >= 20) {
    
    wild <- c(wild, n)
    
  }
  
}

DEP_Sev <- c(NA * nrow(Summary_DF))
Summary_DF <- cbind(Summary_DF, DEP_Sev=DEP_Sev)

for (n in none) {
  
 Summary_DF$DEP_Sev[n] <- "None"
  
}

for (n in mild) {
  
  Summary_DF$DEP_Sev[n] <- "Mild"
  
}

for (n in moderate) {
  
  Summary_DF$DEP_Sev[n] <- "Moderate"
  
}

for (n in moderately_severe) {
  
  Summary_DF$DEP_Sev[n] <- "Moderately Severe"
  
}

for (n in wild) {
  
  Summary_DF$DEP_Sev[n] <- "Wild"
  
}

#### TASK 9 #####
# Utilizing the  "Summary_DF" dataframe:
# run a linear regression using the lm() function from the "stats" package
# Predict Mean NEGATIVE Affect (y) by Depression Score (x)
# below is an example of a linear regression, where we are Predicting 'y' by 'x' from dataframe 'DF'

model.nafvdep <- lm(NA_score ~ DEP_Sum, data = Summary_DF)
summary(model.nafvdep)

#### TASK 10 #####
# Utilizing the  "Summary_DF" dataframe:
# run a linear regression using the lm() function from the "stats" package
# Predict Mean POSITIVE Affect (y) by Depression Score (x)
# below is an example of a linear regression, where we are Predicting 'y' by 'x' from dataframe 'DF'

model.pafvdep <- lm(PA_score ~ DEP_Sum, data = Summary_DF)
summary(model.pafvdep)


#### TASK 11 #####
# Utilizing the  "Summary_DF" dataframe:
# run a one-way ANOVA, using the aov() function, to model:
# NEGATIVE Affect as a function of depression severity
# set up like lm() above by naming model to variable, then summary(model.name)

model.naffxdep <- aov(NA_score ~ DEP_Sum, data = Summary_DF)
summary(model.naffxdep)

model.naffxdep <- aov(PA_score ~ DEP_Sum, data = Summary_DF)
summary(model.naffxdep)

#### TASK 12 #####
# Check whether model fits assumption of homoscedasticity:
# use the outline below:

par(mfrow=c(2,2))
plot(model.naffxdep)
par(mfrow=c(1,1))

#### TASK 13 #####
# Conduct post-hoc test (Tukey's Honestly Significant Difference; Tukey's HSD)
# use finction TukeyHSD()

TukeyHSD()

#### TASK 14 #####
# Utilizing the  "Summary_DF" dataframe:
# repeat steps 11-13 for POSITIVE affect instead of negative Affect
#### TASK 15 #####
# Utilizing the  "Summary_DF" dataframe:
# use cor.test() from stats package to assess the correlation between PA_Mean and NA_Mean

model.pafncornaf <- cor.test(Summary_DF$PA_score, Summary_DF$NA_score)
summary(model.pafncornaf)


################## SWITCHING BACK TO data DF ################################
#### TASK 16 #####
# Using Initial dataframe ('data')
# Calculate correlations between all emotion items

emotion_correlation_DF <- data.frame(cbind(data$Happy, data$Sad, data$Tired, data$Upset, data$Excited, data$Irritable, data$Content, data$Attentive, data$Stressed, data$Relaxed, data$Anxious))

column_names_2 <- c("Happy", "Sad", "Tired", "Upset", "Excited", "Irritable", "Content", "Attentive", "Stressed", "Relaxed", "Anxious")

colnames(emotion_correlation_DF) <- column_names_2

emotion_correlation <- cor(emotion_correlation_DF)

#### Task 17 ####
# Create correlation matrix using corrplot() from 'corrplot' package
# look at documentation and mess around with the different arguments
# install.packages("corrplot")

corrplot(emotion_correlation, method=c("shade"))

#### Task 18 ####
# reorder the original dataframe "data" by EMA ID, then Date

data$StartDate <- ymd_hms(data$StartDate)
data <- data[order(data$ID),]
data <- data[order(as.Date(data$StartDate)),]
  
#### Task 19 ####
# Add Exam dates to DENSE (Dense = 1) data in column "exam_num" use the key below to do so:
# Exam 1: Between 2019-02-20 and 2019-02-22
# Exam 2: Between 2019-03-28 and 2019-03-29
# Exam 3: Between 2019-04-05 and 2019-04-11
# Exam 4: Between 2019-04-25 and 2019-04-30
# Exam 5: Between 2019-05-13 and 2019-05-15
#
#hint: the lubridate package is a great tool!

exam_one_dense_interval_start <-  ymd("2019-02-20")
exam_one_dense_interval_end <-  ymd("2019-02-22")
exam_two_dense_interval_start <- ymd("2019-03-28")
exam_two_dense_interval_end <- ymd("2019-03-29")
exam_three_dense_interval_start <- ymd("2019-04-05")
exam_three_dense_interval_end <- ymd("2019-04-11")
exam_four_dense_interval_start <- ymd ("2019-04-25")
exam_four_dense_interval_end <- ymd("2019-04-30")
exam_five_dense_interval_start <- ymd("2019-05-13")
exam_five_dense_interval_end <- ymd("2019-05-15")

exam_num <- c(NA * length(nrow(data)))
data <- cbind(data, exam_num)


for (n in 1:nrow(data)) {
  
  if (date(data$StartDate[n]) >= date(exam_one_dense_interval_start) & date(data$StartDate[n]) <= date(exam_one_dense_interval_end) & data$Dense[n] == 1) {
    
    data$exam_num[n] <- 1
    
  }
  
  if (date(data$StartDate[n]) >= date(exam_two_dense_interval_start) & date(data$StartDate[n]) <= date(exam_two_dense_interval_end) & data$Dense[n] == 1) {
    
    data$exam_num[n] <- 2
    
  }
  
  if (date(data$StartDate[n]) >= date(exam_three_dense_interval_start) & date(data$StartDate[n]) <= date(exam_three_dense_interval_end) & data$Dense[n] == 1) {
    
    data$exam_num[n] <- 3
    
  }
  
  if (date(data$StartDate[n]) >= date(exam_four_dense_interval_start) & date(data$StartDate[n]) <= date(exam_four_dense_interval_end) & data$Dense[n] == 1) {
    
    data$exam_num[n] <- 4
    
  }
  
  if (date(data$StartDate[n]) >= date(exam_five_dense_interval_start) & date(data$StartDate[n]) <= date(exam_five_dense_interval_end) & data$Dense[n] == 1) {
    
    data$exam_num[n] <- 5
    
  }
  
}

#### Task 20 ####
# Add Exam dates to NONDENSE (Dense = 0) data in column "exam_num" use the key below to do so:
# Exam 1: Before 2019-02-20 
# Exam 2: Between 2019-02-21 and 2019-03-28 
# Exam 3: Between 2019-03-29 and 2019-04-05 and 2019-04-11
# Exam 4: Between 2019-04-11 and 2019-04-25 
# Exam 5: Between 2019-04-30 and 2019-05-13
# Exam 6: After 2019-05-15
 

exam_two_interval_start <- ymd("2019-02-21")
exam_two_interval_end <- ymd("2019-03-28")
exam_three_interval_start <- ymd("2019-03-29")
exam_three_interval_end <- ymd("2019-04-05")
exam_four_interval_start <- ymd ("2019-04-11")
exam_four_interval_end <- ymd("2019-04-25")
exam_five_interval_start <- ymd("2019-04-30")
exam_five_interval_end <- ymd("2019-05-13")
exam_six_interval_start <- ymd("2019-05-15")


for (n in 1:nrow(data)) {
  
  if (date(data$StartDate[n]) <= date(exam_two_interval_start) & (data$Dense[n] == 0)) {
    
    data$exam_num[n] <- 1
    
  }
  
  if ((date(data$StartDate[n]) >= date(exam_two_interval_start) & date(data$StartDate[n]) <= date(exam_two_interval_end)) & ( data$Dense[n] == 0)) {
    
    data$exam_num[n] <- 2
    
  }
  
  if ((date(data$StartDate[n]) >= date(exam_three_interval_start) & date(data$StartDate[n]) <= date(exam_three_interval_end)) & (data$Dense[n] == 0)) {
    
    data$exam_num[n] <- 3
    
  }
  
  if ((date(data$StartDate[n]) >= date(exam_four_interval_start) & date(data$StartDate[n]) <= date(exam_four_interval_end)) & (data$Dense[n] == 0)) {
    
    data$exam_num[n] <- 4
    
  }
  
  if ((date(data$StartDate[n]) >= date(exam_five_interval_start) & date(data$StartDate[n]) <= date(exam_five_interval_end)) & (data$Dense[n] == 0)) {
    
    data$exam_num[n] <- 5
    
  }
  
  if ((date(data$StartDate[n]) >= date(exam_six_interval_start)) & (data$Dense[n] == 0)) {
    
    data$exam_num[n] <- 6
    
  }
}



#### Task 21 ####
# Add Exam Grades to new column called 'grade'
# from dataframe below:

## &&

#### Task 22 ####
# Add Prediction to new column called 'pred_1'
# found in grades DF

grades <- read.csv("exam_grades_practice.csv", stringsAsFactors = F, na.strings = c("NA", "Dropped"))

grades$ID <- paste("9", grades$ID, sep='')
grades$ID <- strtoi(grades$ID)

data <- left_join(data, grades, by = c("ID" = "ID", "exam_num" = 'exam_num'))

#### Task 23 ####
# Add Prediction Error to new column called 'PE_1'
# Formula: prediction - grade


data <- cbind(data, pred_error=c(NA*nrow(data)))
data$pred_error <- data$prediction - data$grade


#### Task 24 ####
# please add Depression Scores in a new column from the Sx_data_test dataframe below add to large "data" DF
# the depression scores should correspond to the correct subject
# these scores should repeat over all EMA responses for an individual
# call the new column "DEP_Sum"

dep_sum_table <- cbind(ID=Summary_DF$ID,dep_sum=Sx_data_test$DEP_Sum)

data <- left_join(data, dep_sum_table, by = "ID", copy=TRUE)


#### Task 25 ####
# Z-score Affect scores in data frame within individual (across all nondense and dense responses)
# name the new columns as: PA -> PA.z or NA -> NA.z
#
# hint: the scale() function can aid you in this

#affect_table <- cbind(ID=Summary_DF$ID, NAf=Summary_DF$NA_score, PAf=Summary_DF$PA_score)

#data <- left_join(data, affect_table, by = "ID", copy=TRUE)

PAz <- NA
NAz <- NA
AfID <- NA

for (n in unique(data$ID)) {
  
  subdata <- data.frame(data[which(data$ID == n),])
  PAz <- c(PAz, (subdata$PA_score - mean(subdata$PA_score))/sd(subdata$PA_score)) 
  AfID <- c(AfID, subdata$ID)
  NAz <- c(NAz, (subdata$PA_score - mean(subdata$NA_score))/sd(subdata$NA_score)) 
}

PAz <- PAz[2:length(PAz)]
NAz <- NAz[2:length(NAz)]
AfID <- AfID[2:length(AfID)]

z_table <- data.frame(cbind(PAz=PAz,NAz=NAz,ID=AfID))

data <- left_join(data, z_table, by = "ID", copy=TRUE)

#Get Bill to check

#### TASK 26 #####
# Utilizing the  "data" dataframe:
# run a linear mixed-effects regression using the lmer() function from the "lme4" package
# Predict Dense  Z-scored Negative Affect (NA.z for Dense only) by Prediction Error + exam number 
# add random effects structure of individual (as seen below)
# below is an example of a linear regression, 
# where we are Predicting 'y' by 'x' and 'z' adding random effects for 'ID' from dataframe 'DF'

install.packages("lme4")
library(lme4)

dense_naz <- data.frame(data$NAz[which(data$Dense == 1)])
dense_paz <- data.frame(data$PAz[which(data$Dense == 1)])
dense_exam <- data.frame(data$exam_num[which(data$Dense == 1)])
ID <- data.frame(data$ID[which(data$Dense == 1)])

model_subdata <- cbind(dense_naz=dense_naz, dense_paz=dense_paz, dense_exam=dense_exam, ID=ID)
colnames(model_subdata) <- c("dense_naz", "dense_paz", "dense_exam", "ID")

paz.model <- lmer(model_subdata$dense_naz ~ model_subdata$dense_paz + model_subdata$dense_exam + (1|ID), data = model_subdata)
summary(paz.model)

#### TASK 27 #####
# repeat task 26, but with Dense PA.z instead of NA.z

naz.model <- lmer(model_subdata$dense_paz ~ model_subdata$dense_naz + model_subdata$dense_exam + (1|ID), data = model_subdata)
summary(naz.model)

