library(lubridate)
library(tidyverse)
library(stringr)
library(dplyr)

setwd("D:/triton_outputs")
data <- c(list.files("D:/triton_outputs"))
data <- data[which(endsWith(data, "event_pat.csv"))]
final_dataframe <- NULL

for (n in 1:length(data)) {
  filename <- paste0(data[n])
  subdata <- read.csv(filename, stringsAsFactors = F, header = F)

  # Counter loop
  print(paste0("this is the", n, "th iteration, filename: ", filename))
  test_vector <- subdata[2,]
  print(test_vector)
  list_position <- NULL
  
  for (m in 1:length(test_vector)) {
    
    if (is.na(test_vector[m])) {
      
      list_position <- c(list_position, m)
      
    }
    
  }
  
  list_position <- list(list_position)
  filename <- data.frame(filename)
  found_empty_col_nums <- data.frame(matrix(data=list_position, nrow=1, ncol=1))
  found_empty_col_nums <- cbind(empty_col_nums, filename)
  final_dataframe <- rbind(final_dataframe, found_empty_col_nums)

}
# Find and locate the file structure of 
# Optional, counts the populated rows (rows without NAs) in each file and prints each one
for (n in 1:length(data)) {
  filename <- paste0(data[n])
  subdata <- read.csv(filename, stringsAsFactors = F, header = F)
  subdata <- subdata[ , colSums(is.na(subdata))==0]
  columncount <- ncol(subdata)
  print(columncount)
}