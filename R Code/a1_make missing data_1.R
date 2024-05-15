set.seed(123)

# File My thesis
# Author PHT
# Date 07/28/2023
# Modify history:
#       1-07/28/2023
#       * Check the code
#       * Split the code file
#       * Create final data sets
#       * Update the final results
#       2-07/30/2023
#       * draw chart and missing data chart
#       * comparing data time series
#       3-08/01/2023
#       * Remove bootstrap test
#       * comparing data time series
#       4-08/03/2023
#       * Update data use (use the dataset in paper)
#       * Update CCC
#       5-09/08/2023
#       * Update code

######
# ** Part A ** Preparing for the data ******** -----------
######
# **** A1- Cleaning the work space -------
######
rm(list = ls())

# **** A2- Set the working directory -------
######
setwd("D:/0-Thesis/1-Data_incidence rate")

# ******** Load the required packages -------
library(missMethods)
library(tidyverse)
library(readxl)
library(imputeTS)
library(Amelia)
library(zoo)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(sjPlot)
library(writexl)
library(segmented)
library(MASS)

######
# **** A3- Import the excel data sets -------

dta_raw <- read_excel("0_dta_2021.xlsx")

# Calculate percentage of missing values for each column in the data frame
missing_perc <- sapply(dta_raw, function(x) {sum(is.na(x))/length(x)*100})

# Display the result
missing_perc
######
# **** A4- Make the data frame “dta_clean” -------
colnames(dta_raw)

dta_clean <- dta_raw %>%
  # Format and recode
  mutate(on_set_day = as.Date(onsetday)) %>%
  # Select variable
  dplyr::select(
    id, no, maso, masobn, hovaten,
    # Thong tin ca nhan
    namsinh, dob_1, gioitinh, gioitinh_code,
    # Dia diem tam tru
    tinhthanhtamtru_code, quanhuyentamtru_code, phuongxatamtru_code, 
    # Doi tuong lay mau
    phanloai, phanloai_code, doituonglaymau, doituonglaymau_code,
    # Cac moc thoi gian
    on_set_day
  )


# Check the missing value
missing_perc <- sapply(dta_clean, function(x) {sum(is.na(x))/length(x)*100})

# Display the result
missing_perc

######
# ** Part B ** Calculate the case per data at each commune unit ******** -----------
######
# **** B1- Calculate the case per day -------

# ******** Group by "on_set_day" and summarize the data -----------
colnames(dta_clean)

b1_inci_rate_raw <- dta_clean %>% 
	mutate(count = 1,
  	provine = tinhthanhtamtru_code,
  	district = quanhuyentamtru_code,
  	commune = phuongxatamtru_code,
  	case_type = doituonglaymau_code) %>%
	filter(provine == 27,
		case_type == "CD") %>% 
	group_by(commune, on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(count)) 

colnames(b1_inci_rate_raw)

# Create a sequence of dates from "2021-01-01" to "2021-12-31"
date_sequence <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days")

# Create a data frame with the "on_set_day" column
day_study <- data.frame(on_set_day = date_sequence)

# ********  Merge data ******** -----------
# Function to process each sub-data frame
process_sub_data <- function(sub_df) {
  # Replace missing values in "commune" column with the mode of the column
  mode_commune <- as.character(stats::na.omit(sub_df$commune)[1])
  sub_df$commune[is.na(sub_df$commune)] <- mode_commune
  
  # Replace missing values in "case" column with 0
  sub_df$case[is.na(sub_df$case)] <- 0
  
  return(sub_df)
}

# Split b1_inci_rate_raw by "commune" and process each sub-data frame
sub_data_list <- split(b1_inci_rate_raw, b1_inci_rate_raw$commune)
processed_sub_data_list <- lapply(sub_data_list, process_sub_data)

# Merge each processed sub-data frame with day_study using all = TRUE
merged_data_list <- lapply(processed_sub_data_list, function(sub_df) {
  merge(sub_df, day_study, by = "on_set_day", all = TRUE)
})

# Combine all the merged data frames into one data frame
merge_data <- do.call(rbind, merged_data_list)

# ********  Make time period ******** -----------

b1_inci_rate_raw <- merge_data %>%
    mutate(period = case_when(
        on_set_day <= as.Date("2021-07-02") ~ "Zero - COVID",
        on_set_day >= as.Date("2021-07-03") & on_set_day <= as.Date("2021-10-22") ~ "Transition",
        TRUE ~ "New-normal"
    ))

# ********  Remove missing data ******** -----------
b1_inci_rate_raw$id <- rownames(b1_inci_rate_raw)
class(b1_inci_rate_raw$id)

b1_inci_rate_raw$id <- sub("\\..*", "", b1_inci_rate_raw$id)

b1_inci_rate_raw$case[is.na(b1_inci_rate_raw$case)] <- 0
b1_inci_rate_raw$commune[is.na(b1_inci_rate_raw$commune)] <- b1_inci_rate_raw$id[is.na(b1_inci_rate_raw$commune)]

# Drop the column "id"
b1_inci_rate_raw <- b1_inci_rate_raw[, !(names(b1_inci_rate_raw) == "id")]

# ********  Eplore to excel file ******** -----------
library(writexl)

write_xlsx(b1_inci_rate_raw,"b1_inci_rate_raw.xlsx")

# ******** Seprate the data by periods -----------

b1_zero_raw <- b1_inci_rate_raw %>%
	filter(period == "Zero - COVID")

b1_transi_raw <- b1_inci_rate_raw %>%
	filter(period == "Transition")

b1_new_raw <- b1_inci_rate_raw %>%
	filter(period == "New-normal")

# Explore the data
library(writexl)

write_xlsx(b1_zero_raw,"b2_raw.xlsx")
write_xlsx(b1_transi_raw,"b3_raw.xlsx")
write_xlsx(b1_new_raw,"b4_raw.xlsx")

######
# **** B2.- Create missing Zero COVID data sets -------

# ******** B2.1 Apply the function to each data frame

b2_mi_05 <- delete_MCAR(ds = b1_zero_raw, cols_mis = "case", 0.05)

b2_mi_10 <- delete_MCAR(ds = b1_zero_raw, cols_mis = "case", 0.10)

b2_mi_15 <- delete_MCAR(ds = b1_zero_raw, cols_mis = "case", 0.15)

b2_mi_20 <- delete_MCAR(ds = b1_zero_raw, cols_mis = "case", 0.20)

b2_mi_25 <- delete_MCAR(ds = b1_zero_raw, cols_mis = "case", 0.25)

b2_mi_30 <- delete_MCAR(ds = b1_zero_raw, cols_mis = "case", 0.30)

# ******** Make note ----------
b2_mi_05 <- b2_mi_05 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b2_mi_10 <- b2_mi_10 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b2_mi_15 <- b2_mi_15 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b2_mi_20 <- b2_mi_20 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b2_mi_25 <- b2_mi_25 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b2_mi_30 <- b2_mi_30 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

# ******** B2.2 Explore data 
write_xlsx(b2_mi_05,"b2_mi_05.xlsx")
write_xlsx(b2_mi_10,"b2_mi_10.xlsx")
write_xlsx(b2_mi_15,"b2_mi_15.xlsx")
write_xlsx(b2_mi_20,"b2_mi_20.xlsx")
write_xlsx(b2_mi_25,"b2_mi_25.xlsx")
write_xlsx(b2_mi_30,"b2_mi_30.xlsx")

######
# **** B3.- Create missing Transition data sets -------

# ******** B3.1 Apply the function to each data frame

b3_mi_05 <- delete_MCAR(ds = b1_transi_raw, cols_mis = "case", 0.05)

b3_mi_10 <- delete_MCAR(ds = b1_transi_raw, cols_mis = "case", 0.10)

b3_mi_15 <- delete_MCAR(ds = b1_transi_raw, cols_mis = "case", 0.15)

b3_mi_20 <- delete_MCAR(ds = b1_transi_raw, cols_mis = "case", 0.20)

b3_mi_25 <- delete_MCAR(ds = b1_transi_raw, cols_mis = "case", 0.25)

b3_mi_30 <- delete_MCAR(ds = b1_transi_raw, cols_mis = "case", 0.30)

# ******** Make note ----------
b3_mi_05 <- b3_mi_05 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b3_mi_10 <- b3_mi_10 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b3_mi_15 <- b3_mi_15 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b3_mi_20 <- b3_mi_20 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b3_mi_25 <- b3_mi_25 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b3_mi_30 <- b3_mi_30 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

# ******** B3.1 Explore data 
write_xlsx(b3_mi_05,"b3_mi_05.xlsx")
write_xlsx(b3_mi_10,"b3_mi_10.xlsx")
write_xlsx(b3_mi_15,"b3_mi_15.xlsx")
write_xlsx(b3_mi_20,"b3_mi_20.xlsx")
write_xlsx(b3_mi_25,"b3_mi_25.xlsx")
write_xlsx(b3_mi_30,"b3_mi_30.xlsx")

######
# **** B4- Create missing Zero COVID data sets -------
# ******** B3.1 Apply the function to each data frame

b4_mi_05 <- delete_MCAR(ds = b1_new_raw, cols_mis = "case", 0.05)

b4_mi_10 <- delete_MCAR(ds = b1_new_raw, cols_mis = "case", 0.10)

b4_mi_15 <- delete_MCAR(ds = b1_new_raw, cols_mis = "case", 0.15)

b4_mi_20 <- delete_MCAR(ds = b1_new_raw, cols_mis = "case", 0.20)

b4_mi_25 <- delete_MCAR(ds = b1_new_raw, cols_mis = "case", 0.25)

b4_mi_30 <- delete_MCAR(ds = b1_new_raw, cols_mis = "case", 0.30)

# ******** Make note ----------
b4_mi_05 <- b4_mi_05 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b4_mi_10 <- b4_mi_10 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b4_mi_15 <- b4_mi_15 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b4_mi_20 <- b4_mi_20 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b4_mi_25 <- b4_mi_25 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

b4_mi_30 <- b4_mi_30 %>%
  mutate(note = case_when(is.na(case) ~ "miss", TRUE ~ "none" ))

# ******** B4.1 - Explore data --------------------
write_xlsx(b4_mi_05,"b4_mi_05.xlsx")
write_xlsx(b4_mi_10,"b4_mi_10.xlsx")
write_xlsx(b4_mi_15,"b4_mi_15.xlsx")
write_xlsx(b4_mi_20,"b4_mi_20.xlsx")
write_xlsx(b4_mi_25,"b4_mi_25.xlsx")
write_xlsx(b4_mi_30,"b4_mi_30.xlsx")

