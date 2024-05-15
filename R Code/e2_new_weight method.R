# File e2_backfill_zero covid
# Author PHT
# Date 08/11/2023
# Modify history:
#       1-08/12/2023
#       * Check the code
#       * Update teh results


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
library(scales)
######
# **** A3- Import the excel data sets -------

b1_raw <- read_excel("b4_raw.xlsx")
e2_mi_05 <- read_excel("b4_mi_05.xlsx")
e2_mi_10 <- read_excel("b4_mi_10.xlsx")
e2_mi_15 <- read_excel("b4_mi_15.xlsx")
e2_mi_20 <- read_excel("b4_mi_20.xlsx")
e2_mi_25 <- read_excel("b4_mi_25.xlsx")
e2_mi_30 <- read_excel("b4_mi_30.xlsx")

######
# **** A4 **** Weighted average  imputation --------
#####

# Function to impute missing values using the mean of the last 14 days
impute_weighted <- function(x) {
  if (any(!is.na(x))) {
    # Calculate the mean of the last 14 days (including the current day)
    last_14_days <- tail(x, 14)
    return(mean(last_14_days, na.rm = TRUE))
  } else {
    return(NA)
  }
}

# ******** A4.1 - 5 per --------

e2_mi_05 <- e2_mi_05 %>%
	# Sort the dataframe by "commune" and "on_set_day"
	arrange(commune, on_set_day) %>%
	# Impute missing values in the "case" column using the mean of the last 14 days
	group_by(commune) %>% 
	mutate(case = ifelse(is.na(case), impute_weighted(case), case)) %>%
	# Clear grouping
	ungroup()

#####
# ******** A4.2 - 10 % -------------------

e2_mi_10 <- e2_mi_10 %>%
	# Sort the dataframe by "commune" and "on_set_day"
	arrange(commune, on_set_day) %>%
	# Impute missing values in the "case" column using the mean of the last 14 days
	group_by(commune) %>% 
	mutate(case = ifelse(is.na(case), impute_weighted(case), case)) %>%
	# Clear grouping
	ungroup()

#####
# ******** A4.3 - 15 % -------------------

e2_mi_15 <- e2_mi_15 %>%
	# Sort the dataframe by "commune" and "on_set_day"
	arrange(commune, on_set_day) %>%
	# Impute missing values in the "case" column using the mean of the last 14 days
	group_by(commune) %>% 
	mutate(case = ifelse(is.na(case), impute_weighted(case), case)) %>%
	# Clear grouping
	ungroup()

#####
# ******** A4.4 - 20 % -------------------

e2_mi_20 <- e2_mi_20 %>%
	# Sort the dataframe by "commune" and "on_set_day"
	arrange(commune, on_set_day) %>%
	# Impute missing values in the "case" column using the mean of the last 14 days
	group_by(commune) %>% 
	mutate(case = ifelse(is.na(case), impute_weighted(case), case)) %>%
	# Clear grouping
	ungroup()

#####
# ******** A4.5 - 25 % -------------------

e2_mi_25 <- e2_mi_25 %>%
	# Sort the dataframe by "commune" and "on_set_day"
	arrange(commune, on_set_day) %>%
	# Impute missing values in the "case" column using the mean of the last 14 days
	group_by(commune) %>% 
	mutate(case = ifelse(is.na(case), impute_weighted(case), case)) %>%
	# Clear grouping
	ungroup()

#####
# ******** A4.6 - 30 % -------------------

e2_mi_30 <- e2_mi_30 %>%
	# Sort the dataframe by "commune" and "on_set_day"
	arrange(commune, on_set_day) %>%
	# Impute missing values in the "case" column using the mean of the last 14 days
	group_by(commune) %>% 
	mutate(case = ifelse(is.na(case), impute_weighted(case), case)) %>%
	# Clear grouping
	ungroup()

######
# ** Part B ** Calculate the incidence rate ******** -----------
######
# Create the dataset for caluculate the Crude bias and crude RMSE ----------
b1_raw_1 <- b1_raw
e2_mi_05_1 <- e2_mi_05
e2_mi_10_1 <- e2_mi_10
e2_mi_15_1 <- e2_mi_15
e2_mi_20_1 <- e2_mi_20
e2_mi_25_1 <- e2_mi_25
e2_mi_30_1 <- e2_mi_30

######
# **** B1 - Make the incidence rate -------------
e2_inci_raw <- b1_raw %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "raw")

e2_inci_mi_05 <- e2_mi_05 %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "5 per")

e2_inci_mi_10 <- e2_mi_10 %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "10 per")

e2_inci_mi_15 <- e2_mi_15 %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "15 per")

e2_inci_mi_20 <- e2_mi_20 %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "20 per")

e2_inci_mi_25 <- e2_mi_25 %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "25 per")

e2_inci_mi_30 <- e2_mi_30 %>%
	group_by(on_set_day) %>%
	summarize(
    # Incidence rate
    case = sum(case),
    incidence_rate = case/1368840*1000000) %>%
	mutate(type = "30 per")

######
# **** B2 - Merge the data -------------
e2_inci_merge <- rbind(e2_inci_raw, e2_inci_mi_05, e2_inci_mi_10, 
	e2_inci_mi_15, e2_inci_mi_20, e2_inci_mi_25, e2_inci_mi_30)

e2_inci_merge$type <- factor(e2_inci_merge$type, 
                        levels = c("raw", "5 per", "10 per", 
                        	"15 per", "20 per", "25 per", "30 per"))

######
# **** B5- Ve bieu do ----------
e2_inci_raw <- subset(e2_inci_raw, select = -type)
e2_inci_mi_05 <- subset(e2_inci_mi_05, select = -type)
e2_inci_mi_10 <- subset(e2_inci_mi_10, select = -type)
e2_inci_mi_15 <- subset(e2_inci_mi_15, select = -type)
e2_inci_mi_20 <- subset(e2_inci_mi_20, select = -type)
e2_inci_mi_25 <- subset(e2_inci_mi_25, select = -type)
e2_inci_mi_30 <- subset(e2_inci_mi_30, select = -type)


colnames(e2_inci_raw) <- c("on_set_day", "case_raw", "incidence_rate_raw")
colnames(e2_inci_mi_05) <- c("on_set_day", "case_05", "incidence_rate_05")
colnames(e2_inci_mi_10) <- c("on_set_day", "case_10", "incidence_rate_10")
colnames(e2_inci_mi_15) <- c("on_set_day", "case_15", "incidence_rate_15")
colnames(e2_inci_mi_20) <- c("on_set_day", "case_20", "incidence_rate_20")
colnames(e2_inci_mi_25) <- c("on_set_day", "case_25", "incidence_rate_25")
colnames(e2_inci_mi_30) <- c("on_set_day", "case_30", "incidence_rate_30")

e2_graph_inci_merge <- merge(e2_inci_raw, e2_inci_mi_05, by = "on_set_day", all.x = TRUE)
e2_graph_inci_merge <- merge(e2_graph_inci_merge, e2_inci_mi_10, by = "on_set_day", all.x = TRUE)
e2_graph_inci_merge <- merge(e2_graph_inci_merge, e2_inci_mi_15, by = "on_set_day", all.x = TRUE)
e2_graph_inci_merge <- merge(e2_graph_inci_merge, e2_inci_mi_20, by = "on_set_day", all.x = TRUE)
e2_graph_inci_merge <- merge(e2_graph_inci_merge, e2_inci_mi_25, by = "on_set_day", all.x = TRUE)
e2_graph_inci_merge <- merge(e2_graph_inci_merge, e2_inci_mi_30, by = "on_set_day", all.x = TRUE)

e2_graph_inci_merge$on_set_day <- as.Date(e2_graph_inci_merge$on_set_day )

# color
# 05 Blue: "blue"
# 10 Red: "red"
# 15 Green: "green"
# 20 Purple: "purple"
# 25 Orange: "orange"
# 30 Magenta: "magenta"
# ******** B5.1 - Raw vs 5% ---------
e2_graph_raw_05 <- ggplot(data = e2_graph_inci_merge, aes(x = on_set_day)) + 
  geom_line(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +
  geom_line(aes(y = incidence_rate_05, color = "Missing Data (5%)"), size = 1) + 
  geom_point(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +  
  geom_point(aes(y = incidence_rate_05, color = "Missing Data (5%)"), size = 1) + 
  labs(x = "Onset Day",                     # X-axis label 
       y = "Incidence Rate",                # Y-axis label 
  ) +
  scale_color_manual(name = "Data Type", values = c("Raw Data" = "black", "Missing Data (5%)" = "blue")) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
	# Title
    ggtitle(label = "Original data vs missing value data at 5% level") + 
    # Chinh Ox 
    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b"))

e2_graph_raw_05

#####
# ******** B5.2 - Raw vs 10% ---------
e2_graph_raw_10 <- ggplot(data = e2_graph_inci_merge, aes(x = on_set_day)) + 
  geom_line(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +
  geom_line(aes(y = incidence_rate_10, color = "Missing Data (10%)"), size = 1) + 
  geom_point(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +  
  geom_point(aes(y = incidence_rate_10, color = "Missing Data (10%)"), size = 1) + 
  labs(x = "Onset Day",                     # X-axis label 
       y = "Incidence Rate",                # Y-axis label 
  ) +
  scale_color_manual(name = "Data Type", values = c("Raw Data" = "black", "Missing Data (10%)" = "red")) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
	# Title
    ggtitle(label = "Original data vs missing value data at 10% level") + 
    # Chinh Ox 
    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b"))

e2_graph_raw_10

#####
# ******** B5.3 - Raw vs 15% ---------
e2_graph_raw_15 <- ggplot(data = e2_graph_inci_merge, aes(x = on_set_day)) + 
  geom_line(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +
  geom_line(aes(y = incidence_rate_15, color = "Missing Data (15%)"), size = 1) + 
  geom_point(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +  
  geom_point(aes(y = incidence_rate_15, color = "Missing Data (15%)"), size = 1) + 
  labs(x = "Onset Day",                     # X-axis label 
       y = "Incidence Rate",                # Y-axis label 
  ) +
  scale_color_manual(name = "Data Type", values = c("Raw Data" = "black", "Missing Data (15%)" = "green")) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
	# Title
    ggtitle(label = "Original data vs missing value data at 15% level") + 
    # Chinh Ox 
    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b"))
e2_graph_raw_15

#####
# ******** B5.4 - Raw vs 20% ---------
e2_graph_raw_20 <- ggplot(data = e2_graph_inci_merge, aes(x = on_set_day)) + 
  geom_line(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +
  geom_line(aes(y = incidence_rate_20, color = "Missing Data (20%)"), size = 1) + 
  geom_point(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +  
  geom_point(aes(y = incidence_rate_20, color = "Missing Data (20%)"), size = 1) + 
  labs(x = "Onset Day",                     # X-axis label 
       y = "Incidence Rate",                # Y-axis label 
  ) +
  scale_color_manual(name = "Data Type", values = c("Raw Data" = "black", "Missing Data (20%)" = "purple")) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
	# Title
    ggtitle(label = "Original data vs missing value data at 20% level") + 
    # Chinh Ox 
    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b"))

e2_graph_raw_20

#####
# ******** B5.5 - Raw vs 25% ---------
e2_graph_raw_25 <- ggplot(data = e2_graph_inci_merge, aes(x = on_set_day)) + 
  geom_line(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +
  geom_line(aes(y = incidence_rate_25, color = "Missing Data (25%)"), size = 1) + 
  geom_point(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +  
  geom_point(aes(y = incidence_rate_25, color = "Missing Data (25%)"), size = 1) + 
  labs(x = "Onset Day",                     # X-axis label 
       y = "Incidence Rate",                # Y-axis label 
  ) +
  scale_color_manual(name = "Data Type", values = c("Raw Data" = "black", "Missing Data (25%)" = "orange")) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
	# Title
    ggtitle(label = "Original data vs missing value data at 25% level")  + 
    # Chinh Ox 
    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b"))
e2_graph_raw_25

#####
# ******** B5.6 - Raw vs 30% ---------
e2_graph_raw_30 <- ggplot(data = e2_graph_inci_merge, aes(x = on_set_day)) + 
  geom_line(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +
  geom_line(aes(y = incidence_rate_30, color = "Missing Data (30%)"), size = 1) + 
  geom_point(aes(y = incidence_rate_raw, color = "Raw Data"), size = 1) +  
  geom_point(aes(y = incidence_rate_30, color = "Missing Data (30%)"), size = 1) + 
  labs(x = "Onset Day",                     # X-axis label 
       y = "Incidence Rate",                # Y-axis label 
  ) +
  scale_color_manual(name = "Data Type", values = c("Raw Data" = "black", "Missing Data (30%)" = "magenta")) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
	# Title
    ggtitle(label = "Original data vs missing value data at 30% level")  + 
    # Chinh Ox 
    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b"))

e2_graph_raw_30

#####
# ******** B5.7 - Merge 3 graph one ---------
# Load the required packages
library(gridExtra)
library(ggplot2)

# Assuming you already have the six ggplot graphs (graph_raw_05, graph_raw_10, ..., graph_raw_30)

# Create a list of the six ggplot graphs
e2_plot_list_1 <- list(e2_graph_raw_05, e2_graph_raw_10, e2_graph_raw_15)

e2_plot_list_2 <- list(e2_graph_raw_20, e2_graph_raw_25, e2_graph_raw_30)

# Arrange the plots 
e2_combined_plot_0515 <- grid.arrange(
  grobs = e2_plot_list_1,
  ncol = 1  # Adjust the number of columns here if needed
)

# Explore the plot
ggsave(
	filename = "e2_1_combined_plot_0515.PNG", 
  plot = e2_combined_plot_0515, 
  width = 9, height = 6, 
  units = "in", dpi = 600)

# Arrange the plots 
e2_combined_plot_2030 <- grid.arrange(
  grobs = e2_plot_list_2,
  ncol = 1  # Adjust the number of columns here if needed
)

# Explore the plot
ggsave(
	filename = "e2_2_combined_plot_2030.PNG", 
  plot = e2_combined_plot_2030, 
  width = 9, height = 6, 
  units = "in", dpi = 600)

######
# ** Part C ** Calculate the CCC -----------
######

# Remove 0 value
b1_raw <- b1_raw %>%
	filter(case != 0)

e2_mi_05 <- e2_mi_05 %>%
	filter(case != 0)

e2_mi_10 <- e2_mi_10 %>%
	filter(case != 0)

e2_mi_15 <- e2_mi_15 %>%
	filter(case != 0)

e2_mi_20 <- e2_mi_20 %>%
	filter(case != 0)

e2_mi_25 <- e2_mi_25 %>%
	filter(case != 0)

e2_mi_30 <- e2_mi_30 %>%
	filter(case != 0)

# **** C1 - raw -----------
######
# **************** C1.1 - Gop ----------
e2_raw_sum <- b1_raw %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C1.2 - Tinh lag time ----------
e2_raw_sum_lag <- e2_raw_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C1.3 - Xac dinh vong lap ----------
library(data.table)

e2_raw_sum_cycle <- e2_raw_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C1.4 - Tinh cac chi so ccc ---------------
e2_raw_sum_ccc_indexes <- e2_raw_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C1.5 - Tong hop ket qua CCC ---------------
e2_raw_ccc_indexes <- e2_raw_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_raw = max(ccc_count),
  	ccc_time_raw = mean(ccc_time),
  	ccc_confirm_raw = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C2 - 5 per -----------
######
# **************** C2.1 - Gop ----------
e2_mi_05_sum <- e2_mi_05 %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C2.2 - Tinh lag time ----------
e2_mi_05_sum_lag <- e2_mi_05_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C2.3 - Xac dinh vong lap ----------
library(data.table)

e2_mi_05_sum_cycle <- e2_mi_05_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C2.4 - Tinh cac chi so ccc ---------------
e2_mi_05_sum_ccc_indexes <- e2_mi_05_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C2.5 - Tong hop ket qua CCC ---------------
e2_mi_05_ccc_indexes <- e2_mi_05_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_mi_05 = max(ccc_count),
  	ccc_time_mi_05 = mean(ccc_time),
  	ccc_confirm_mi_05 = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C3 - 10 per -----------
######
# **************** C3.1 - Gop ----------
e2_mi_10_sum <- e2_mi_10 %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C3.2 - Tinh lag time ----------
e2_mi_10_sum_lag <- e2_mi_10_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C3.3 - Xac dinh vong lap ----------
library(data.table)

e2_mi_10_sum_cycle <- e2_mi_10_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C3.4 - Tinh cac chi so ccc ---------------
e2_mi_10_sum_ccc_indexes <- e2_mi_10_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C3.5 - Tong hop ket qua CCC ---------------
e2_mi_10_ccc_indexes <- e2_mi_10_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_mi_10 = max(ccc_count),
  	ccc_time_mi_10 = mean(ccc_time),
  	ccc_confirm_mi_10 = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C4 - 15 per -----------
######
# **************** C4.1 - Gop ----------
e2_mi_15_sum <- e2_mi_15 %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C4.2 - Tinh lag time ----------
e2_mi_15_sum_lag <- e2_mi_15_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C4.3 - Xac dinh vong lap ----------
library(data.table)

e2_mi_15_sum_cycle <- e2_mi_15_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C4.4 - Tinh cac chi so ccc ---------------
e2_mi_15_sum_ccc_indexes <- e2_mi_15_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C4.5 - Tong hop ket qua CCC ---------------
e2_mi_15_ccc_indexes <- e2_mi_15_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_mi_15 = max(ccc_count),
  	ccc_time_mi_15 = mean(ccc_time),
  	ccc_confirm_mi_15 = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C5 - 20 per -----------
######
# **************** C5.1 - Gop ----------
e2_mi_20_sum <- e2_mi_20 %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C5.2 - Tinh lag time ----------
e2_mi_20_sum_lag <- e2_mi_20_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C5.3 - Xac dinh vong lap ----------
library(data.table)

e2_mi_20_sum_cycle <- e2_mi_20_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C5.4 - Tinh cac chi so ccc ---------------
e2_mi_20_sum_ccc_indexes <- e2_mi_20_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C5.5 - Tong hop ket qua CCC ---------------
e2_mi_20_ccc_indexes <- e2_mi_20_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_mi_20 = max(ccc_count),
  	ccc_time_mi_20 = mean(ccc_time),
  	ccc_confirm_mi_20 = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C6 - 25 per -----------
######
# **************** C6.1 - Gop ----------
e2_mi_25_sum <- e2_mi_25 %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C6.2 - Tinh lag time ----------
e2_mi_25_sum_lag <- e2_mi_25_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C6.3 - Xac dinh vong lap ----------
library(data.table)

e2_mi_25_sum_cycle <- e2_mi_25_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C6.4 - Tinh cac chi so ccc ---------------
e2_mi_25_sum_ccc_indexes <- e2_mi_25_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C6.5 - Tong hop ket qua CCC ---------------
e2_mi_25_ccc_indexes <- e2_mi_25_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_mi_25 = max(ccc_count),
  	ccc_time_mi_25 = mean(ccc_time),
  	ccc_confirm_mi_25 = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C7 - 30 per -----------
######
# **************** C7.1 - Gop ----------
e2_mi_30_sum <- e2_mi_30 %>%
    # Nhom theo cac var
    group_by(date = on_set_day, 
        commune = commune) %>%
    # Sap xep theo thu tu
    arrange(date, commune) %>%
    dplyr :: summarise(confirm = sum(case))

#####
# **************** C7.2 - Tinh lag time ----------
e2_mi_30_sum_lag <- e2_mi_30_sum %>%
    # Sap xep theo thu tu
    arrange(commune, date) %>%
    # Tinh toan lag time giua 2 lan co ca benh o moi noi o
    group_by(commune) %>%
    mutate(
        # Delta time 1
        delta_1 = as.numeric(as.Date(date) - lag(as.Date(date)) +1),
        delta_1 = case_when(
            is.na(delta_1) ~ 0,
            TRUE ~ delta_1
        )) %>%
    ungroup() 

#####
# **************** C7.3 - Xac dinh vong lap ----------
library(data.table)

e2_mi_30_sum_cycle <- e2_mi_30_sum_lag %>%
    # Xac dinh cac su kien
    arrange(commune) %>%
    mutate(event = case_when(
        # Dich
        delta_1 >= 0 & delta_1 <= 14 ~ 1,
        # het dich 14 ngay
        delta_1 > 14 ~ 2,)) %>%
	# Tinh vong lap cua event =1 va 2
    arrange(commune) %>%
    group_by(commune) %>%
    mutate(time = rleid(commune, event)) %>%
    # Tinh start and end time
    group_by(commune) %>%
    group_by(commune, event, time) %>%
    mutate(start_time = case_when(
        event == 1 ~ min(date),
        event == 2 ~ date
        ), 
        end_time = case_when(
        event == 1 ~ max (date),
        event == 2 ~ date
        )) %>%
	# Tinh lag time
    ungroup() %>%
    group_by(commune, event) %>%
	    mutate(
        # Delta time 1
        lag_t = as.numeric(as.Date(end_time) - as.Date(start_time)),
        lag_t = case_when(
            is.na(lag_t) ~ 0,
            TRUE ~ lag_t
        ))

#####
# **************** C7.4 - Tinh cac chi so ccc ---------------
e2_mi_30_sum_ccc_indexes <- e2_mi_30_sum_cycle %>%
    # Tinh ccc count
    group_by(commune) %>%
    mutate(ccc_count = rleid(commune, start_time, end_time)) %>%
	# Tinh ccc time
    mutate(ccc_time = as.numeric(as.Date(end_time) - as.Date(start_time) + 1)) %>%
	# Tinh ccc confirm case
    group_by(commune, ccc_count, ccc_time, start_time, end_time) %>%
    summarise(ccc_confirm = sum(confirm)) 

#####
# **************** C7.5 - Tong hop ket qua CCC ---------------
e2_mi_30_ccc_indexes <- e2_mi_30_sum_ccc_indexes %>%
  # Calculate the maximum ccc_count for each commune
  group_by(commune) %>%
  mutate(ccc_count_mi_30 = max(ccc_count),
  	ccc_time_mi_30 = mean(ccc_time),
  	ccc_confirm_mi_30 = mean(ccc_confirm)) %>%
  ungroup() %>%
  # Remove columns start_day and end_day
  dplyr::select(-c("start_time", "end_time", 
  	"ccc_count", "ccc_time", "ccc_confirm")) %>%
  # Remove duplicate rows
  distinct()

######
# **** C8 - Merge data -------------
######
e2_merge <- merge(e2_raw_ccc_indexes, e2_mi_05_ccc_indexes, by = "commune", all.x = TRUE)
e2_merge <- merge(e2_merge, e2_mi_10_ccc_indexes, by = "commune", all.x = TRUE)
e2_merge <- merge(e2_merge, e2_mi_15_ccc_indexes, by = "commune", all.x = TRUE)
e2_merge <- merge(e2_merge, e2_mi_20_ccc_indexes, by = "commune", all.x = TRUE)
e2_merge <- merge(e2_merge, e2_mi_25_ccc_indexes, by = "commune", all.x = TRUE)
e2_merge <- merge(e2_merge, e2_mi_30_ccc_indexes, by = "commune", all.x = TRUE)

e2_merge[is.na(e2_merge)] <- 0

######
# ** Part D ** Calculate Percentage change of CCC indexes --------
######

colnames(e2_merge)

e2_merge <- e2_merge %>%
	mutate(
		# Percentage change in ccc count
		change_count_05 = sqrt((ccc_count_raw - ccc_count_mi_05)^2),
		change_count_10 = sqrt((ccc_count_raw - ccc_count_mi_10)^2),
		change_count_15 = sqrt((ccc_count_raw - ccc_count_mi_15)^2),
		change_count_20 = sqrt((ccc_count_raw - ccc_count_mi_20)^2),
		change_count_25 = sqrt((ccc_count_raw - ccc_count_mi_25)^2),
		change_count_30 = sqrt((ccc_count_raw - ccc_count_mi_30)^2),
		# Percentage change in ccc time
		change_time_05 = sqrt((ccc_time_raw - ccc_time_mi_05)^2),
		change_time_10 = sqrt((ccc_time_raw - ccc_time_mi_10)^2),
		change_time_15 = sqrt((ccc_time_raw - ccc_time_mi_15)^2),
		change_time_20 = sqrt((ccc_time_raw - ccc_time_mi_20)^2),
		change_time_25 = sqrt((ccc_time_raw - ccc_time_mi_25)^2),
		change_time_30 = sqrt((ccc_time_raw - ccc_time_mi_30)^2),
		# Percentage change in ccc confirm case
		change_confirm_05 = sqrt((ccc_confirm_raw - ccc_confirm_mi_05)^2),
		change_confirm_10 = sqrt((ccc_confirm_raw - ccc_confirm_mi_10)^2),
		change_confirm_15 = sqrt((ccc_confirm_raw - ccc_confirm_mi_15)^2),
		change_confirm_20 = sqrt((ccc_confirm_raw - ccc_confirm_mi_20)^2),
		change_confirm_25 = sqrt((ccc_confirm_raw - ccc_confirm_mi_25)^2),
		change_confirm_30 = sqrt((ccc_confirm_raw - ccc_confirm_mi_30)^2)
	)


# **** D1 Percentage change of CCC count **** -------
######

e2_merge_ch_count_05 <- e2_merge %>%
	dplyr::select(c("change_count_05", "ccc_count_raw")) %>%
	mutate(per_change_count = change_count_05/ccc_count_raw*100,
		level = "05 %")%>%
	dplyr::select(c("per_change_count", "level"))

e2_merge_ch_count_10 <- e2_merge %>%
	dplyr::select(c("change_count_10", "ccc_count_raw")) %>%
	mutate(per_change_count = change_count_10/ccc_count_raw*100,
		level = "10 %")%>%
	dplyr::select(c("per_change_count", "level"))

e2_merge_ch_count_15 <- e2_merge %>%
	dplyr::select(c("change_count_15", "ccc_count_raw")) %>%
	mutate(per_change_count = change_count_15/ccc_count_raw*100,
		level = "15 %")%>%
	dplyr::select(c("per_change_count", "level"))

e2_merge_ch_count_20 <- e2_merge %>%
	dplyr::select(c("change_count_20", "ccc_count_raw")) %>%
	mutate(per_change_count = change_count_20/ccc_count_raw*100,
		level = "20 %")%>%
	dplyr::select(c("per_change_count", "level"))

e2_merge_ch_count_25 <- e2_merge %>%
	dplyr::select(c("change_count_25", "ccc_count_raw")) %>%
	mutate(per_change_count = change_count_25/ccc_count_raw*100,
		level = "25 %")%>%
	dplyr::select(c("per_change_count", "level"))

e2_merge_ch_count_30 <- e2_merge %>%
	dplyr::select(c("change_count_30", "ccc_count_raw")) %>%
	mutate(per_change_count = change_count_30/ccc_count_raw*100,
		level = "30 %")%>%
	dplyr::select(c("per_change_count", "level"))

e2_merge_ch_count_sum <- rbind(e2_merge_ch_count_05, e2_merge_ch_count_10,
	e2_merge_ch_count_15, e2_merge_ch_count_20,
	e2_merge_ch_count_25, e2_merge_ch_count_30)

e2_merge_ch_count_sum$method <- "2-weight"

library(writexl)

write_xlsx(e2_merge_ch_count_sum,"e2_ch_count.xlsx")

# **** D2 Percentage change of CCC time **** -------
######

e2_merge_ch_time_05 <- e2_merge %>%
	dplyr::select(c("change_time_05", "ccc_time_raw")) %>%
	mutate(per_change_time = change_time_05/ccc_time_raw*100,
		level = "05 %")%>%
	dplyr::select(c("per_change_time", "level"))

e2_merge_ch_time_10 <- e2_merge %>%
	dplyr::select(c("change_time_10", "ccc_time_raw")) %>%
	mutate(per_change_time = change_time_10/ccc_time_raw*100,
		level = "10 %")%>%
	dplyr::select(c("per_change_time", "level"))

e2_merge_ch_time_15 <- e2_merge %>%
	dplyr::select(c("change_time_15", "ccc_time_raw")) %>%
	mutate(per_change_time = change_time_15/ccc_time_raw*100,
		level = "15 %")%>%
	dplyr::select(c("per_change_time", "level"))

e2_merge_ch_time_20 <- e2_merge %>%
	dplyr::select(c("change_time_20", "ccc_time_raw")) %>%
	mutate(per_change_time = change_time_20/ccc_time_raw*100,
		level = "20 %")%>%
	dplyr::select(c("per_change_time", "level"))

e2_merge_ch_time_25 <- e2_merge %>%
	dplyr::select(c("change_time_25", "ccc_time_raw")) %>%
	mutate(per_change_time = change_time_25/ccc_time_raw*100,
		level = "25 %")%>%
	dplyr::select(c("per_change_time", "level"))

e2_merge_ch_time_30 <- e2_merge %>%
	dplyr::select(c("change_time_30", "ccc_time_raw")) %>%
	mutate(per_change_time = change_time_30/ccc_time_raw*100,
		level = "30 %")%>%
	dplyr::select(c("per_change_time", "level"))

e2_merge_ch_time_sum <- rbind(e2_merge_ch_time_05, e2_merge_ch_time_10,
	e2_merge_ch_time_15, e2_merge_ch_time_20,
	e2_merge_ch_time_25, e2_merge_ch_time_30)

e2_merge_ch_time_sum$method <- "2-weight"

library(writexl)

write_xlsx(e2_merge_ch_time_sum,"e2_ch_time.xlsx")

# **** D3 Percentage change of CCC confirm case **** -------

e2_merge_ch_confirm_05 <- e2_merge %>%
	dplyr::select(c("change_confirm_05", "ccc_confirm_raw")) %>%
	mutate(per_change_confirm = change_confirm_05/ccc_confirm_raw*100,
		level = "05 %")%>%
	dplyr::select(c("per_change_confirm", "level"))

e2_merge_ch_confirm_10 <- e2_merge %>%
	dplyr::select(c("change_confirm_10", "ccc_confirm_raw")) %>%
	mutate(per_change_confirm = change_confirm_10/ccc_confirm_raw*100,
		level = "10 %")%>%
	dplyr::select(c("per_change_confirm", "level"))

e2_merge_ch_confirm_15 <- e2_merge %>%
	dplyr::select(c("change_confirm_15", "ccc_confirm_raw")) %>%
	mutate(per_change_confirm = change_confirm_15/ccc_confirm_raw*100,
		level = "15 %")%>%
	dplyr::select(c("per_change_confirm", "level"))

e2_merge_ch_confirm_20 <- e2_merge %>%
	dplyr::select(c("change_confirm_20", "ccc_confirm_raw")) %>%
	mutate(per_change_confirm = change_confirm_20/ccc_confirm_raw*100,
		level = "20 %")%>%
	dplyr::select(c("per_change_confirm", "level"))

e2_merge_ch_confirm_25 <- e2_merge %>%
	dplyr::select(c("change_confirm_25", "ccc_confirm_raw")) %>%
	mutate(per_change_confirm = change_confirm_25/ccc_confirm_raw*100,
		level = "25 %")%>%
	dplyr::select(c("per_change_confirm", "level"))

e2_merge_ch_confirm_30 <- e2_merge %>%
	dplyr::select(c("change_confirm_30", "ccc_confirm_raw")) %>%
	mutate(per_change_confirm = change_confirm_30/ccc_confirm_raw*100,
		level = "30 %")%>%
	dplyr::select(c("per_change_confirm", "level"))

e2_merge_ch_confirm_sum <- rbind(e2_merge_ch_confirm_05, e2_merge_ch_confirm_10,
	e2_merge_ch_confirm_15, e2_merge_ch_confirm_20,
	e2_merge_ch_confirm_25, e2_merge_ch_confirm_30)

e2_merge_ch_confirm_sum$method <- "2-weight"

library(writexl)

write_xlsx(e2_merge_ch_confirm_sum,"e2_ch_confirm.xlsx")

######
# ** Part E ** Calculate the Crude Bias and Crude RMSE --------
######

# **** E1 Prepare **** ------------
# Remove column
b1_raw_1 <- b1_raw_1[, -which(names(b1_raw_1) == "period")]
e2_mi_05_1 <- e2_mi_05_1[, -which(names(e2_mi_05_1) == "period")]
e2_mi_10_1 <- e2_mi_10_1[, -which(names(e2_mi_10_1) == "period")]
e2_mi_15_1 <- e2_mi_15_1[, -which(names(e2_mi_15_1) == "period")]
e2_mi_20_1 <- e2_mi_20_1[, -which(names(e2_mi_20_1) == "period")]
e2_mi_25_1 <- e2_mi_25_1[, -which(names(e2_mi_25_1) == "period")]
e2_mi_30_1 <- e2_mi_30_1[, -which(names(e2_mi_30_1) == "period")]

# Rename the colummns
colnames(b1_raw_1) <- c("on_set_day", "commune", "case_raw")
colnames(e2_mi_05_1) <- c("on_set_day", "commune", "case_mi_05", "note_mi_05")
colnames(e2_mi_10_1) <- c("on_set_day", "commune", "case_mi_10", "note_mi_10")
colnames(e2_mi_15_1) <- c("on_set_day", "commune", "case_mi_15", "note_mi_15")
colnames(e2_mi_20_1) <- c("on_set_day", "commune", "case_mi_20", "note_mi_20")
colnames(e2_mi_25_1) <- c("on_set_day", "commune", "case_mi_25", "note_mi_25")
colnames(e2_mi_30_1) <- c("on_set_day", "commune", "case_mi_30", "note_mi_30")

# Merge the data
e2_crude_merge <- merge(b1_raw_1, e2_mi_05_1, by = c("on_set_day", "commune"), all=TRUE)
e2_crude_merge <- merge(e2_crude_merge, e2_mi_10_1, by = c("on_set_day", "commune"), all=TRUE)
e2_crude_merge <- merge(e2_crude_merge, e2_mi_15_1, by = c("on_set_day", "commune"), all=TRUE)
e2_crude_merge <- merge(e2_crude_merge, e2_mi_20_1, by = c("on_set_day", "commune"), all=TRUE)
e2_crude_merge <- merge(e2_crude_merge, e2_mi_25_1, by = c("on_set_day", "commune"), all=TRUE)
e2_crude_merge <- merge(e2_crude_merge, e2_mi_30_1, by = c("on_set_day", "commune"), all=TRUE)

######
# **** E2 Calculate the change **** --------
colnames(e2_crude_merge)
# 5 per
e2_crude_merge_05 <- e2_crude_merge %>%
	filter(note_mi_05 == "miss") %>%
	mutate(change_05 = case_raw - case_mi_05) 

e2_crude_05 <- e2_crude_merge_05 %>%
  summarise(
    crude_bias = sum(sqrt(change_05^2)) / n(),
    crude_rmse = sqrt(sum(change_05^2))/sqrt(n())) %>%
	mutate(level = "05 per")


# 10 per
e2_crude_merge_10 <- e2_crude_merge %>%
	filter(note_mi_10 == "miss") %>%
	mutate(change_10 = case_raw - case_mi_10) 

e2_crude_10 <- e2_crude_merge_10 %>%
  summarise(
    crude_bias = sum(sqrt(change_10^2)) / n(),
    crude_rmse = sqrt(sum(change_10^2)) / sqrt(n())) %>%
	mutate(level = "10 per")

# 15 per
e2_crude_merge_15 <- e2_crude_merge %>%
	filter(note_mi_15 == "miss") %>%
	mutate(change_15 = case_raw - case_mi_15) 

e2_crude_15 <- e2_crude_merge_15 %>%
  summarise(
    crude_bias = sum(sqrt(change_15 ^2)) / n(),
    crude_rmse = sqrt(sum(change_15^2)) / sqrt(n())) %>%
	mutate(level = "15 per")

# 20 per
e2_crude_merge_20 <- e2_crude_merge %>%
	filter(note_mi_20 == "miss") %>%
	mutate(change_20 = case_raw - case_mi_20)

e2_crude_20 <- e2_crude_merge_20 %>%
  summarise(
    crude_bias = sum(sqrt(change_20^2)) / n(),
    crude_rmse = sqrt(sum(change_20^2)) / sqrt(n())) %>%
	mutate(level = "20 per")

# 25 per
e2_crude_merge_25 <- e2_crude_merge %>%
	filter(note_mi_25 == "miss") %>%
	mutate(change_25 = case_raw - case_mi_25)

e2_crude_25 <- e2_crude_merge_25 %>%
  summarise(
    crude_bias = sum(sqrt(change_25^2)) / n(),
    crude_rmse = sqrt(sum(change_25^2)) / sqrt(n())) %>%
	mutate(level = "25 per")

# 30 per
e2_crude_merge_30 <- e2_crude_merge %>%
	filter(note_mi_30 == "miss") %>%
	mutate(change_30 = case_raw - case_mi_30)

e2_crude_30 <- e2_crude_merge_30 %>%
  summarise(
    crude_bias = sum(sqrt(change_30^2)) / n(),
    crude_rmse = sqrt(sum(change_30^2)) / sqrt(n())) %>%
	mutate(level = "30 per")

# Merge all
e2_crude_all <- rbind(e2_crude_05, e2_crude_10,
	e2_crude_15, e2_crude_20,
	e2_crude_25, e2_crude_30)

e2_crude_all$method <- "2-weight"

library(writexl)

write_xlsx(e2_crude_all,"e2_crude_all.xlsx")

######
# **** D3 -Percentage change in incidence rate ----------------
# Replace missing value

e2_graph_inci_merge <- replace(e2_graph_inci_merge, is.na(e2_graph_inci_merge), 0)

colnames(e2_graph_inci_merge)

e2_merge_ch_inci_05 <- e2_graph_inci_merge %>%
	dplyr::select(c("incidence_rate_05", "incidence_rate_raw")) %>%
	mutate(per_change_inci = sqrt((incidence_rate_05 - incidence_rate_raw)^2)/incidence_rate_raw*100,
		level = "05 %")%>%
	dplyr::select(c("per_change_inci", "level"))

e2_merge_ch_inci_10 <- e2_graph_inci_merge %>%
	dplyr::select(c("incidence_rate_10", "incidence_rate_raw")) %>%
	mutate(per_change_inci = sqrt((incidence_rate_10 - incidence_rate_raw)^2)/incidence_rate_raw*100,
		level = "10 %")%>%
	dplyr::select(c("per_change_inci", "level"))

e2_merge_ch_inci_15 <- e2_graph_inci_merge %>%
	dplyr::select(c("incidence_rate_15", "incidence_rate_raw")) %>%
	mutate(per_change_inci = sqrt((incidence_rate_15 - incidence_rate_raw)^2)/incidence_rate_raw*100,
		level = "15 %")%>%
	dplyr::select(c("per_change_inci", "level"))

e2_merge_ch_inci_20 <- e2_graph_inci_merge %>%
	dplyr::select(c("incidence_rate_20", "incidence_rate_raw")) %>%
	mutate(per_change_inci = sqrt((incidence_rate_20 - incidence_rate_raw)^2)/incidence_rate_raw*100,
		level = "20 %")%>%
	dplyr::select(c("per_change_inci", "level"))

e2_merge_ch_inci_25 <- e2_graph_inci_merge %>%
	dplyr::select(c("incidence_rate_25", "incidence_rate_raw")) %>%
	mutate(per_change_inci = sqrt((incidence_rate_25 - incidence_rate_raw)^2)/incidence_rate_raw*100,
		level = "25 %")%>%
	dplyr::select(c("per_change_inci", "level"))

e2_merge_ch_inci_30 <- e2_graph_inci_merge %>%
	dplyr::select(c("incidence_rate_30", "incidence_rate_raw")) %>%
	mutate(per_change_inci = sqrt((incidence_rate_30 - incidence_rate_raw)^2)/incidence_rate_raw*100,
		level = "30 %")%>%
	dplyr::select(c("per_change_inci", "level"))

e2_merge_ch_inci_sum <- rbind(e2_merge_ch_inci_05, e2_merge_ch_inci_10,
	e2_merge_ch_inci_15, e2_merge_ch_inci_20,
	e2_merge_ch_inci_25, e2_merge_ch_inci_30)

e2_merge_ch_inci_sum$method <- "2-weight"

library(writexl)

write_xlsx(e2_merge_ch_inci_sum,"e2_ch_inci.xlsx")
