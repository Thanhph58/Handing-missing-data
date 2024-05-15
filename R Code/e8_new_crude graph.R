# File c6
# Author PHT
# Date 08/04/2023
# Modify history:
#       1-08/04/2023
#       * Check the code
#       * Split the code file
#       1-09/21/2023
#       * Update the working direction
#       * Update the figure title and themes

######
# ** Part A ** Preparing for the data ******** -----------
######
# **** A1- Cleaning the work space -------
######
rm(list = ls())

# **** A2- Set the working directory -------
######
setwd("D:/0-Thesis/1-Data_incidence rate/5_crude of each methods")

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
library(writexl)
library(segmented)
library(MASS)
library(scales)
######
# **** A3- Import the excel data sets -------
e1_crude_all <- read_excel("e1_crude_all.xlsx")
e2_crude_all <- read_excel("e2_crude_all.xlsx")
e3_crude_all <- read_excel("e3_crude_all.xlsx")
e4_crude_all <- read_excel("e4_crude_all.xlsx")
e5_crude_all <- read_excel("e5_crude_all.xlsx")
e6_crude_all <- read_excel("e6_crude_all.xlsx")
e7_crude_all <- read_excel("e7_crude_all.xlsx")

######
# ******** A4 Incidence rate ******** -------------
######
# **** A4.1 Merge data set -------------
e8_crude_all <- rbind(e1_crude_all, e2_crude_all, e3_crude_all, 
	e4_crude_all, e5_crude_all, e6_crude_all, e7_crude_all)

table(e8_crude_all$method)

# Update the levels and labels for the factor
e8_crude_all$method <- factor(e8_crude_all$method, 
                              labels = c("Backfill", "Weight", "Median", 
                              	"Maximum", "Linear", "ARIMA",
                              	"KNN"))

e8_crude_all$level <- factor(e8_crude_all$level, 
                              labels = c("5%", "10%", "15%", "20%", "25%", "30%"))

colnames(e8_crude_all)

library(writexl)

write_xlsx(e8_crude_all,"e8_crude_all.xlsx")


#####
# **** A4.2 Draw the graph **** -------------

# Load required libraries
library(ggplot2)

# Create a custom color palette
method_colors <- c("Backfill" = "red", 
                   "Weight" = "blue", 
                   "Linear" = "black", 
                   "Maximum" = "green", 
                   "Median" = "purple", 
                   "ARIMA" = "orange",
                   "KNN" = "chocolate")

# Create the line + point graph for Crude bias 
e8_graph_a <- ggplot(e8_crude_all, aes(x = level, y = crude_bias, 
                                        color = method, 
                                        group = method)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Percentage of missing data", 
  	y = expression(paste(bar("ACB")))) +  # Add overline symbol for mean RMSE) +
  theme_classic() + 
  theme( 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "none",  # Remove the legend
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = method_colors)  # Use the custom color palette

e8_graph_a

# Create the line + point graph for Crude RMSE
e8_graph_b <- ggplot(e8_crude_all, aes(x = level, y = crude_rmse, 
                                        color = method, 
                                        group = method)) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Percentage of missing data", 
  	y = expression(paste(bar("RMSE")))) +
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
  scale_color_manual(name = "Methods:",
                     labels = c("Backfill\nimputation", 
                     	"Moving average\nimputation", 
                     	"Median\nimputation", 
                     	"Maximum likelihood\nimputation", 
                     	"Linear interpolation\nimputation", 
                     	"ARIMA model\nimputation",
                     	"K-nearest neighbor\nimputation"),
                     values = method_colors)

e8_graph_b

# Merge the graphs 
# Load the required packages
library(gridExtra)
library(ggplot2)


# Create a list of the six ggplot graphs
e8_plot_list <- list(e8_graph_a, e8_graph_b)

# Arrange the plots
e8_combined_plot <- grid.arrange(
  grobs = e8_plot_list,
  ncol = 1  
)

# Explore the plot
ggsave(
	filename = "e8_combined_plot.PNG", 
  plot = e8_combined_plot, 
  width = 12, height = 6, 
  units = "in", dpi = 600)

######
# Part B Percentage change of CCC table -----------------
######
######
setwd("D:/0-Thesis/1-Data_incidence rate/6_Data per change")
# **** B1- Import the excel data sets -------
# CCC count
e1_ch_count <- read_excel("e1_ch_count.xlsx")
e2_ch_count <- read_excel("e2_ch_count.xlsx")
e3_ch_count <- read_excel("e3_ch_count.xlsx")
e4_ch_count <- read_excel("e4_ch_count.xlsx")
e5_ch_count <- read_excel("e5_ch_count.xlsx")
e6_ch_count <- read_excel("e6_ch_count.xlsx")
e7_ch_count <- read_excel("e7_ch_count.xlsx")

# CCC time
e1_ch_time <- read_excel("e1_ch_time.xlsx")
e2_ch_time <- read_excel("e2_ch_time.xlsx")
e3_ch_time <- read_excel("e3_ch_time.xlsx")
e4_ch_time <- read_excel("e4_ch_time.xlsx")
e5_ch_time <- read_excel("e5_ch_time.xlsx")
e6_ch_time <- read_excel("e6_ch_time.xlsx")
e7_ch_time <- read_excel("e7_ch_time.xlsx")

# CCC confirm cases
e1_ch_confirm <- read_excel("e1_ch_confirm.xlsx")
e2_ch_confirm <- read_excel("e2_ch_confirm.xlsx")
e3_ch_confirm <- read_excel("e3_ch_confirm.xlsx")
e4_ch_confirm <- read_excel("e4_ch_confirm.xlsx")
e5_ch_confirm <- read_excel("e5_ch_confirm.xlsx")
e6_ch_confirm <- read_excel("e6_ch_confirm.xlsx")
e7_ch_confirm <- read_excel("e7_ch_confirm.xlsx")

######
# **** B2 - Merge data ------------------

# CCC count
e8_ch_count <- rbind(e1_ch_count, e2_ch_count, e3_ch_count, 
	e4_ch_count, e5_ch_count, e6_ch_count, e7_ch_count)

table(e8_ch_count$method)
colnames(e8_ch_count)

# CCC time
e8_ch_time <- rbind(e1_ch_time, e2_ch_time, e3_ch_time, 
	e4_ch_time, e5_ch_time, e6_ch_time, e7_ch_time)

table(e8_ch_time$method)
colnames(e8_ch_time)

# CCC confirm cases
e8_ch_confirm <- rbind(e1_ch_confirm, e2_ch_confirm, e3_ch_confirm, 
	e4_ch_confirm, e5_ch_confirm, e6_ch_confirm, e7_ch_confirm)

table(e8_ch_confirm$method)
colnames(e8_ch_confirm)

######
# **** B3 - Table ------------------
# CCC count
e8_ch_count$method <- factor(e8_ch_count$method)

e8_table_ch_count <- e8_ch_count %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_count),
    SE = sd(per_change_count) / sqrt(n())
  )

write_xlsx(e8_table_ch_count,"e8_2_table_ch_count.xlsx")

# CCC time
e8_ch_time$method <- factor(e8_ch_time$method)

e8_table_ch_time <- e8_ch_time %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_time),
    SE = sd(per_change_time) / sqrt(n())
  )

write_xlsx(e8_table_ch_time,"e8_3_table_ch_time.xlsx")

# CCC confirm cases
e8_ch_confirm$method <- factor(e8_ch_confirm$method)

e8_table_ch_confirm <- e8_ch_confirm %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_confirm),
    SE = sd(per_change_confirm) / sqrt(n())
  )

write_xlsx(e8_table_ch_confirm,"e8_4_table_ch_confirm.xlsx")


######
# Part C Percentage change of CCC table -----------------
######

# **** C1- Import the excel data sets -------
# CCC count
e0_ch_count <- read_excel("e0_ch_count.xlsx")

# CCC time
e0_ch_time <- read_excel("e0_ch_time.xlsx")

# CCC confirm cases
e0_ch_confirm <- read_excel("e0_ch_confirm.xlsx")

# **** C2 - Table ------------------
# CCC count
e0_table_ch_count <- e0_ch_count %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_count),
    SE = sd(per_change_count) / sqrt(n())
  )

write_xlsx(e0_table_ch_count,"e0_table_ch_count.xlsx")

# CCC time
e0_table_ch_time <- e0_ch_time %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_time),
    SE = sd(per_change_time) / sqrt(n())
  )

write_xlsx(e0_table_ch_time,"e0_table_ch_time.xlsx")

# CCC confirm cases
e0_table_ch_confirm <- e0_ch_confirm %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_confirm),
    SE = sd(per_change_confirm) / sqrt(n())
  )

write_xlsx(e0_table_ch_confirm,"e0_table_ch_confirm.xlsx")



######
# Part D Percentage change of incidence rate table -----------------
######

# Incidence rate change
e1_ch_inci <- read_excel("e1_ch_inci.xlsx")
e2_ch_inci <- read_excel("e2_ch_inci.xlsx")
e3_ch_inci <- read_excel("e3_ch_inci.xlsx")
e4_ch_inci <- read_excel("e4_ch_inci.xlsx")
e5_ch_inci <- read_excel("e5_ch_inci.xlsx")
e6_ch_inci <- read_excel("e6_ch_inci.xlsx")
e7_ch_inci <- read_excel("e7_ch_inci.xlsx")

# Merge
e8_ch_inci <- rbind(e1_ch_inci, e2_ch_inci, e3_ch_inci, 
	e4_ch_inci, e5_ch_inci, e6_ch_inci, e7_ch_inci)

e8_ch_inci$per_change_inci <- as.numeric(e8_ch_inci$per_change_inci)

e8_ch_inci[is.na(e8_ch_inci)] <- 0

e8_ch_inci$per_change_inci <- ifelse(e8_ch_inci$per_change_inci == "Inf", "0", e8_ch_inci$per_change_inci)

e8_ch_inci$per_change_inci <- as.numeric(e8_ch_inci$per_change_inci)

# Table
e8_table_ch_inci <- e8_ch_inci %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_inci),
    SE = sd(per_change_inci) / sqrt(n())
  )

write_xlsx(e8_table_ch_inci,"e8_1_table_ch_inci.xlsx")

# incidence rate raw vs miss
e0_ch_inci <- read_excel("e0_ch_inci.xlsx")

e0_table_ch_inci <- e0_ch_inci %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_inci),
    SE = sd(per_change_inci) / sqrt(n())
  )

write_xlsx(e0_table_ch_inci,"e0_table_ch_inci.xlsx")
