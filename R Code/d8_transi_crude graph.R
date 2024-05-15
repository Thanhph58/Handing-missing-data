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
d1_crude_all <- read_excel("d1_crude_all.xlsx")
d2_crude_all <- read_excel("d2_crude_all.xlsx")
d3_crude_all <- read_excel("d3_crude_all.xlsx")
d4_crude_all <- read_excel("d4_crude_all.xlsx")
d5_crude_all <- read_excel("d5_crude_all.xlsx")
d6_crude_all <- read_excel("d6_crude_all.xlsx")
d7_crude_all <- read_excel("d7_crude_all.xlsx")

######
# ******** A4 Incidence rate ******** -------------
######
# **** A4.1 Merge data set -------------
d8_crude_all <- rbind(d1_crude_all, d2_crude_all, d3_crude_all, 
	d4_crude_all, d5_crude_all, d6_crude_all, d7_crude_all)

table(d8_crude_all$method)

# Update the levels and labels for the factor
d8_crude_all$method <- factor(d8_crude_all$method, 
                              labels = c("Backfill", "Weight", "Median", 
                              	"Maximum", "Linear", "ARIMA",
                              	"KNN"))

d8_crude_all$level <- factor(d8_crude_all$level, 
                              labels = c("5%", "10%", "15%", "20%", "25%", "30%"))

colnames(d8_crude_all)

library(writexl)

write_xlsx(d8_crude_all,"d8_crude_all.xlsx")


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
d8_graph_a <- ggplot(d8_crude_all, aes(x = level, y = crude_bias, 
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

d8_graph_a

# Create the line + point graph for Crude RMSE
d8_graph_b <- ggplot(d8_crude_all, aes(x = level, y = crude_rmse, 
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

d8_graph_b

# Merge the graphs 
# Load the required packages
library(gridExtra)
library(ggplot2)


# Create a list of the six ggplot graphs
d8_plot_list <- list(d8_graph_a, d8_graph_b)

# Arrange the plots
d8_combined_plot <- grid.arrange(
  grobs = d8_plot_list,
  ncol = 1  
)

# Explore the plot
ggsave(
	filename = "d8_combined_plot.PNG", 
  plot = d8_combined_plot, 
  width = 12, height = 6, 
  units = "in", dpi = 600)

######
# Part B Percentage change of CCC table -----------------
######
######
setwd("D:/0-Thesis/1-Data_incidence rate/6_Data per change")
# **** B1- Import the excel data sets -------
# CCC count
d1_ch_count <- read_excel("d1_ch_count.xlsx")
d2_ch_count <- read_excel("d2_ch_count.xlsx")
d3_ch_count <- read_excel("d3_ch_count.xlsx")
d4_ch_count <- read_excel("d4_ch_count.xlsx")
d5_ch_count <- read_excel("d5_ch_count.xlsx")
d6_ch_count <- read_excel("d6_ch_count.xlsx")
d7_ch_count <- read_excel("d7_ch_count.xlsx")

# CCC time
d1_ch_time <- read_excel("d1_ch_time.xlsx")
d2_ch_time <- read_excel("d2_ch_time.xlsx")
d3_ch_time <- read_excel("d3_ch_time.xlsx")
d4_ch_time <- read_excel("d4_ch_time.xlsx")
d5_ch_time <- read_excel("d5_ch_time.xlsx")
d6_ch_time <- read_excel("d6_ch_time.xlsx")
d7_ch_time <- read_excel("d7_ch_time.xlsx")

# CCC confirm cases
d1_ch_confirm <- read_excel("d1_ch_confirm.xlsx")
d2_ch_confirm <- read_excel("d2_ch_confirm.xlsx")
d3_ch_confirm <- read_excel("d3_ch_confirm.xlsx")
d4_ch_confirm <- read_excel("d4_ch_confirm.xlsx")
d5_ch_confirm <- read_excel("d5_ch_confirm.xlsx")
d6_ch_confirm <- read_excel("d6_ch_confirm.xlsx")
d7_ch_confirm <- read_excel("d7_ch_confirm.xlsx")

######
# **** B2 - Merge data ------------------

# CCC count
d8_ch_count <- rbind(d1_ch_count, d2_ch_count, d3_ch_count, 
	d4_ch_count, d5_ch_count, d6_ch_count, d7_ch_count)

table(d8_ch_count$method)
colnames(d8_ch_count)

# CCC time
d8_ch_time <- rbind(d1_ch_time, d2_ch_time, d3_ch_time, 
	d4_ch_time, d5_ch_time, d6_ch_time, d7_ch_time)

table(d8_ch_time$method)
colnames(d8_ch_time)

# CCC confirm cases
d8_ch_confirm <- rbind(d1_ch_confirm, d2_ch_confirm, d3_ch_confirm, 
	d4_ch_confirm, d5_ch_confirm, d6_ch_confirm, d7_ch_confirm)

table(d8_ch_confirm$method)
colnames(d8_ch_confirm)

######
# **** B3 - Table ------------------
# CCC count
d8_ch_count$method <- factor(d8_ch_count$method)

d8_table_ch_count <- d8_ch_count %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_count),
    SE = sd(per_change_count) / sqrt(n())
  )

write_xlsx(d8_table_ch_count,"d8_2_table_ch_count.xlsx")

# CCC time
d8_ch_time$method <- factor(d8_ch_time$method)

d8_table_ch_time <- d8_ch_time %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_time),
    SE = sd(per_change_time) / sqrt(n())
  )

write_xlsx(d8_table_ch_time,"d8_3_table_ch_time.xlsx")

# CCC confirm cases
d8_ch_confirm$method <- factor(d8_ch_confirm$method)

d8_table_ch_confirm <- d8_ch_confirm %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_confirm),
    SE = sd(per_change_confirm) / sqrt(n())
  )

write_xlsx(d8_table_ch_confirm,"d8_4_table_ch_confirm.xlsx")


######
# Part C Percentage change of CCC table -----------------
######

# **** C1- Import the excel data sets -------
# CCC count
d0_ch_count <- read_excel("d0_ch_count.xlsx")

# CCC time
d0_ch_time <- read_excel("d0_ch_time.xlsx")

# CCC confirm cases
d0_ch_confirm <- read_excel("d0_ch_confirm.xlsx")

# **** C2 - Table ------------------
# CCC count
d0_table_ch_count <- d0_ch_count %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_count),
    SE = sd(per_change_count) / sqrt(n())
  )

write_xlsx(d0_table_ch_count,"d0_table_ch_count.xlsx")

# CCC time
d0_table_ch_time <- d0_ch_time %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_time),
    SE = sd(per_change_time) / sqrt(n())
  )

write_xlsx(d0_table_ch_time,"d0_table_ch_time.xlsx")

# CCC confirm cases
d0_table_ch_confirm <- d0_ch_confirm %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_confirm),
    SE = sd(per_change_confirm) / sqrt(n())
  )

write_xlsx(d0_table_ch_confirm,"d0_table_ch_confirm.xlsx")



######
# Part D Percentage change of incidence rate table -----------------
######

# Incidence rate change
d1_ch_inci <- read_excel("d1_ch_inci.xlsx")
d2_ch_inci <- read_excel("d2_ch_inci.xlsx")
d3_ch_inci <- read_excel("d3_ch_inci.xlsx")
d4_ch_inci <- read_excel("d4_ch_inci.xlsx")
d5_ch_inci <- read_excel("d5_ch_inci.xlsx")
d6_ch_inci <- read_excel("d6_ch_inci.xlsx")
d7_ch_inci <- read_excel("d7_ch_inci.xlsx")

# Merge
d8_ch_inci <- rbind(d1_ch_inci, d2_ch_inci, d3_ch_inci, 
	d4_ch_inci, d5_ch_inci, d6_ch_inci, d7_ch_inci)

d8_ch_inci$per_change_inci <- as.numeric(d8_ch_inci$per_change_inci)

d8_ch_inci[is.na(d8_ch_inci)] <- 0

d8_ch_inci$per_change_inci <- ifelse(d8_ch_inci$per_change_inci == "Inf", "0", d8_ch_inci$per_change_inci)

d8_ch_inci$per_change_inci <- as.numeric(d8_ch_inci$per_change_inci)

# Table
d8_table_ch_inci <- d8_ch_inci %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_inci),
    SE = sd(per_change_inci) / sqrt(n())
  )

write_xlsx(d8_table_ch_inci,"d8_1_table_ch_inci.xlsx")

# incidence rate raw vs miss
d0_ch_inci <- read_excel("d0_ch_inci.xlsx")

d0_table_ch_inci <- d0_ch_inci %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_inci),
    SE = sd(per_change_inci) / sqrt(n())
  )

write_xlsx(d0_table_ch_inci,"d0_table_ch_inci.xlsx")
