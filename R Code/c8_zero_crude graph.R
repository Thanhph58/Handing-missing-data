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
c1_crude_all <- read_excel("c1_crude_all.xlsx")
c2_crude_all <- read_excel("c2_crude_all.xlsx")
c3_crude_all <- read_excel("c3_crude_all.xlsx")
c4_crude_all <- read_excel("c4_crude_all.xlsx")
c5_crude_all <- read_excel("c5_crude_all.xlsx")
c6_crude_all <- read_excel("c6_crude_all.xlsx")
c7_crude_all <- read_excel("c7_crude_all.xlsx")

######
# ******** A4 Incidence rate ******** -------------
######
# **** A4.1 Merge data set -------------
c8_crude_all <- rbind(c1_crude_all, c2_crude_all, c3_crude_all, 
	c4_crude_all, c5_crude_all, c6_crude_all, c7_crude_all)

table(c8_crude_all$method)

# Update the levels and labels for the factor
c8_crude_all$method <- factor(c8_crude_all$method, 
                              labels = c("Backfill", "Weight", "Median", 
                              	"Maximum", "Linear", "ARIMA",
                              	"KNN"))

c8_crude_all$level <- factor(c8_crude_all$level, 
                              labels = c("5%", "10%", "15%", "20%", "25%", "30%"))

colnames(c8_crude_all)

library(writexl)

write_xlsx(c8_crude_all,"c8_crude_all.xlsx")


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
c8_graph_a <- ggplot(c8_crude_all, aes(x = level, y = crude_bias, 
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

c8_graph_a

# Create the line + point graph for Crude RMSE
c8_graph_b <- ggplot(c8_crude_all, aes(x = level, y = crude_rmse, 
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

c8_graph_b

# Merge the graphs 
# Load the required packages
library(gridExtra)
library(ggplot2)


# Create a list of the six ggplot graphs
c8_plot_list <- list(c8_graph_a, c8_graph_b)

# Arrange the plots
c8_combined_plot <- grid.arrange(
  grobs = c8_plot_list,
  ncol = 1  
)

# Explore the plot
ggsave(
	filename = "c8_combined_plot.PNG", 
  plot = c8_combined_plot, 
  width = 12, height = 6, 
  units = "in", dpi = 600)

######
# Part B Percentage change of CCC table -----------------
######
######
setwd("D:/0-Thesis/1-Data_incidence rate/6_Data per change")
# **** B1- Import the excel data sets -------
# CCC count
c1_ch_count <- read_excel("c1_ch_count.xlsx")
c2_ch_count <- read_excel("c2_ch_count.xlsx")
c3_ch_count <- read_excel("c3_ch_count.xlsx")
c4_ch_count <- read_excel("c4_ch_count.xlsx")
c5_ch_count <- read_excel("c5_ch_count.xlsx")
c6_ch_count <- read_excel("c6_ch_count.xlsx")
c7_ch_count <- read_excel("c7_ch_count.xlsx")

# CCC time
c1_ch_time <- read_excel("c1_ch_time.xlsx")
c2_ch_time <- read_excel("c2_ch_time.xlsx")
c3_ch_time <- read_excel("c3_ch_time.xlsx")
c4_ch_time <- read_excel("c4_ch_time.xlsx")
c5_ch_time <- read_excel("c5_ch_time.xlsx")
c6_ch_time <- read_excel("c6_ch_time.xlsx")
c7_ch_time <- read_excel("c7_ch_time.xlsx")

# CCC confirm cases
c1_ch_confirm <- read_excel("c1_ch_confirm.xlsx")
c2_ch_confirm <- read_excel("c2_ch_confirm.xlsx")
c3_ch_confirm <- read_excel("c3_ch_confirm.xlsx")
c4_ch_confirm <- read_excel("c4_ch_confirm.xlsx")
c5_ch_confirm <- read_excel("c5_ch_confirm.xlsx")
c6_ch_confirm <- read_excel("c6_ch_confirm.xlsx")
c7_ch_confirm <- read_excel("c7_ch_confirm.xlsx")

######
# **** B2 - Merge data ------------------

# CCC count
c8_ch_count <- rbind(c1_ch_count, c2_ch_count, c3_ch_count, 
	c4_ch_count, c5_ch_count, c6_ch_count, c7_ch_count)

table(c8_ch_count$method)
colnames(c8_ch_count)

# CCC time
c8_ch_time <- rbind(c1_ch_time, c2_ch_time, c3_ch_time, 
	c4_ch_time, c5_ch_time, c6_ch_time, c7_ch_time)

table(c8_ch_time$method)
colnames(c8_ch_time)

# CCC confirm cases
c8_ch_confirm <- rbind(c1_ch_confirm, c2_ch_confirm, c3_ch_confirm, 
	c4_ch_confirm, c5_ch_confirm, c6_ch_confirm, c7_ch_confirm)

table(c8_ch_confirm$method)
colnames(c8_ch_confirm)

######
# **** B3 - Table ------------------
# CCC count
c8_ch_count$method <- factor(c8_ch_count$method)

c8_table_ch_count <- c8_ch_count %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_count),
    SE = sd(per_change_count) / sqrt(n())
  )

write_xlsx(c8_table_ch_count,"c8_2_table_ch_count.xlsx")

# CCC time
c8_ch_time$method <- factor(c8_ch_time$method)

c8_table_ch_time <- c8_ch_time %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_time),
    SE = sd(per_change_time) / sqrt(n())
  )

write_xlsx(c8_table_ch_time,"c8_3_table_ch_time.xlsx")

# CCC confirm cases
c8_ch_confirm$method <- factor(c8_ch_confirm$method)

c8_table_ch_confirm <- c8_ch_confirm %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_confirm),
    SE = sd(per_change_confirm) / sqrt(n())
  )

write_xlsx(c8_table_ch_confirm,"c8_4_table_ch_confirm.xlsx")


######
# Part C Percentage change of CCC table -----------------
######

# **** C1- Import the excel data sets -------
# CCC count
c0_ch_count <- read_excel("c0_ch_count.xlsx")

# CCC time
c0_ch_time <- read_excel("c0_ch_time.xlsx")

# CCC confirm cases
c0_ch_confirm <- read_excel("c0_ch_confirm.xlsx")

# **** C2 - Table ------------------
# CCC count
c0_table_ch_count <- c0_ch_count %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_count),
    SE = sd(per_change_count) / sqrt(n())
  )

write_xlsx(c0_table_ch_count,"c0_table_ch_count.xlsx")

# CCC time
c0_table_ch_time <- c0_ch_time %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_time),
    SE = sd(per_change_time) / sqrt(n())
  )

write_xlsx(c0_table_ch_time,"c0_table_ch_time.xlsx")

# CCC confirm cases
c0_table_ch_confirm <- c0_ch_confirm %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_confirm),
    SE = sd(per_change_confirm) / sqrt(n())
  )

write_xlsx(c0_table_ch_confirm,"c0_table_ch_confirm.xlsx")



######
# Part D Percentage change of incidence rate table -----------------
######

# Incidence rate change
c1_ch_inci <- read_excel("c1_ch_inci.xlsx")
c2_ch_inci <- read_excel("c2_ch_inci.xlsx")
c3_ch_inci <- read_excel("c3_ch_inci.xlsx")
c4_ch_inci <- read_excel("c4_ch_inci.xlsx")
c5_ch_inci <- read_excel("c5_ch_inci.xlsx")
c6_ch_inci <- read_excel("c6_ch_inci.xlsx")
c7_ch_inci <- read_excel("c7_ch_inci.xlsx")

# Merge
c8_ch_inci <- rbind(c1_ch_inci, c2_ch_inci, c3_ch_inci, 
	c4_ch_inci, c5_ch_inci, c6_ch_inci, c7_ch_inci)

c8_ch_inci$per_change_inci <- as.numeric(c8_ch_inci$per_change_inci)

c8_ch_inci[is.na(c8_ch_inci)] <- 0

c8_ch_inci$per_change_inci <- ifelse(c8_ch_inci$per_change_inci == "Inf", "0", c8_ch_inci$per_change_inci)

c8_ch_inci$per_change_inci <- as.numeric(c8_ch_inci$per_change_inci)

# Table
c8_table_ch_inci <- c8_ch_inci %>%
  group_by(method, level) %>%
  summarise(
    Mean = mean(per_change_inci),
    SE = sd(per_change_inci) / sqrt(n())
  )

write_xlsx(c8_table_ch_inci,"c8_1_table_ch_inci.xlsx")

# incidence rate raw vs miss
c0_ch_inci <- read_excel("c0_ch_inci.xlsx")

c0_table_ch_inci <- c0_ch_inci %>%
  group_by(level) %>%
  summarise(
    Mean = mean(per_change_inci),
    SE = sd(per_change_inci) / sqrt(n())
  )

write_xlsx(c0_table_ch_inci,"c0_table_ch_inci.xlsx")
