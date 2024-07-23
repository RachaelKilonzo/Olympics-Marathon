

#INSTALL PACKAGES

#install.packages("devtools")
#devtools::install_github("BlakeRMills/MoMAColors")
#install.packages("scales")
#install.packages("colorspace")
#install.packages("ggtext")
#install.packages("ggpubr")
#install.packages("cowplot")


#LOAD LIBRARY

library(tidyverse)
library(sysfonts)
library(showtext)
library(ggforce)
library(cowplot)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(colorspace)
library(ggblur)
library(stringr)
library(ggtext)
library(MoMAColors)
library(ggpubr)
library(cowplot)


#FONTS

showtext_auto()
font_add_google("Playball")
font1 = "Playball"
font_add_google("Tourney")
font2 = "Tourney"


# DATA WRANGLING AND CLEANING

# Read the CSV file
olympics = read_csv('C:/Users/PC/Desktop/Portfolio/olympics/HistoricOlympicData.csv')
# Function to convert time format from x-xx:xx or x-xx:xx.x to hh:mm:ss or hh:mm:ss.s
convert_time_format = function(time_str) {
  gsub("-", ":", time_str)
}
# Function to convert hh:mm:ss or hh:mm:ss.s format to seconds
time_to_seconds = function(time_str) {
  parts = unlist(strsplit(time_str, ":"))
  hours = as.numeric(parts[1])
  minutes = as.numeric(parts[2])
  seconds = as.numeric(parts[3])
  
  total_seconds = hours * 3600 + minutes * 60 + seconds
  return(total_seconds)
}
# Function to convert hh:mm:ss or hh:mm:ss.s format to minutes
time_to_minutes = function(time_str) {
  parts = unlist(strsplit(time_str, ":"))
  hours = as.numeric(parts[1])
  minutes = as.numeric(parts[2])
  seconds = as.numeric(parts[3])
  
  total_minutes = hours * 60 + minutes + seconds / 60
  return(total_minutes)
}
# Create Year clusters for coloring
Unique_Years = unique(olympics$Year)
cluster_labels = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10)
year_to_cluster = setNames(cluster_labels, Unique_Years)
# Process the data
olympics = olympics %>%
  filter(!(Pos %in% c("DNF", "DQ", "DNS")) & Time != "-") %>%
  mutate(Time = str_trim(Time),
         Time = convert_time_format(Time),
         Seconds = sapply(Time, time_to_seconds),
         Minutes = sapply(Time, time_to_minutes),
         Cluster = year_to_cluster[as.character(Year)])
# Select top marathon runners and drop 'Nr' column
marathon = olympics %>%
  filter(Pos %in% 1:3) %>%
  select(-Nr) %>%
  mutate(Seconds = Seconds*-1,
    SecondsRank = rank(Seconds, ties.method = "first"),
         id = row_number()) %>%
  mutate(YearOrder = Year + seq(0, by = 0.002, length.out = n()))




# Filter data for positions 1 and 2
positions_1_2 = olympics %>%
  filter(Pos %in% 1:2)

# Calculate the difference in completion times between 1st and 2nd positions for each year
time_diff = positions_1_2 %>%
  group_by(Year) %>%
  summarise(Diff = abs(diff(Seconds)))

# Identify the year with the largest difference
year_largest_diffgs = time_diff %>%
  filter(Diff == max(Diff))

# Print the year with the largest difference and the difference in seconds
print(year_largest_diffgs)



# Filter data for positions 1, 2, and 3
positions_1_2_3 = olympics %>%
  filter(Pos %in% c(1, 2, 3))

# Calculate the sum of completion times for positions 1, 2, and 3 for each year
time_sum = positions_1_2_3 %>%
  group_by(Year) %>%
  summarise(SumTime = sum(Seconds))

# Identify the year with the minimal sum of completion times
year_minimal_sum = time_sum %>%
  filter(SumTime == min(SumTime, na.rm = TRUE))

# Print the year with the minimal sum of completion times and the sum
print(year_minimal_sum)

# Identify the year with the smallest difference
year_smallest_diffgs = time_diff %>%
  filter(Diff == min(Diff))

# Print the year with the smallest difference between gold and silver medalists in seconds
print(year_smallest_diffgs)



# Filter data for positions 1 and 3
positions_1_3 = olympics %>%
  filter(Pos %in% c(1, 3))
# Calculate the difference in completion times between 1st and 3rd positions for each year
time_diff = positions_1_3 %>%
  group_by(Year) %>%
  summarise(Diff = abs(Seconds[Pos == 1] - Seconds[Pos == 3]))
# Identify the year with the largest difference between gold and bronze medalists in seconds
year_largest_diffgb = time_diff %>%
  filter(Diff == max(Diff, na.rm = TRUE))
# Print the year with the largest difference between gold and bronze medalists in seconds
print(year_largest_diffgb)

# Identify the year with the smallest difference between gold and bronze medalists in seconds
year_smallest_diffgb = time_diff %>%
filter(Diff == min(Diff, na.rm = TRUE))
# Print the year with the smallest difference between gold and bronze medalists in seconds
print(year_smallest_diffgb)






#PLOTTING

# Define custom color palette from MoMAColors
klein_palette = moma.colors("Klein")[1:10]
# Custom color scale using the modified Klein palette
custom_color_scale = scale_colour_manual(values = klein_palette)
plt = ggplot(data = marathon) +
  geom_link(aes(x = Year, xend = Year, y = -13700, yend = Seconds, colour = factor(Cluster)), size = 2.0, alpha = 0.8) +
  geom_point(aes(x = Year, y = Seconds, colour = factor(Cluster)), size = 2.5) + 
  custom_color_scale +
  coord_polar(theta = "y", clip="off", start = 0.785) +
  scale_x_continuous(limits = c(1880, NA)) +
  scale_y_continuous(limits = c(NA, -7400)) +
  theme_void() +
  theme(plot.background = element_rect(fill="#afeeee", color="#afeeee"),
        legend.position = "none",
        plot.margin = margin(3, 1, 0.25, 0, "cm")) +
  annotate(geom = "text", label ="1896", x = 1896, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[1], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1906", x = 1906, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[2], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1920", x = 1920, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[3], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1932", x = 1932, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[4], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1952", x = 1952, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[5], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1964", x = 1964, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[6], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1976", x = 1976, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[7], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="1988", x = 1988, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[8], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="2000", x = 2000, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[9], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="2012", x = 2012, y = -13700, vjust = -0.1, hjust = 1.0, color = klein_palette[10], family = font1, size = 20,  fontface="bold") +
  annotate(geom = "text", label ="Olympics Game Year", x = 2020, y = -13700, vjust = -2.0, hjust = 1.2, color = klein_palette[1], family = font2, size = 40,  fontface="bold", angle = 45)


ggdraw(plt) +
  draw_label(label = "Olympics Marathon Medalists' Finish Times", x = 0.5, y = 0.96, color = klein_palette[2], fontface="bold", size=180, fontfamily = font2) +
  draw_label(label = "Github : Rachael Kilonzo | Source: Olympedia | Inspiration: BlakeRMills & Cedric Scherer", 
             x = 0.5, y = 0.015, color = klein_palette[3], fontface = "bold", size = 120, fontfamily = font1) +
  draw_label(label = "Sammy Wanjiru of Kenya\nOlympics marathon record holder\n2008 Beijing Olympics\nTime = 2:06:32", x=0.75, y=0.90, color=klein_palette[9], fontface="bold", size=58, fontfamily = font2, lineheight=0.26) +
  draw_label(label = "Spyros Louis of Greece\nFirst olympics gold medalist\n1896 Athens olympics\nTime = 2:58:50\n1896=Year with the largest time gap\nbetween the Gold & Silver\nMedalist", x=0.915, y=0.5, color=klein_palette[1], fontface="bold", size=58, fontfamily = font2, lineheight=0.26, angle = -90) +
  draw_label(label = "A century later\n1996=Year with the smallest time gap\nbetween the Gold & Silver\nand Gold & Bronze\nMedalists", x=0.85, y=0.80, color=klein_palette[8], fontface="bold", size=58, fontfamily = font2, lineheight=0.26) +
draw_label(label = "Eliud Kipchoge of Kenya\nLIVING marathon record holder\n2022 Berlin Marathon\nTime = 2:01:09", x=0.25, y=0.85, color=klein_palette[10], fontface="bold", size=58, fontfamily = font2, lineheight=0.26) +
  draw_label(label = "1900=Year with the largest time gap\nbetween the Gold & Bronze\nMedalist", x=0.85, y=0.15, color=klein_palette[1], fontface="bold", size=58, fontfamily = font2, lineheight=0.26) +
  draw_label(label = "2008 = The fastest Year\n Collectively for Olympic Medalists ", x=0.085, y=0.5, color=klein_palette[9], fontface="bold", size=58, fontfamily = font2, lineheight=0.26, angle = 90) +
  draw_label(label = "1916, 1940, 1944\nMissed Olympic games\n due to WWI and WWII", x=0.15, y=0.1, color=klein_palette[4], fontface="bold", size=58, fontfamily = font2, lineheight=0.26) 

ggsave("C:/Users/PC/Desktop/Portfolio/olympics/marathon.png", width = 20, height = 20) 

