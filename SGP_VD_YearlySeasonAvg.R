
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Set the file path name to your computer
VegDRI_path <- "C:/Users/fafroz/Downloads/SGP_VD_Analysis_april09Aug24/Data-SGPVD_April09Aug24"
# Provide a list of all the VegDRI tif files
all_VegDRI <- list.files(VegDRI_path,
                         full.names = TRUE,
                         pattern = ".tif$")

########### fix with-files-date-format-issues##############

# Extract the date from the file names that match the expected pattern

matching_files <- grepl(".*week\\d+_(\\d{6})\\.tif$", basename(all_VegDRI))
dates <- sub(".*week\\d+_(\\d{6})\\.tif$", "\\1", basename(all_VegDRI[matching_files]))


# Convert the extracted dates to Date format (using lubridate's mdy)
dates <- lubridate::mdy(dates)

# Identify the non-matching files (optional)

non_matching_files <- all_VegDRI[!matching_files]
if (length(non_matching_files) > 0) {
  warning("The following files do not match the expected pattern and were skipped:")
  print(non_matching_files)
}

########### working with only matching files ##########

length(all_VegDRI)
length(dates)
length(non_matching_files)

# Create a data frame with filenames and corresponding dates (only matching files)

file_df <- data.frame(
  file = all_VegDRI[matching_files],
  date = dates
)


#################################

# Add a season column based on the month
file_df <- file_df %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "Winter",
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Fall"
  ))

# Extract year from date
file_df <- file_df %>%
  mutate(year = year(date))

# Group by year and season
seasonal_data <- file_df %>%
  group_by(year, season) %>%
  summarise(files = list(file), .groups = 'drop')


############################################# 


#Convert Raster Data to DataFrame *********
## Create seasonal_averages Object

library(terra)

# Initialize an empty list to store seasonal averages
seasonal_averages <- list()

### Loop through each combination of year and season

for (i in 1:nrow(seasonal_data)) {
  year <- seasonal_data$year[i]
  season <- seasonal_data$season[i]
  files <- seasonal_data$files[[i]]
  
  # Load the rasters for the given season
  rasters <- rast(files)
  
  # Calculate the mean across all rasters
  seasonal_mean <- app(rasters, mean, na.rm = TRUE)
  
  # Store the result in the list with a meaningful name
  seasonal_averages[[paste(year, season, sep = "_")]] <- seasonal_mean
}

# Convert seasonal_averages to DataFrames
# Initialize an empty list to store data frames
df_list <- list()

# Convert each raster to a data frame and add year, season columns
for (key in names(seasonal_averages)) {
  # Extract year and season from the key
  year_season <- strsplit(key, "_")[[1]]
  year <- year_season[1]
  season <- year_season[2]
  
  # Convert raster to dataframe
  raster_df <- as.data.frame(seasonal_averages[[key]], xy = TRUE)
  names(raster_df)[3] <- "value"  # Rename the third column to "value"
  
  # Add year and season columns
  raster_df <- raster_df %>%
    mutate(year = year, season = season)
  
  # Add this dataframe to the list
  df_list[[key]] <- raster_df
}



# Combine all data frames into one
seasonal_averages_df <- bind_rows(df_list)

############### Data Visualization #############

########## Spatial analysis- Plotting the Grid of Maps ############

ggplot(seasonal_averages_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_grid(season ~ year) +  # Aligns plots by season (rows) and year (columns)
  scale_fill_viridis_c() +
  coord_fixed() +  # Ensures the aspect ratio of the maps is preserved
  theme_minimal() +
  labs(
    title = "Average Seasonal VegDRI in Different Years ",
    x = "Longitude",  # Label for x-axis
    y = "Latitude"    # Label for y-axis
  ) +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 6),  # Adjust text size for coordinates
    axis.ticks = element_line(linewidth = 0.5),  # Adjust tick linewidth
    panel.spacing = unit(0.5, "lines")  # Adjust spacing between panels
  )


#################################### Code for -Trend Analysis #####################


##############  Summarize the pixel-level data to get a mean value for each year and season

seasonal_trend <- seasonal_averages_df %>%
  group_by(year, season) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

############### Now plot the summarized data with the Trend line

# Create a line plot to visualize the trend over time with all years included on the x-axis
ggplot(seasonal_trend, aes(x = as.numeric(year), y = mean_value, color = season, group = season)) +
  geom_line(linewidth = 1) +  # Use linewidth for line width
  geom_point(size = 2) +
  labs(
    title = "Seasonal Trend Analysis of VegDRI in SGP",
    x = "Year",
    y = "Mean VegDRI Value",
    color = "Season"
  ) +
  scale_x_continuous(breaks = seq(min(as.numeric(seasonal_trend$year)), max(as.numeric(seasonal_trend$year)), by = 1)) +  # Show all years
  scale_y_continuous(limits = c(min(seasonal_trend$mean_value) - 0.05, max(seasonal_trend$mean_value) + 0.05)) +  # Increase y-axis range
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


#### Histogram Plot
ggplot(seasonal_averages_df) +
  geom_histogram(aes(value), bins = 30, fill = "green", color = "black") +
  facet_grid(year ~ season) +
  theme_minimal() +
  ggtitle("Frequency of Average Seasonal VegDRI")

########################### Box-plot ################

seasonal_averages_df$season <- factor(seasonal_averages_df$season, levels = c("Winter", "Spring", "Summer", "Fall"))

###############  
ggplot(seasonal_averages_df, aes(x = season, y = value, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Seasonal VegDRI Distribution Across Years",
       x = "Season",
       y = "VegDRI Value") +
  theme_minimal()

##################### Heatmap of VegDRI Values (not completed yet)

ggplot(seasonal_trend, aes(x = year, y = season, fill = mean_value)) +
  geom_tile() +
  scale_fill_viridis_c() +  # Using a nice color scale for continuous data
  labs(title = "Heatmap of Seasonal VegDRI Trends",
       x = "Year",
       y = "Season",
       fill = "Mean VegDRI") +
  theme_minimal()

