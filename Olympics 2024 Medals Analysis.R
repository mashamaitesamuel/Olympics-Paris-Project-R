# What is the medal ditribution by country in the 2024 Olympics
# How do medals vary by country
# Which country has the highest number of gold
# Country with the most balnced medal distribution

# Load the necessary libraries
library(readxl)  # For reading Excel files
library(ggplot2) # For creating visualizations
library(dplyr)   # For data manipulation

# Read the data from the Excel file
file_path <- "~/Desktop/R Projects/Olympics Paris Project/olympics2024.xls"
olympics_data <- read_excel(file_path)

# Checking few rows of the data
head(olympics_data)

# Checking for structure of data
str(olympics_data)

# Summary of dataset
summary(olympics_data)

# DATA CLEANING
# Handling missing values 
olympics_data <- na.omit(olympics_data)  # Remove rows with any missing values

# Display column names and structure of the data
colnames(olympics_data)
str(olympics_data)

# Sort data by the total number of medals in descending order
medal_counts <- olympics_data %>%
  arrange(desc(Total))

# 1. MEDAL DISTRIBUTION BY COUNTRY IN THE 2024 OLYMPICS

# Create the bar chart
ggplot(medal_counts, aes(x = reorder(Country, Total), y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the coordinates for better readability
  labs(title = "Medal Distribution by Country in the 2024 Olympics",
       x = "Country",
       y = "Total Medals") +
  theme_minimal()

# 2. HOW MEDALS VARY BY COUNTRY 

# Load necessary library
library(dplyr)

# Summarize medal counts by type for each country

medal_distribution <- olympics_data %>%
  select(Country, Gold, Silver, Bronze) %>%  # Focus on relevant columns
  arrange(desc(Gold), desc(Silver), desc(Bronze))  # Sort by Gold first, then Silver, then Bronze


# Melt the data for easier plotting (requires 'reshape2' library)
install.packages("reshape2")
library(reshape2)

# Reshape the data for plotting
medal_distribution_long <- melt(medal_distribution, id.vars = "Country", 
                                variable.name = "Medal_Type", value.name = "Count")

# Create a stacked bar chart
ggplot(medal_distribution_long, aes(x = reorder(Country, -Count), y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for better readability
  labs(title = "Distribution of Medals by Country in the 2024 Olympics",
       x = "Country",
       y = "Number of Medals",
       fill = "Medal Type") +
  theme_minimal()

# 3. COUNTRY WITH THE HIGHEST NUMBER OF GOLD MEDALS

# Find the country with the highest number of gold medals
top_gold_country <- olympics_data %>%
  filter(Gold == max(Gold)) %>%
  select(Country, Gold)

# Display the result
top_gold_country

# Select the top 10 countries by gold medals
top_10_gold_countries <- olympics_data %>%
  arrange(desc(Gold)) %>%
  slice(1:10)  # Select the top 10 rows

# Create the bar chart
ggplot(top_10_gold_countries, aes(x = reorder(Country, Gold), y = Gold)) +
  geom_bar(stat = "identity", fill = "gold") +
  coord_flip() +  # Flip for better readability
  labs(title = "Top 10 Countries by Gold Medals in the 2024 Olympics",
       x = "Country",
       y = "Number of Gold Medals") +
  theme_minimal()

# 4. MEDAL DISTRIBUTION

install.packages('tidyr')
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
# or
library(tidyr)



# Get the top 10 countries based on gold medal counts
top_gold_countries <- olympics_data %>%
  arrange(desc(Gold)) %>%
  slice(1:5)

# Reshape the data for easier plotting

top_gold_countries_long <- data.frame(
  Country = rep(top_gold_countries$Country, each = 3),
  Medal_Type = c(rep("Gold", nrow(top_gold_countries)), 
                 rep("Silver", nrow(top_gold_countries)), 
                 rep("Bronze", nrow(top_gold_countries))),
  Count = c(top_gold_countries$Gold, top_gold_countries$Silver, top_gold_countries$Bronze)
)



