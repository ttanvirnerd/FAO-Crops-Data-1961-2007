
# import packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gridExtra)


#import data of csv files in R programming
getwd() #fuction to know the current working directory(WD)
setwd("D:\\faodata")#function to reset the current WD


#import csv files
faocrops <-read.csv("fao_data_crops.csv") 


# Data Explore

colnames(faocrops)

# Get unique country or area of the 'country_or_area' column
unique_country_or_area <- unique(faocrops$country_or_area)
# Define unwanted entries
unwanted_entries <- c("fnSeqID", "Fc", "A ", "NR", "F ", "* ")

# Keep only the entries not in the unwanted list
cleaned_country_or_area <- unique_country_or_area[!unique_country_or_area %in% unwanted_entries]

# Print the cleaned vector
head(cleaned_country_or_area)
tail(cleaned_country_or_area)





# Get unique elements of the 'element' column
unique_element <- unique(faocrops$element)
# Remove empty string entries
unique_element <- unique_element[unique_element != ""]
# Print the filtered unique elements
print(unique_element) # There are 10 unique elements in the element column




# Get unique element code of the 'element_code' column
unique_element_code <- unique(faocrops$element_code)
print(unique_element_code) # There are some unwanted elements in the unique elements code
# Define the elements to be removed
elements_to_remove <- c("Footnote", "Calculated Data", "May include official, semi-official or estimated data", 
                        "Not reported by country", "FAO Estimate", "Unofficial figure")
# Remove the specified elements
unique_element_code <- setdiff(unique_element_code, elements_to_remove)

# Print the filtered unique elements
print(unique_element_code)




# Get unique elements of the 'year' column
unique_year <- unique(faocrops$year)
# Remove NA values
unique_year <- unique_year[!is.na(unique_year)]
# Print the filtered unique elements
print(unique_year)
# Print the filtered unique elements
print(unique_year)
head(unique_year)
tail(unique_year)
# The dataset contain information of 1961-2007



# Get unique elements of the 'unit' column
unique_unit <- unique(faocrops$unit)
# Remove NA values
unique_unit <- unique_unit[unique_unit != ""]
# Print the filtered unique elements
print(unique_unit) # There are 5 unique elements in the elements column



# Get unique elements of the 'category' column
unique_category <- unique(faocrops$category) # There are 172 unique elements in the category column

head(unique_category)
tail(unique_category)


#Data analysis starts from here


# Clean the 'element' column to remove extra spaces and convert to lowercase
faocrops <- faocrops %>%
  mutate(element = str_trim(tolower(element)))


# Create separate vectors for each unique element
area_harvested_values <- faocrops %>%
  filter(element == "area harvested") %>%
  pull(value)

yield_values <- faocrops %>%
  filter(element == "yield") %>%
  pull(value)

production_quantity_values <- faocrops %>%
  filter(element == "production quantity") %>%
  pull(value)

seed_values <- faocrops %>%
  filter(element == "seed") %>%
  pull(value)

# Print the vectors
head(area_harvested_values)
head(yield_values)
head(production_quantity_values)
head(seed_values)


# creating function to get top and least 10 categories of crops according to element


summarize_by_element <- function(data, element_name) {
  data %>%
    filter(element == element_name) %>%
    group_by(category) %>%
    summarize(summarized_value = sum(value, na.rm = TRUE)) %>%
    arrange(desc(summarized_value))
}

# element area harvested
category_by_area_harvested <- summarize_by_element(faocrops, "area harvested")
# Top 10 categories according to total area harvested
print(category_by_area_harvested[1:10, ])
# Least 10 categories according to total area harvested
print(category_by_area_harvested[161:170, ])

# element yield
category_by_yield <- summarize_by_element(faocrops, "yield")
# Top 10 categories according to total yield
print(category_by_yield[1:10, ])
# Least 10 categories according to total yield
print(category_by_yield[1:10, ])


# element production quantity
category_by_production_quantity <- summarize_by_element(faocrops, "production quantity")
# Top 10 categories according to total production quantity
print(category_by_production_quantity[1:10, ])
# Least 10 categories according to total production quantity
print(category_by_production_quantity[1:10, ])

#element seed
category_by_seed <- summarize_by_element(faocrops, "seed")
# Top 10 categories according to total seed
print(category_by_production_quantity[1:10, ])
# Least 10 categories according to total seed
print(category_by_production_quantity[1:10, ])


# Display top 10 categories of crops in the world related to various elements in Bar plot
# Display least 10 categories of crops in the world related to various elements in Dot plot


# Function to plot top and least 10 categories
plot_top_least_categories <- function(data, element_name) {
  summarized_data <- summarize_by_element(data, element_name)
  
  # Plot top 10 categories
  top_10_plot <- summarized_data[1:10, ] %>%
    ggplot(aes(reorder(category, -summarized_value), summarized_value, fill = summarized_value)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#88e18b", high = "#19931c") +
    theme_bw() +
    labs(x = "Crop Category", y = "Total Yield",
         title = paste("Top 10 Crop Categories by Total", element_name, "in the World")) +
    coord_flip()
  
  # Plot least 10 categories
  least_10_plot <- summarized_data[(nrow(summarized_data)-9):nrow(summarized_data), ] %>%
    ggplot(aes(summarized_value, category)) +
    geom_point(aes(colour = category), size = 3) +
    labs(title = paste("Least 10 Crop Categories by Total", element_name, "in the World"),
         x = "Total Yield",
         y = "Crop Category") +
    theme_bw()
  
  list(top_10_plot = top_10_plot, least_10_plot = least_10_plot)
}



# Repeat for other elements
elements <- c("area harvested", "yield", "production quantity", "seed")
for (element in elements) {
  cat("\n", element, "\n")
  print(summarize_by_element(faocrops, element))
  plots <- plot_top_least_categories(faocrops, element)
  print(plots$top_10_plot)
  print(plots$least_10_plot)
}


# Country or area vs Element


# creating function to get top and least 10 country or area according to element

  # Function to summarize data by element and country_or_area, omitting zero values
  summarize_by_element <- function(data, element_name) {
    data %>%
      filter(element == element_name) %>%
      group_by(country_or_area) %>%
      summarize(summarized_value = sum(value, na.rm = TRUE)) %>%
      filter(summarized_value != 0) %>%  # Omit rows with zero values
      arrange(desc(summarized_value))
  }

# Function to summarize and print top and least 10 categories for multiple elements
summarize_and_print_by_elements <- function(data, elements) {
  for (element in elements) {
    cat("\nElement:", element, "\n")
    summarized_data <- summarize_by_element(data, element)
    
    # Print top 10 countries
    cat("Top 10 countries:\n")
    print(summarized_data[1:10, ])
    
    # Print least 10 countries
    cat("Least 10 countries:\n")
    print(summarized_data[(nrow(summarized_data)-9):nrow(summarized_data), ])
  }
}

# Example usage:
elements <- c("area harvested", "yield", "production quantity", "seed")
summarize_and_print_by_elements(faocrops, elements) 


# Display top 10 countries or area according to element area harvested in lollipop plot

# Function to create lollipop plots for top 10 and least 10 countries
create_lollipop_plots <- function(data, element_name) {
  summarized_data <- summarize_by_element(data, element_name)
  
  # Select top 10 countries
  top_10 <- summarized_data[1:10, ]
  mean_top_10 <- mean(top_10$summarized_value)
  
  # Select least 10 countries
  least_10 <- summarized_data[(nrow(summarized_data)-9):nrow(summarized_data), ]
  mean_least_10 <- mean(least_10$summarized_value)
  
  # Create top 10 lollipop plot
  top_10_plot <- ggplot(top_10, aes(x = reorder(country_or_area, summarized_value), y = summarized_value)) +
    geom_segment(aes(x = country_or_area, xend = country_or_area, y = mean_top_10, yend = summarized_value), color = "grey") +
    geom_point(aes(color = country_or_area), size = 4, show.legend = FALSE) +
    geom_hline(yintercept = mean_top_10, linetype = "dashed", color = "grey", size = 1) +
    coord_flip() +
    theme_bw() +
    labs(x = "Country or Area", y = "Total Value",
         title = paste("Top 10 Countries or Areas by Total", element_name)) +
    theme(legend.position = "none")
  
  # Create least 10 lollipop plot
  least_10_plot <- ggplot(least_10, aes(x = reorder(country_or_area, summarized_value), y = summarized_value)) +
    geom_segment(aes(x = country_or_area, xend = country_or_area, y = mean_least_10, yend = summarized_value), color = "grey") +
    geom_point(aes(color = country_or_area), size = 4, show.legend = FALSE) +
    geom_hline(yintercept = mean_least_10, linetype = "dashed", color = "grey", size = 1) +
    coord_flip() +
    theme_bw() +
    labs(x = "Country or Area", y = "Total Value",
         title = paste("Least 10 Countries or Areas by Total", element_name)) +
    theme(legend.position = "none")
  
  list(top_10_plot = top_10_plot, least_10_plot = least_10_plot)
}

# Example usage:
elements <- c("area harvested", "yield", "production quantity", "seed")
for (element in elements) {
  plots <- create_lollipop_plots(faocrops, element)
  print(plots$top_10_plot)
  print(plots$least_10_plot)
}



# Showing top 5 and least 5 element in raster plot according to country or area in Raster Plot

# Define a function to generate raster plots
generate_raster_plots <- function(data, element_names) {
  unwanted_entries <- c("fnSeqID", "Fc", "A ", "NR", "F ", "* ")
  
  for (element in element_names) {
    # Filter for the specified element and exclude unwanted entries
    filtered_data <- data %>%
      filter(element == !!element) %>%
      filter(!country_or_area %in% unwanted_entries) %>%
      group_by(country_or_area) %>%
      summarise(total_value = sum(value, na.rm = TRUE), .groups = 'drop')
    
    # Find the top 5 and least 5 countries by total value
    top_5 <- filtered_data %>% top_n(5, total_value)
    least_5 <- filtered_data %>% top_n(-5, total_value)
    
    # Create a combined dataset for plotting
    combined_data <- expand.grid(country_or_area_top = top_5$country_or_area,
                                 country_or_area_least = least_5$country_or_area)
    
    # Merge with total value
    combined_data <- combined_data %>%
      left_join(filtered_data, by = c("country_or_area_top" = "country_or_area")) %>%
      left_join(filtered_data, by = c("country_or_area_least" = "country_or_area"))
    
    # Arrange the factors for better differentiation
    combined_data$country_or_area_top <- factor(combined_data$country_or_area_top, levels = top_5$country_or_area)
    combined_data$country_or_area_least <- factor(combined_data$country_or_area_least, levels = least_5$country_or_area)
    
    # Create a raster plot
    plot <- ggplot(combined_data, aes(x = country_or_area_least, y = country_or_area_top, fill = total_value.x)) +
      geom_tile(color = "white") +  # Add white borders between tiles
      scale_fill_gradient(low = "#f0fff0", high = "#006400") +  # Adjust color based on the element
      labs(title = paste("Top 5 vs Least 5 Countries by", element),
           x = "Least 5 Countries or Areas",
           y = "Top 5 Countries or Areas",
           fill = element) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),  # Remove grid lines
            panel.grid.minor = element_blank())
    
    # Print the plot
    print(plot)
  }
}

# Define the elements to plot
elements_to_plot <- c("area harvested", "yield", "production quantity", "seed")

# Call the function with your faocrops dataset
generate_raster_plots(faocrops, elements_to_plot)

