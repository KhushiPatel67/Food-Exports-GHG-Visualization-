# Required packages
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data source: https://www.ers.usda.gov/data-products/state-agricultural-trade-data/

transform_sheet <- function(sheet_data, product_name) {
  # Convert the data frame to a longer format
  transformed <- sheet_data |>
    pivot_longer(
      cols = -State,
      names_to = "Year",
      values_to = "Export_mil"
    ) |>
    mutate(
      Year = as.numeric(Year),
      Product = product_name
    ) |>
    arrange(State, Year)
  
  return(transformed)
}

# Main function to process all sheets
process_excel_sheets <- function(file_path) {

  sheets <- excel_sheets(file_path)
  transformed_sheets <- list()
  
  # Process each sheet
  for(i in seq_along(sheets)) {
    sheet_data <- read_excel(
      file_path,
      sheet = sheets[i],
      col_types = "text"
    )
    
    # Clean column names
    colnames(sheet_data) <- gsub("[^[:alnum:]]", "", colnames(sheet_data))
    
    # Convert numeric columns (years) to numeric type
    year_cols <- which(grepl("^[0-9]{4}$", colnames(sheet_data)))
    sheet_data[, year_cols] <- lapply(sheet_data[, year_cols], as.numeric)
    
    transformed <- transform_sheet(sheet_data, sheets[i])
    transformed_sheets[[i]] <- transformed
  }
  
  # Combine all transformed sheets
  final_dataset <- bind_rows(transformed_sheets)
  
  return(final_dataset)
}

final_data <- process_excel_sheets("ag-mil-(RAW_DATA).xlsx")

#####

final_data <- final_data |>
  group_by(State, Year) |>
  mutate(Export_percentage = case_when(
    Product == "Total exports" ~ 100,  # Total exports will be 100%
    TRUE ~ (Export_mil / first(Export_mil[Product == "Total exports"])) * 100
  )) |>
  ungroup()

## Plant product and animal product are the most general version 
# (always add up to 100% of exports)
# Everything else should be separated from these

plant_animal_high <- final_data[final_data$Product %in% c("Animal products", 
                                                          "Plant products"), ]
everything_else <- final_data[!final_data$Product %in% c("Animal products", 
                                                        "Plant products", 
                                                        "Total exports"), ]


total_exports <- final_data[final_data$Product %in% c("Total exports") ,]

# Tests to see if the everything_else adds up to 100% 
# across exports per state per year
the_100_test <- aggregate(Export_percentage ~ State + Year, 
                          data = everything_else, 
                          FUN = function(x) sum(x, na.rm = TRUE))

# Second round of cleaning, merging small percentages in the everything_else
# to reduce cluttering in pie charts

# Group < 5% as "Other"
# Change "Other plant products" to "Non-commodity crops" for clarity
n = 5
everything_else_other <- everything_else %>%
  group_by(State, Year) %>%
  mutate(
    Product = if_else(Export_percentage < n, "All < 5% Products", Product),
    Product = if_else(Product == "Other plant products", "Non-commodity crops", 
                      Product),
    Export_percentage = if_else(Product == "All < 5% Products",
                                sum(Export_percentage[Export_percentage < n]),
                                Export_percentage)
  ) %>%
  distinct(State, Year, Product, Export_percentage)

# Test for pie chart
cleaned_plot_data <- everything_else_other %>%
  filter(State == "Utah", Year == 2020)

cleaned_plot <- cleaned_plot_data %>%
  ggplot(aes(x = "", y = Export_percentage, fill = Product)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Utah 2020") +
  theme_minimal()

cleaned_plot

# Write each to a CSV
#write.csv(final_data, "cleaned_unseparated_data.csv", row.names = FALSE)
#write.csv(plant_animal_high, "plant_animal_high.csv", row.names = FALSE)
write.csv(everything_else, "(RAW)_everything_else.csv", row.names = FALSE)
#write.csv(everything_else_other, "everything_else_5%threshold.csv", row.names = FALSE)
#write.csv(total_exports, "total_exports.csv", row.names = FALSE)



