library(dplyr)

everything_else <- read.csv("(RAW)_everything_else.csv")

cleaned_categories <- everything_else %>%
  mutate(Product = case_when(
    # Meat & Animal Products
    Product %in% c("Beef", "Pork", "Hides", "Other livestock", 
                   "Dairy") ~ "Meat & Animal Products",
    
    # Poultry & Eggs
    Product %in% c("Broilers", "Other poultry") ~ "Poultry & Eggs",
    
    # Vegetables
    Product %in% c("Veggies, fresh", "Veggies, processed") ~ "Vegetables",
    
    # Fruits & Nuts
    Product %in% c("Fruits, fresh", "Fruits, processed", 
                   "Tree nuts") ~ "Fruits & Nuts",
    
    # Grains & Feed
    Product %in% c("Corn", "Wheat", "Rice", "Feeds", 
                   "Grain products") ~ "Grains & Feed",
    
    # Orange
    Product %in% c("Soybeans", "Soymeal", "Vegetable oils",
                   "Other oilseed products") ~ "Oilseeds",
    
    # Purple
    # Product %in% c("Cotton", "Tobacco", "Other plant products") 
    # ~ "Cotton, tobacco, other plant products",
    Product == "Cotton" ~ "Cotton",
    
    Product %in% c("Tobacco", "Other plant products") ~ "Non-commodity crops",
    
    # Catch any remaining categories
    TRUE ~ "Other"
  )) %>%
  # Group by State, Year, and new Product categories to sum percentages
  group_by(State, Year, Product) %>%
  summarize(
    Export_percentage = sum(Export_percentage, na.rm = TRUE),
    Export_mil = sum(Export_mil, na.rm = TRUE),
    .groups = 'drop'
  )

# To check the unique categories after cleaning
unique(cleaned_categories$Product)


# Check the number of categories now
n_distinct(cleaned_categories$Product)

cleaned_plot_data_2 <- cleaned_categories %>%
  filter(State == "Kansas", Year == 2020)

cleaned_plot <- cleaned_plot_data_2 %>%
  ggplot(aes(x = "", y = Export_percentage, fill = Product)) +
  geom_bar(stat = "identity", width = 1) +
  #coord_polar("y", start = 0) +
  labs(title = "Test Plot") +
  theme_minimal()

cleaned_plot

write.csv(cleaned_categories, "../final_data/food_exports_by_state_year_foodcat_in_mils$.csv")

# Dataset for Totals
total_percentage_per_year <- cleaned_categories %>%
  group_by(Year, Product) %>%
  summarize(
    total_export_percentage = sum(Export_percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate the percentage of each product over the total export percentage for that year
  group_by(Year) %>%
  mutate(
    product_percentage_of_total = total_export_percentage / sum(total_export_percentage) * 100
  ) %>%
  ungroup()

write.csv(total_percentage_per_year, "../final_data/total_exports.csv")

