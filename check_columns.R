# Load required libraries
library(readr)

# Read the data
base_data <- read.csv("base.csv", sep = ";", stringsAsFactors = FALSE)

# Print column names
print("Column names in the CSV file:")
print(colnames(base_data))

# Save the column names to a file for reference
write.table(colnames(base_data), "column_names.txt", row.names = FALSE, col.names = FALSE)
