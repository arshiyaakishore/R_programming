# i) Create two files with the name "Sales.txt" and "Region.txt"
data <- data.frame(
  Region = c("East", "West", "East", "West", "East", "West"),
  Product = c("Paper", "Paper", "Pens", "Pens", "Paper", "Paper"),
  Qty = c(73, 33, 14, 40, 21, 10),
  Cost = c(12.95, 12.95, 2.19, 2.19, 12.95, 12.95),
  Amt = c(945.35, 427.35, 30.66, 87.60, 271.95, 129.50),
  Tax = c(66.17, 29.91, 2.15, 6.13, 19.04, 9.07)
)

write.table(data[, c("Product", "Qty", "Cost", "Amt", "Tax")], file = "Sales.txt", row.names = FALSE, col.names = TRUE, sep = "\t")
write.table(data[, c("Product", "Region")], file = "Region.txt", row.names = FALSE, col.names = TRUE, sep = "\t")

# ii) Using "Sales.txt" compute covariance between Cost and Tax using pearson method.
sales_data <- read.table("Sales.txt", header = TRUE, sep = "\t")
covariance <- cov(sales_data$Cost, sales_data$Tax, method = "pearson")
cat("Covariance between Cost and Tax:", covariance, "\n")

# iii) Use "Sales.txt" and "Region.txt" files to display total tax in the East region for Paper.
region_data <- read.table("Region.txt", header = TRUE, sep = "\t")
merged_data <- merge(sales_data, region_data, by = "Product")
total_tax_east_paper <- sum(merged_data$Tax[merged_data$Region == "East" & merged_data$Product == "Paper"])
cat("Total Tax in the East region for Paper:", total_tax_east_paper, "\n")

# iv) Use "Sales.txt" and "Region.txt" files to display region name where the maximum number of pens sale out.
max_pens_region <- subset(merged_data, Product == "Pens" & Qty == max(Qty))$Region
cat("Region with the maximum number of Pens sales:", max_pens_region, "\n")

# v) Write R functions to rename "Sales.txt" as "Sales22.txt" and "Region.txt" as "Region22.txt" 
# and to check if "Sales.txt" and "Region.txt" files still exist or not.
rename_files <- function(old_filename, new_filename) {
  file.rename(old_filename, new_filename)
}

check_file_exists <- function(filename) {
  if (file.exists(filename)) {
    cat(paste(filename, "exists.\n"))
  } else {
    cat(paste(filename, "does not exist.\n"))
  }
}

# Rename files
rename_files("Sales.txt", "Sales22.txt")
rename_files("Region.txt", "Region22.txt")

# Check if files exist
check_file_exists("Sales.txt")
check_file_exists("Sales22.txt")
check_file_exists("Region.txt")
check_file_exists("Region22.txt")
