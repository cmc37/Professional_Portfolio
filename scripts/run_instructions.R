#Data is on external drive
#check for files
list.files("/Volumes/LaCie/DropBox/Github/data/")
# Corrected file path (remove trailing quote)
file_path <- "/Volumes/LaCie/DropBox/Github/data/AHRF 2023-2024 CSV/ahrf2024_Feb2025.csv"  

# Read CSV
ahrf_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Inspect first few rows
head(ahrf_data)
str(ahrf_data)

# Corrected file path (remove trailing quote)
file_path <- "/Volumes/LaCie/DropBox/Github/data/PLACES__Local_Data_for_Better_Health,_County_Data_2024_release_20251128.csv"  

# Read CSV
placescdc_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Inspect first few rows
head(placescdc_data)
str(placescdc_data)

### END - NOW HAVE CONNECTED DATA FILES FOR CLEANING AND ANALYSIS