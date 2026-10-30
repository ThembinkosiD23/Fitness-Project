#Load the dataset
myDataSet <- read.csv("fitness_dataset.csv", stringsAsFactors = FALSE)
#check the data structure
str(myDataSet)
#check first 6 rows
head(myDataSet)
#get a summary of every column
summary(myDataSet)

#CLEANING
#1 Fix the 'smokes' column ----
myDataSet$smokes <- tolower(as.character(myDataSet$smokes))                   # Convert everything to lowercase
myDataSet$smokes[myDataSet$smokes %in% c("yes", "1")] <- 1                    # Covert yes and 1 to int 1
myDataSet$smokes[myDataSet$smokes %in% c("no", "0")] <- 0                     # Covert no and 0 to int 0
myDataSet$smokes <- as.numeric(myDataSet$smokes)

# Verify it worked
table(myDataSet$smokes)

#2 Handle missing values in 'sleep_hours
median_sleep <- median(myDataSet$sleep_hours, na.rm = TRUE)
myDataSet$sleep_hours[is.na(myDataSet$sleep_hours)] <- median_sleep            # Impute with the median

sum(is.na(myDataSet$sleep_hours))                                              # check for any NA

#3Cap outliers in 'weight_kg'
lower_cap <- quantile(myDataSet$weight_kg, 0.01, na.rm = TRUE)                 # Find the 1st and 99th percentiles
upper_cap <- quantile(myDataSet$weight_kg, 0.99, na.rm = TRUE)
myDataSet$weight_kg[myDataSet$weight_kg < lower_cap] <- lower_cap              # Replace values below/above with the cap
myDataSet$weight_kg[myDataSet$weight_kg > upper_cap] <- upper_cap
summary(myDataSet$weight_kg)