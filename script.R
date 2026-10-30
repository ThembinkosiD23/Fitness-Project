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