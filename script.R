#Load the dataset
myDataSet <- read.csv("fitness_dataset.csv", stringsAsFactors = FALSE)
#check the data structure
str(myDataSet)
#check first 6 rows
head(myDataSet)
#get a summary of every column
summary(myDataSet)
