##Project for Getting and Tidying Data 

##load necessary libraries
library(dplyr)

filename <- "Accelerometers.zip"

##check if file and folder exists
if(!file.exists(filename)) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,filename, method = "curl")
}
if(!file.exists("UCI HAR Dataset")) {
    unzip(filename)
}

##read activity and feature labels
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("ID","Activity"))
features <- read.table("UCI HAR Dataset/features.txt")

##read in subject files 
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names = "Subject")
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")

##read in values corresponding to activities
yTest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "ID")
yTrain <- read.table("UCI HAR Dataset/train/y_train.txt",col.names = "ID")

##read features data 
xTest <- read.table("UCI HAR Dataset/test/x_test.txt", col.names = features$V2)
xTrain <- read.table("UCI HAR Dataset/train/x_train.txt", col.names = features$V2)


##Step 1: merge data to create one dataset

x <- rbind(xTest,xTrain)
##set name for x
names(x) <- features$V2
y <- rbind(yTest,yTrain)
subject <- rbind(subjectTest,subjectTrain)
##merge all data
data <- cbind(subject,x,y)


##Step 2: extracts mean and standard deviation for each measurement

clean_data <- data %>% select(Subject,ID,contains("mean"),contains("std"))


##Step 3: Use descriptive activity names to name activities 

clean_data$ID <- factor(clean_data$ID, labels = activities$Activity)


##Step 4: Label the data set with descriptive variable names 

##replace prefix t with time
names(clean_data) <- gsub("^t","Time",names(clean_data))
##replace acc with accelerometer 
names(clean_data) <- gsub("acc","Accelerometer",names(clean_data),ignore.case = TRUE)
##replace gyro with gyroscope
names(clean_data) <- gsub("gyro","Gyroscope",names(clean_data),ignore.case = TRUE)
##replace prefix f with frequency
names(clean_data) <- gsub("^f","Frequency",names(clean_data))
##replace mag with magnitude
names(clean_data) <- gsub("mag","Magnitude",names(clean_data),ignore.case = TRUE)
##replace bodybody with body 
names(clean_data) <- gsub("bodybody","Body",names(clean_data),ignore.case = TRUE)
##replace -mean with mean
names(clean_data) <- gsub("-mean","Mean",names(clean_data),ignore.case = TRUE)
##replace -std with std
names(clean_data) <- gsub("-std","STD",names(clean_data),ignore.case = TRUE)

#rename ID column to Activity 
clean_data <- rename(clean_data, Activity = ID)


##Step 5: create independent data set with average of each variable

TidyData <- clean_data %>% 
            group_by(Subject,Activity) %>%
            summarise_all(funs(mean))

write.table(TidyData,"TidyData.txt",row.names = FALSE)