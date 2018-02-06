#Getting and Cleaning Data Course Project


#1. MERGE the train and the test sets to create one data set. ----

    #Set the working directory
    setwd("C:/Users/karlman/Desktop/DOST PCIEERD/PCIEERDdatascience")

    #Create a directory if it does not exist
    if(!file.exists("data")){
        dir.create("data")
      } 

    #Download the file
    download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "./data/getdata_projectfiles_UCI HAR Dataset.zip")
    
    #Unzip the downloaded file
    unzip("./data/getdata_projectfiles_UCI HAR Dataset.zip")
    
    #Read the dataset into R
    #For the train data    
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
    X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
    data_train <- cbind(subject_train,y_train,X_train)
    #For the test data
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
    X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
    data_test <- cbind(subject_test,y_test,X_test)
    
    #Merge train and test sets 
    data_merged <- rbind(data_train, data_test)
    
    #Assign column names to the merged dataset
    ftrs <- as.character(read.csv("./UCI HAR Dataset/features.txt", header=F, sep = '')[,2])
    colnames(data_merged) <- c("SUBJECT","ACTIVITY",ftrs)
  
    
#2. EXTRACT only the measurements on the mean and standard deviation for each measurement. ----
    
    #Find the columns that match
    mean_std <- grep(("mean|std"), colnames(data_merged))
    
    #Extract using the previously matched indices
    data_mean_std <- data_merged[, mean_std]
    data_extracted <- cbind(data_merged[1:2],data_mean_std)
   
#3. USE descriptive activity names to name the activities in the data set. ----
    
    #Read the labels into R
    activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt')
    activity_labels
    
    #Name the activities
    data_extracted$ACTIVITY <- activity_labels[,2][data_extracted$ACTIVITY]
    
    
#4. LABEL appropriately the data set with descriptive variable names. ----
    
    #View the default names
    names(data_extracted)
    #Modify the names by fixing the characters    
    names(data_extracted) <- gsub("-", " ", names(data_extracted))
    names(data_extracted) <- gsub("^t", "TIME ", names(data_extracted))
    names(data_extracted) <- gsub("^f", "FREQUENCY ", names(data_extracted))
    names(data_extracted) <- gsub("BodyAcc", "Body_Linear_Accelerration", names(data_extracted))
    names(data_extracted) <- gsub("mean[(][)]", "Mean", names(data_extracted))
    names(data_extracted) <- gsub("std[(][)]", "Standard_Deviation", names(data_extracted))
    names(data_extracted) <- gsub("GravityAcc", "Gravity_Acceleration", names(data_extracted))
    names(data_extracted) <- gsub("Jerk", " Jerk", names(data_extracted))
    names(data_extracted) <- gsub("BodyGyro", "Body_Angular_Velocity", names(data_extracted))
    names(data_extracted) <- gsub("Mag", " Magnitude", names(data_extracted))
    names(data_extracted) <- gsub("meanFreq[(][)]", "Mean_Frequency", names(data_extracted))
    #View modified names
    names(data_extracted)
    
    
#5. From the data set in step 4, CREATE a second, independent tidy data set with the average of each variable for each activity and each subject. ----
    
    #Load the needed package
    library(dplyr)
    #Create a second data set: (1)grouping by subject and activity and (2)getting the mean
    data_tidy <- data_extracted %>% group_by(SUBJECT, ACTIVITY) %>% summarise_all(funs(mean))
    #Create a text file to save the tidy data set
    write.table(x = data_tidy, file = "data_tidy.txt", row.names = F)
    