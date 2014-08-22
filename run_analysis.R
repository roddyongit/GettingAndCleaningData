run_analysis <- function() {

        #-----------------------------------------------------------------------
        # This script takes the files from the Samsung smartphones study
        # and transform them as follows:
        # 1. Combines (merges) the test and train data set into a single data set.
        # 2. Generates a single data set containing only the variables for the mean() 
        #    and std() measurements
        # 3. Generates a second tidy data set with the average of all columns (mean and std) 
        #    grouped by subject and actitivy
        #-----------------------------------------------------------------------
        
        # Load required libraries to use ddply function
        library("plyr")
        
        
        # Load text files: Test, Train, Activities and features...
        # ----------------------------------------------------------------------
        # Load results of the study (Test or Train)...
        xTrain = read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
        xTest = read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)        
        
        # Load subject: Person involved in the study.
        subjectTrain = read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
        subjectTest = read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
        
        # Load Activities: Values: 1 to 6 
        yTrain = read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)
        yTest = read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE)        
        
        # Load features description file: 561 varialbes from the study
        features = read.table("UCI HAR Dataset/features.txt", header=FALSE) 
        # ----------------------------------------------------------------------
                
        # Merge test and train study results
        mergedTestAndTrain = rbind(xTrain, xTest)
        
        # Add variable names from features.txt file to mergedTestAndTrain dataset
        colnames(mergedTestAndTrain) <- features[,2]
        
        # Merge subject from train and test files
        subjectAll = rbind(subjectTrain, subjectTest)
        
        # Merge activities from Y_train and Y_test files
        activityAll =  rbind(yTrain, yTest)       
        
        # Add colmns "subject" and "activity" to the left of mergedTestAndTrain dataset
        # and change col. names to more meaninful names: "subject" and "activity" instead of "V1"
        #------------------------------------------------------------------------------------------ 
        mergedTestAndTrain = cbind(subjectAll, activityAll, mergedTestAndTrain )
        colnames(mergedTestAndTrain)[1] <- "subject"
        colnames(mergedTestAndTrain)[2] <- "activity"
        
        # Project Requirement Part 3: Use descriptive activity names to name activities in data set
        # Replace activity numbers with descriptive activity names (based on activity_labels file)
        #------------------------------------------------------------------------------------------ 
        mergedTestAndTrain$activity[mergedTestAndTrain$activity == 1]= "WALKING"
        mergedTestAndTrain$activity[mergedTestAndTrain$activity == 2]= "WALKING_UPSTAIRS"
        mergedTestAndTrain$activity[mergedTestAndTrain$activity == 3]= "WALKING_DOWNSTAIRS"
        mergedTestAndTrain$activity[mergedTestAndTrain$activity == 4]= "SITTING"   
        mergedTestAndTrain$activity[mergedTestAndTrain$activity == 5]= "STANDING"
        mergedTestAndTrain$activity[mergedTestAndTrain$activity == 6]= "LAYING" 
        
        # Project Requirement Step 1: Mereges the training and test sets to create one data set.
        # Generates file TestAndTrainDataSet1.txt and stores it in the current working directory
        # This file will not be uploaded to Coursera, it will only exist in the current working dir.
        # ----------------------------------------------------------------------------- 
         write.table(mergedTestAndTrain, file = "TestAndTrainDataSet1.txt",      
         sep = ",", row.names=FALSE, quote=FALSE)
        # -------------------------------------------------------------------------------
        
        # Project requirment Part 2: Extracts only the measurements on the mean
        # and standard deviation for each measurement.
                
        stdAndMean = features[grepl("mean\\()",tolower(features$V2)) | grepl("std\\()",tolower(features$V2)),]
        stdAndMeanVector = as.vector(stdAndMean$V2)   
        TestAndTrainMeanAndStd =  mergedTestAndTrain[,c("subject","activity",stdAndMeanVector)]
        
        # ---------------------------------------------------------------------
        # Project Requirment Part 4: Appropriety labels the data set 
        # with descriptive variable names 
        # ---------------------------------------------------------------------
        # Remove all "-", "()"
        tidyVarNames = gsub("-","",names(TestAndTrainMeanAndStd))
        tidyVarNames = gsub("\\()","",tidyVarNames)
        
        # Convert mean and std variable names to camel case naming convention and more meaninful
        tidyVarNames = gsub("meanX","MeanXaxis",tidyVarNames)
        tidyVarNames = gsub("meanY","MeanYaxis",tidyVarNames)
        tidyVarNames = gsub("meanZ","MeanZaxis",tidyVarNames)
        tidyVarNames = gsub("stdX","StdXaxis",tidyVarNames)
        tidyVarNames = gsub("stdY","StdYaxis",tidyVarNames)
        tidyVarNames = gsub("stdZ","StdZaxis",tidyVarNames)
        tidyVarNames = gsub("mean","Mean",tidyVarNames)
        tidyVarNames = gsub("std","Std",tidyVarNames)        
        tidyVarNames = gsub("Acc","Acceleration",tidyVarNames)    
        tidyVarNames = gsub("Mag","Magnitude",tidyVarNames)      
        tidyVarNames = gsub("Gyro","Gyroscope",tidyVarNames)     
        tidyVarNames = gsub("BodyBody","Body",tidyVarNames)      
        
        # ---------------------------------------------------------------------
        # Apply new tidy column names to data set
        colnames(TestAndTrainMeanAndStd) = tidyVarNames
        
        # Part 2 -- Extract only the measurements on the mean and std for each measurement.
        # Subset of the mean and std variables only. 
        # Generates file ExtractMeanAndStdOnly.txt and stores it in the current working directory
        # This file will not be uploaded to Coursera, it will only exist in the current working dir.
        write.table(TestAndTrainMeanAndStd, file = "ExtractMeanAndStdOnly.txt",      
                    sep = ",", row.names=FALSE, quote=FALSE)
        # ----------------------------------------------------------------------------------------------
        
        # Calculate the average for all variables in the dataset by subject and activity
        AvgAllBySubjectActivity <-ddply(TestAndTrainMeanAndStd,.(subject,activity),colwise(mean))
        
        # Append suffix "_colAVG" to all variables in final tidy data set
        names(AvgAllBySubjectActivity) = paste(names(AvgAllBySubjectActivity),"_colAVG",sep="")
        colnames(AvgAllBySubjectActivity)[1] <- "subject"
        colnames(AvgAllBySubjectActivity)[2] <- "activity"
        # ---------------------------------------------------------------------------------------------
        
        # Project Requirement Step 5: Creates a second independent tidy data set with the average of 
        # each activity and each subject
        # This file will be uploaded to Coursera. This is the final data set.
        # ----------------------------------------------------------------------------------------------
        write.table(AvgAllBySubjectActivity, file = "AvgAllBySubjectActivityDataSet2.txt",      
                    sep = ",", row.names=FALSE, quote=FALSE)
        # ----------------------------------------------------------------------------------------------
        
        
}