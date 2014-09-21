#LOAD PACKAGES
library(data.table)
library(reshape2)

#READING DATA

features <- read.table('./UCI HAR Dataset/features.txt')

y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')

y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')

#MERGING DATA

train_merged <- cbind(x_train,subject_train)
train_merged <- cbind(train_merged, y_train)

test_merged <- cbind(x_test,subject_test)
test_merged <- cbind(test_merged, y_test)

merged_data <- rbind(test_merged, train_merged)

#EXTRACTING MEAN AND STANDARD DEVIATION MEASUREMENTS
columns <- c(grep("mean\\(\\)",features[[2]]),562,563)
data_meanStd <- merged_data[,columns]

#LABEL VARIABLES/COLUMNS
column_names <- features[[2]][features[[1]] %in% columns]
colnames(data_meanStd) <- union(column_names, c('Subject','Activity'))

#NAMING ACTIVITIES
data_meanStd$Activity[data_meanStd$Activity==1] <- 'WALKING' 
data_meanStd$Activity[data_meanStd$Activity==2] <- 'WALKING_UPSTAIRS'
data_meanStd$Activity[data_meanStd$Activity==3] <- 'WALKING_DOWNSTAIRS' 
data_meanStd$Activity[data_meanStd$Activity==4] <- 'SITTING' 
data_meanStd$Activity[data_meanStd$Activity==5] <- 'STANDING' 
data_meanStd$Activity[data_meanStd$Activity==6] <- 'LAYING' 

#TIDY DATA


data_melt <- melt(data_meanStd, id=c('Subject','Activity'), measure.vars=column_names)
tidy_data   <- dcast(data_melt, Subject + Activity ~ variable, mean)
write.table(tidy_data, file = "./tidy_data.txt")