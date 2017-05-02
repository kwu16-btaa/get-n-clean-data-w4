library(dplyr)
# import data
directory <- 'UCI HAR Dataset'

getdata <- function(directory, filename) {
    
    if(!file.exists(directory)) return('Invalid file directory')
    
    filename <- paste(directory, '/', filename, sep = '')
    
    if(!file.exists(filename)){
        str(filename)
        return("please check the file name")
    }
    
    read.table(filename)
}

X_train <- getdata(paste(directory, '/train', sep=''), 'X_train.txt')
y_train <- getdata(paste(directory, '/train', sep=''), 'y_train.txt')
X_test <- getdata(paste(directory, '/test', sep=''), 'X_test.txt')
y_test <- getdata(paste(directory, '/test', sep=''), 'y_test.txt')
subject_train <- getdata(paste(directory, '/train', sep=''), 'subject_train.txt')
subject_test <- getdata(paste(directory, '/test', sep=''), 'subject_test.txt')

features <- getdata(directory, 'features.txt')
activities <- getdata(directory, 'activity_labels.txt')
colnames(activities) <- c('ActivityID', 'Activity')


# rename the column names of the target variable in the data frame
colnames(y_train) <- c('ActivityID') -> colnames(y_test)

# Use descriptive activity names to replace the class label code in y
# left join y and actvities 
y_train <- left_join(y_train, activities, by="ActivityID")
y_test <- left_join(y_test, activities, by='ActivityID')

# Remove ActivityID field
y_train <- y_train['Activity']
y_test <- y_test['Activity']

# keep only mean and standard deviation measurement variables
keep_col_nums <- grep("mean|std",features[,2])
X_train <-  select(X_train ,keep_col_nums)
X_test <- select(X_test, keep_col_nums)

# change the column name from generic names V1,..., V128 to descripitive names
Keep_col_names <- grep("mean|std",features[,2], value = TRUE)

# clean up the variable names such as removing the ()
Keep_col_names <- gsub('mean\\(\\)', 'mean', Keep_col_names)
Keep_col_names <- gsub('std\\(\\)', 'std', Keep_col_names)
Keep_col_names <- gsub('meanFreq\\(\\)', 'meanFreq', Keep_col_names)
Keep_col_names <- gsub('-', '_', Keep_col_names)

colnames(X_train) <- Keep_col_names
colnames(X_test) <- Keep_col_names

# combining training and testing data
y_all <- rbind(y_train,  y_test)
X_all <- rbind(X_train,  X_test)
subject_all <- rbind(subject_train, subject_test)
colnames(subject_all) <- 'Object_ID'

# merge data frames of activity, subject and measurement variables
data_all <- cbind(y_all, subject_all)
data_all <- cbind(data_all, X_all)
write.csv(data_all, file = 'All.csv', row.names = FALSE)
write.csv(colnames(data_all), file='variablenames.csv', row.names = FALSE)
# group by activity and subject and calculate the average
data_average <- data_all %>% group_by(Activity, Object_ID) %>% summarise_each(funs(mean))
write.csv(data_average, file = 'Average.csv', row.names = FALSE)


# write to tidy data set to a file