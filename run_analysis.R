
##Reads required data to R
setwd("UCI HAR Dataset-2")
files <- c("features.txt", "activity_labels.txt", "test/subject_test.txt",
           "test/y_test.txt", "test/X_test.txt", "train/subject_train.txt",
           "train/y_train.txt", "train/X_train.txt")

#declares tables
measureNames <- read.table(files[1])
activityNames <- read.table(files[2])
subjectIDtest <- read.table(files[3])
activityTest <- read.table(files[4])
measurementTest <- read.table(files[5])
subjectIDtrain <- read.table(files[6])
activityTrain <- read.table(files[7])
measurementTrain <- read.table(files[8])

#creates variable name vector
varNames <- c("SubjectID", "Activity", as.character(measureNames[,2]))

#adds ID and activity to meansurements
test <- cbind(subjectIDtest, activityTest, measurementTest)
train <- cbind(subjectIDtrain, activityTrain, measurementTrain)

#unites test and train sets
data <- rbind(test, train)

#adds variable names
names(data) <- varNames

#subsets to identification variables and measurement variables 
stdMean <- data[,grepl("mean()", colnames(data), fixed = TRUE) | 
                 grepl("std()", colnames(data), fixed = TRUE)]
fullSet <- cbind(data[,1:2], stdMean)


#recasts activity values as words
 
activeVec <- c("walkFlat", "walkUp", "walkDn", "sit", "stand", "lie")

fullSet$Activity <- activeVec[fullSet$Activity]

#Function for do enabled pipeline to generate averages and rename variables 
 
summar <- function(df){
  newDf <- colMeans(df[,c(3:ncol(df))])
  dfOut <- data.frame(as.list(newDf))
  names(dfOut) <- paste(names(dfOut), "variableAverage", sep="-")

  return(dfOut)
}

#Pipeline to generate grouped averages

finalSet <- fullSet %>% group_by(SubjectID, Activity) %>% do(summar(.))

#writes final data

write.table(finalSet, file = "tidyData.txt", row.names = FALSE)






