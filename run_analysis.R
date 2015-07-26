
## 1.Merges the training and the test sets to create one data set.

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset/train")
trainx <- read.table("X_train.txt")

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset/test")
testx <- read.table("X_test.txt")

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset")
features <- read.table("features.txt")

x <- rbind(trainx, testx)
colnames(x) <- features[,2]


## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

tolimit <- grep("[mM]ean|[sS]td", names(x))
limited <- x[, tolimit]

## 3.Uses descriptive activity names to name the activities in the data set

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset/train")
trainy <- read.table("Y_train.txt")

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset/test")
testy <- read.table("Y_test.txt")

y <- rbind(trainy, testy)[,1]

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset/train")
trainsubject <- read.table("subject_train.txt")

setwd(dir = "/Users/julienderville/Documents/Coursera/UCI HAR Dataset/test")
testsubject <- read.table("subject_test.txt")

subject <- rbind(trainsubject, testsubject)

activities <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activities[y]
tidy <- cbind(Subject = subject, Activity = activities, limited)


## 4.Appropriately labels the data set with descriptive variable names. 
## t = time and f = frequency
colnames(limited) <- sub("^f", "Frequency", colnames(limited))
colnames(limited) <- sub("^t", "Time", colnames(limited))

## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidydata <- ddply(tidy, .(V1, Activity), limitedColMeans)
names(tidydata)[-c(1,2)] <- paste0("Mean", names(tidydata)[-c(1,2)])

colnames(tidydata) <- sub("f", "Frequency", colnames(tidydata))
colnames(tidydata) <- sub("t", "Time", colnames(tidydata))
