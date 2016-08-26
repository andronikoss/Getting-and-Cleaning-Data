# ---- Peer Graded Assignment (Getting and Cleaning Data) ----
# Written by Andranik Stepanyan
# Fri Aug 26 21:35:22 2016


# Loading packages
library(reshape2)

# Cleaning the memory cache
rm(list=ls())

# Set the working Directory (*Change/*Review this line if necessary)
setwd("D:/Users/s4anstep/Dropbox/Coursera/Getting and Cleaning Data/Data")

# Read the 'test sets', i.e. X_test.txt file from the order 'test'
data1 <- read.table("./test/X_test.txt", colClasses = "numeric")
# Subjects of the test group
sbj1 <- read.table("./test/subject_test.txt", colClasses = "numeric")
# Activities the test group
act1 <- read.table("./test/y_test.txt", colClasses = "numeric")

table(sbj1);table(act1)
# Read the 'training sets', i.e. X_train.txt file from the order 'train'
data2 <- read.table("./train/X_train.txt", colClasses = "numeric")
sbj2 <- read.table("./train/subject_train.txt", colClasses = "numeric")
act2 <- read.table("./train/y_train.txt", colClasses = "numeric")
table(sbj2);table(act2)

# Completing/Adding the variables to each data set.

data1$subject <- sbj1$V1
data2$subject <- sbj2$V1

data1$activity <- act1$V1
data2$activity <- act2$V1

# ---- Task 1: Merging Data into one common Big Data ----
df <- rbind(data1, data2)

# ---- Task 2: Extracts only the measurements on the mean and standard ----
# deviation for each measurement. Get only the data on mean and std. dev.
Name.of.Features <- read.table("./features.txt", stringsAsFactors = F)
str(Name.of.Features)

var <- grep(".*[Mm]ean\\(|.*[Ss]td\\(", Name.of.Features[,2], value=T)

# ---- Task 3: Uses descriptive activity names to name the activities in the data set ----

activities <- read.table("activity_labels.txt", stringsAsFactors = F)
# Create a factor variable and change its levels to names of activities.

f <- as.factor(df$activity)
levels(f)  <- activities[,2]
df$activity <- f

# ---- Task 4: Appropriately labels the data set with descriptive variable names ----

# Create a list of used functions (see features_info.txt)
list.of.func <- c("mean()",  #: Mean value
"std()",   #: Standard deviation
"mad()",   #: Median absolute deviation 
"max()",   #: Largest value in array
"min()",   #: Smallest value in array
"sma()",   #: Signal magnitude area
"energy()",  #: Energy measure. Sum of the squares divided by the number of values. 
"iqr()",         #: Interquartile range 
"entropy()",     #: Signal entropy
"arCoeff()",     #: Autorregresion coefficients with Burg order equal to 4
"correlation()", #: correlation coefficient between two signals
"maxInds()",     #: index of the frequency component with largest magnitude
"meanFreq()",    #: Weighted average of the frequency components to obtain a mean frequency
"skewness()",    #: skewness of the frequency domain signal 
"kurtosis()",    #: kurtosis of the frequency domain signal 
"bandsEnergy()", #: Energy of a frequency interval within the 64 bins of the FFT of each window.
"angle()")       #: Angle between to vectors.

tidy.names <-gsub("\\(\\)", "", list.of.func) 
tidy.names <- paste0(toupper(substring(tidy.names, 1, 1)),substring(tidy.names, 2))
tidy.names <- paste0(tidy.names, ".of.")

N.short <- Name.of.Features[,2]
length(unique(N.short)) # 477 different categories
for(i in 1:length(list.of.func)){
      
      ix <- grep(list.of.func[i], N.short)
      N.short[ix] <- paste0(tidy.names[i],sapply(strsplit(N.short[ix], list.of.func[i]),
                                              function(x) if (length(x)!=1) {paste0(x[1], x[2])}else {x}))
}
N.short <- gsub("-\\(\\)-|-\\(\\)|\\(|\\)", "", N.short)
length(unique(N.short)) # 477 different categories (Well Done!)
names(df)[1:561] <- N.short


# ---- Task 5: From the data set in step 4, creates a second, independent  ----
# tidy data set with the average of each variable for each activity and each subject.

df.melted <- melt(df, id = c("subject", "activity"))
df.melted <- dcast(df.melted, subject + activity ~ variable, mean)

dim(df.melted) # 180 479 (with activity and subject)
View(df.melted)






