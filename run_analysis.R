library(dplyr)
######
# Global Variables

## ROOT directory of unzipped data files
ROOTDIR='data/UCI HAR Dataset'
## LIST of files to merge
FILES<-c()

## Dataframe of features present in file
# $original: Columns present in original file
# $final: columns to retain in generated files 
FEATURES<-data.frame(original=c(), final=c())

## Where we put the resulting files
# Here, in DESTDIR directory
DESTDIR <- '.'

#########################################
### Functions to download and unzip datas
downloadAndUnzip<-function()
{
  if( !dir.exists('data')) 
    dir.create('data', showWarnings = F)
  if( !file.exists('data/UCI-HAR-Dataset.zip')) 
    download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', 
                  destfile='data/UCI-HAR-Dataset.zip')   
  unzip('data/UCI-HAR-Dataset.zip', exdir='data' , overwrite= F)
}



################################
## 
# Extracts the features from the features.txt file present in the ZIP file
#
# Returned Values: a list
# $original  : complete column list
# $final  : only 'mean' and 'std' columns 
readFeatures <-function(ROOTDIR)
{
  features <-read.csv(paste(ROOTDIR, 'features.txt', sep='/'), stringsAsFactors=F, sep = ' ', header = F)
  # Featturename shall contain 'mean' or 'std' as requested by assignment
  subFeat <-subset(features$V2, grepl(features$V2, pattern = ".*-(std|mean).*"))
  list(original = features$V2, final = subFeat)
}



#########################################
#Loading relevant files
loadFiles<-function(ROOTDIR){
  XTrain<-read.table(paste0(ROOTDIR,'/train/X_train.txt'), header = F)
  XTest<-read.table(paste0(ROOTDIR,'/test/X_test.txt'), header = F)
  YTrain<-read.table(paste0(ROOTDIR,'/train/y_train.txt'), header = F)
  YTest<-read.table(paste0(ROOTDIR,'/test/y_test.txt'), header = F)
  STrain<-read.table(paste0(ROOTDIR,'/train/subject_train.txt'), header = F)
  STest<-read.table(paste0(ROOTDIR,'/test/subject_test.txt'), header = F)
  
  datas<-list(XTrain=XTrain, YTrain=YTrain, XTest=XTest, YTest=YTest, STrain=STrain, STest=STest)
}

#Loading files
#data<-loadFiles(ROOTDIR)

#Reading features
#FEATURES<-readFeatures(ROOTDIR)

#########################################
# Merge the X,Y,Subject datasets into one
#Then we trim all the extra columns
mergeAndTrimDatasets<-function(data, FEATURES) {
## Add column names to datasets
names(data[['STrain']])='Subject'
names(data[['STest']])='Subject'
names(data[['YTrain']])='Activity'
names(data[['YTest']])='Activity'
names(data[['XTrain']])=FEATURES$original
names(data[['XTest']])=FEATURES$original

#Trim unnecessary variables from X dataset
data[['XTrain']]<-data[['XTrain']][,FEATURES$final]
data[['XTest']]<-data[['XTest']][,FEATURES$final]

data<-rbind(cbind(data[['XTrain']],data[['STrain']],data[['YTrain']]),
            cbind(data[['XTest']],data[['STest']],data[['YTest']]))
}


##################################################################################
# This function read the activities label from 'activity_labels.txt' 
# located in ROOTDIR directory
# and use these abel to transform the 'activity' column into a factor
factoriseActivity<-function(data, ROOTDIR)
{
  readActivity<-read.table(paste0(ROOTDIR,'/activity_labels.txt'), header = F)
  data$Activity<-factor(data$Activity, levels=readActivity$V1, labels = readActivity$V2)
  data$Subject<-as.factor(data$Subject)
  data  
}


#########################################
# Write the 2 final datasets
# -The trimmed dataset, as trimmed.csv
# -The averaged dataset as averaged.csv
writeResults<-function(data)
{
#Write tidy dataset
write.csv(data, 'trimmed.csv', row.names = F)

#Computing average of each variable by activity and subject
data2<- data %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

#Writing extra dataset
write.csv(data2, 'averaged.csv', row.names = F)
}


####################################
## EXECUTABLE PART

#Loading and unzipping datas
downloadAndUnzip()

#Then loading the 6 relevant files

data<-loadFiles(ROOTDIR)

# Then reading features file to extract column names
FEATURES<-readFeatures(ROOTDIR)

# Then we bind the different data files, name columns, 
# and trim extra columns into 1 "tidy" dataset 
data<-mergeAndTrimDatasets(data, FEATURES)

#Finally, we add the explicit label to the activity variable
data<-factoriseActivity(data, ROOTDIR)

names(data)
# Finally, write the 2 required datasets
writeResults(data)
  