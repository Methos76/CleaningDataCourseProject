library(dplyr)

## read provided data
activitylabels <- read.table('./UCI HAR Dataset/activity_labels.txt', col.names= c('id','name'))
features <- read.table('./UCI HAR Dataset/features.txt', col.names = c('id','name'))


xtestdata <- read.table('./UCI HAR Dataset/test/X_test.txt',col.names = features[,2] )
ytestdata <- read.table('./UCI HAR Dataset/test/y_test.txt',col.names = c('activitylabel'))

xtrainingdata <-read.table('./UCI HAR Dataset/train/X_train.txt',col.names = features[,2] )
ytrainingdata <-read.table('./UCI HAR Dataset/train/y_train.txt',col.names = c('activitylabel'))

#Combine trainingdata with label ids
labeledtrainingdata <- bind_cols(ytrainingdata,xtrainingdata)
#Combine testdata with label ids
labeledtestdata <- bind_cols(ytestdata,xtestdata)

##remove unneeded datastructures (save memory)
remove(xtestdata)
remove(ytestdata)
remove(xtrainingdata)
remove(ytrainingdata)

#combine training and test data
combineddata <- rbind(labeledtestdata,labeledtrainingdata)

##remove unneeded datastructures (save memory)
remove(labeledtestdata)
remove(labeledtrainingdata)

#filter the data for the wanted columns
#need only my activity label and all columns containing either a mean or a standard variation feature
filtereddata = combineddata[grep("activitylabel|mean|std",names(combineddata))]

#add discriptive Names for the activities
finaldata = merge(filtereddata,activitylabels, by.x='activitylabel', by.y='id')

#final clean up
remove(combineddata)
remove(filtereddata)


write.table(finaldata,'./PreparedData.txt')
