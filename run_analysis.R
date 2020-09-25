library(dplyr)

## read provided data
activitylabels <- read.table('./UCI HAR Dataset/activity_labels.txt', col.names= c('activityid','activityname'))
features <- read.table('./UCI HAR Dataset/features.txt', col.names = c('id','name'))

subjecttest <- read.table('./UCI HAR Dataset/test/subject_test.txt', col.names = c('Subject'))
xtestdata <- read.table('./UCI HAR Dataset/test/X_test.txt',col.names = features[,2] )
ytestdata <- read.table('./UCI HAR Dataset/test/y_test.txt',col.names = c('activityid'))

subjecttraining <- read.table('./UCI HAR Dataset/train/subject_train.txt', col.names = c('Subject'))  
xtrainingdata <-read.table('./UCI HAR Dataset/train/X_train.txt',col.names = features[,2] )
ytrainingdata <-read.table('./UCI HAR Dataset/train/y_train.txt',col.names = c('activityid'))

#merge the activity label names to the activitites
ytrainingdata <-merge(ytrainingdata,activitylabels, by='activityid')
ytestdata <-merge(ytestdata,activitylabels, by='activityid')


#Combine trainingdata with label ids and subjects
fulltrainingdata <- bind_cols(subjecttraining,ytrainingdata,xtrainingdata)
#Combine testdata with label ids
fulltestdata <- bind_cols(subjecttest,ytestdata,xtestdata)


##remove unneeded datastructures (save memory)
remove(xtestdata)
remove(ytestdata)
remove(xtrainingdata)
remove(ytrainingdata)
remove(subjecttest)
remove(subjecttraining)


#combine training and test data
combineddata <- rbind(fulltestdata,fulltrainingdata)

##remove unneeded datastructures (save memory)
remove(fulltestdata)
remove(fulltrainingdata)

#filter the data for the wanted columns
#need only my activity label and all columns containing either a mean or a standard variation feature
filtereddata = combineddata[grep("activityid|activityname|Subject|mean|std",names(combineddata))]

#final clean up
remove(combineddata)

#groupeddata <- group_by(filtereddata,Subject,activityid)
groupeddata <- group_by(filtereddata,activityid,Subject)

finaldata = summarise(groupeddata, tBodyAcc.mean...X = mean(tBodyAcc.mean...X),                            
          tBodyAcc.mean...Y = mean(tBodyAcc.mean...Y),                            
          tBodyAcc.mean...Z = mean(tBodyAcc.mean...Z),                            
          tBodyAcc.std...X = mean(tBodyAcc.std...X),                              
          tBodyAcc.std...Y = mean(tBodyAcc.std...Y),                              
          tBodyAcc.std...Z = mean(tBodyAcc.std...Z),                              
          tGravityAcc.mean...X = mean(tGravityAcc.mean...X),                      
          tGravityAcc.mean...Y = mean(tGravityAcc.mean...Y),                      
          tGravityAcc.mean...Z = mean(tGravityAcc.mean...Z),                      
          tGravityAcc.std...X = mean(tGravityAcc.std...X),                        
          tGravityAcc.std...Y = mean(tGravityAcc.std...Y),                        
          tGravityAcc.std...Z = mean(tGravityAcc.std...Z),                        
          tBodyAccJerk.mean...X = mean(tBodyAccJerk.mean...X),                    
          tBodyAccJerk.mean...Y = mean(tBodyAccJerk.mean...Y),                    
          tBodyAccJerk.mean...Z = mean(tBodyAccJerk.mean...Z),                    
          tBodyAccJerk.std...X = mean(tBodyAccJerk.std...X),                      
          tBodyAccJerk.std...Y = mean(tBodyAccJerk.std...Y),                      
          tBodyAccJerk.std...Z = mean(tBodyAccJerk.std...Z),                      
          tBodyGyro.mean...X = mean(tBodyGyro.mean...X),                          
          tBodyGyro.mean...Y = mean(tBodyGyro.mean...Y),                          
          tBodyGyro.mean...Z = mean(tBodyGyro.mean...Z),                          
          tBodyGyro.std...X = mean(tBodyGyro.std...X),                            
          tBodyGyro.std...Y = mean(tBodyGyro.std...Y),                            
          tBodyGyro.std...Z = mean(tBodyGyro.std...Z),                            
          tBodyGyroJerk.mean...X = mean(tBodyGyroJerk.mean...X),                  
          tBodyGyroJerk.mean...Y = mean(tBodyGyroJerk.mean...Y),                  
          tBodyGyroJerk.mean...Z = mean(tBodyGyroJerk.mean...Z),                  
          tBodyGyroJerk.std...X = mean(tBodyGyroJerk.std...X),                    
          tBodyGyroJerk.std...Y = mean(tBodyGyroJerk.std...Y),                    
          tBodyGyroJerk.std...Z = mean(tBodyGyroJerk.std...Z),                    
          tBodyAccMag.mean.. = mean(tBodyAccMag.mean..),                          
          tBodyAccMag.std.. = mean(tBodyAccMag.std..),                            
          tGravityAccMag.mean.. = mean(tGravityAccMag.mean..),                    
          tGravityAccMag.std.. = mean(tGravityAccMag.std..),                      
          tBodyAccJerkMag.mean.. = mean(tBodyAccJerkMag.mean..),                  
          tBodyAccJerkMag.std.. = mean(tBodyAccJerkMag.std..),                    
          tBodyGyroMag.mean.. = mean(tBodyGyroMag.mean..),                        
          tBodyGyroMag.std.. = mean(tBodyGyroMag.std..),                          
          tBodyGyroJerkMag.mean.. = mean(tBodyGyroJerkMag.mean..),                
          tBodyGyroJerkMag.std.. = mean(tBodyGyroJerkMag.std..),                  
          fBodyAcc.mean...X = mean(fBodyAcc.mean...X),                            
          fBodyAcc.mean...Y = mean(fBodyAcc.mean...Y),                            
          fBodyAcc.mean...Z = mean(fBodyAcc.mean...Z),                            
          fBodyAcc.std...X = mean(fBodyAcc.std...X),                              
          fBodyAcc.std...Y = mean(fBodyAcc.std...Y),                              
          fBodyAcc.std...Z = mean(fBodyAcc.std...Z),                              
          fBodyAcc.meanFreq...X = mean(fBodyAcc.meanFreq...X),                    
          fBodyAcc.meanFreq...Y = mean(fBodyAcc.meanFreq...Y),                    
          fBodyAcc.meanFreq...Z = mean(fBodyAcc.meanFreq...Z),                    
          fBodyAccJerk.mean...X = mean(fBodyAccJerk.mean...X),                    
          fBodyAccJerk.mean...Y = mean(fBodyAccJerk.mean...Y),                    
          fBodyAccJerk.mean...Z = mean(fBodyAccJerk.mean...Z),                    
          fBodyAccJerk.std...X = mean(fBodyAccJerk.std...X),                      
          fBodyAccJerk.std...Y = mean(fBodyAccJerk.std...Y),                      
          fBodyAccJerk.std...Z = mean(fBodyAccJerk.std...Z),                      
          fBodyAccJerk.meanFreq...X = mean(fBodyAccJerk.meanFreq...X),            
          fBodyAccJerk.meanFreq...Y = mean(fBodyAccJerk.meanFreq...Y),            
          fBodyAccJerk.meanFreq...Z = mean(fBodyAccJerk.meanFreq...Z),            
          fBodyGyro.mean...X = mean(fBodyGyro.mean...X),                          
          fBodyGyro.mean...Y = mean(fBodyGyro.mean...Y),                          
          fBodyGyro.mean...Z = mean(fBodyGyro.mean...Z),                          
          fBodyGyro.std...X = mean(fBodyGyro.std...X),                            
          fBodyGyro.std...Y = mean(fBodyGyro.std...Y),                            
          fBodyGyro.std...Z = mean(fBodyGyro.std...Z),                            
          fBodyGyro.meanFreq...X = mean(fBodyGyro.meanFreq...X),                  
          fBodyGyro.meanFreq...Y = mean(fBodyGyro.meanFreq...Y),                  
          fBodyGyro.meanFreq...Z = mean(fBodyGyro.meanFreq...Z),                  
          fBodyAccMag.mean.. = mean(fBodyAccMag.mean..),                          
          fBodyAccMag.std.. = mean(fBodyAccMag.std..),                            
          fBodyAccMag.meanFreq.. = mean(fBodyAccMag.meanFreq..),                  
          fBodyBodyAccJerkMag.mean.. = mean(fBodyBodyAccJerkMag.mean..),          
          fBodyBodyAccJerkMag.std.. = mean(fBodyBodyAccJerkMag.std..),            
          fBodyBodyAccJerkMag.meanFreq.. = mean(fBodyBodyAccJerkMag.meanFreq..),  
          fBodyBodyGyroMag.mean.. = mean(fBodyBodyGyroMag.mean..),                
          fBodyBodyGyroMag.std.. = mean(fBodyBodyGyroMag.std..),                  
          fBodyBodyGyroMag.meanFreq.. = mean(fBodyBodyGyroMag.meanFreq..),        
          fBodyBodyGyroJerkMag.mean.. = mean(fBodyBodyGyroJerkMag.mean..),        
          fBodyBodyGyroJerkMag.std.. = mean(fBodyBodyGyroJerkMag.std..),          
          fBodyBodyGyroJerkMag.meanFreq.. = mean(fBodyBodyGyroJerkMag.meanFreq..))



write.table(finaldata,'./PreparedData1.txt', row.name = FALSE)
