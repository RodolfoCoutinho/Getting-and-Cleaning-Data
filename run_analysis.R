#The objective of this function is to group the information about mean, standard deviation, activity and subject into a single tidy dataset.
run_analysis<-function(){


	#load the necessary packages
	library(dplyr)


	#read the necessary files
	test_y<-read.table(file = "UCI HAR Dataset/test/y_test.txt")
	test_x<-read.table(file = "UCI HAR Dataset/test/x_test.txt")
	test_s<-read.table(file = "UCI HAR Dataset/test/subject_test.txt")
	train_y<-read.table(file = "UCI HAR Dataset/train/y_train.txt")
	train_x<-read.table(file = "UCI HAR Dataset/train/x_train.txt")
	train_s<-read.table(file = "UCI HAR Dataset/train/subject_train.txt")
	features<-read.table("UCI HAR Dataset/features.txt")
	activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")



	#create a filter with the index of columns with the words 'mean' or 'std' in their names
	mean_columns<-grep("mean",features$V2)
	std_columns<-grep("std",features$V2)
	columns<-c(mean_columns,std_columns)

	
	
	##with test data
		#effectively filter the test_x data
		test<-test_x[,columns]
		
		#replace the V1,V2,.. column names with descriptive names according to the features file
		column_names<-names(test)
		column_names<-as.integer(sub("V","",column_names))
		names(test)<-features[column_names,2]
		
		#adds the activity and subject columns to the dataset
		test$Activity<-test_y
		test$Subject<-test_s
		
		#replace the activity code for activity labels
		test$Activity<-inner_join(test$Activity,activity_labels)$V2
	##with test data

	##with train data
		train<-train_x[,columns]
		column_names<-names(train)
		column_names<-as.integer(sub("V","",column_names))
		names(train)<-features[column_names,2]
		train$Activity<-train_y
		train$Subject<-train_s
		train$Activity<-inner_join(train$Activity,activity_labels)$V2
	##with train data
	
	
	
	#transform subject columns from lists to vectors
	test$Subject<-test$Subject[, "V1"]
	train$Subject<-train$Subject[, "V1"]

	
	#bind the rows of both datasets into a sigle one
	unified<-rbind(train,test)
	
	#calculate the mean of all columns by activity and subject
	final<-aggregate(unified[,1:79],by=list(unified$Activity,unified$Subject),mean)
	
	#set descriptive names for the first two columns
	final<-rename(final,Activity=Group.1,Subject=Group.2)

	#create a txt file with the tidy dataset
	write.table(final,"dataset.txt",row.names=FALSE)

}