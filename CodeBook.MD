##Tidy Data Set Codebook

###Source
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

###Background
One of the most exciting areas in all of data science right now is wearable computing - see for example this article. 
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
The data linked to from the course website represent data collected from the accelerometers from the Samsung 
Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

###Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 
we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly 
partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and 
then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). 
The sensor acceleration signal, which has gravitational and body motion components, was separated using a 
Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only 
low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of 
features was obtained by calculating variables from the time and frequency domain. 

###Attribute Information
For each record in the dataset it is provided: 
* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

###Modifications
Using the original data set provided, some cleaning and modification was done to create a much tidier data set.
The run_analysis.R code was written to do the task and performs as follows.
It takes the given train and test data sets and does the following:

* Merge the train and test data sets.
* Extract only the measurements on mean and standard deviation.
* Use descriptive activity names to name the activities in the data set.
* Appropriately label the data set with descriptive variable names.
* Creates a new, independent data set with the average for each variable for each activity and each subject.

It finally writes out the output to a .txt file.

###Final Data Set
The final data set after all of the modifications goes in order of the subject and lists the average mean and
standard deviation measurements for each activity, which are also ordered.
