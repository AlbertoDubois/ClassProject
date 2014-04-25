#Read all the data needed
Xtrain = read.table("train/X_train.txt")
Xtest = read.table("test/X_test.txt")
Ytrain = read.table("train/Y_train.txt")
Ytest = read.table("test/Y_test.txt")
subjectTrain = read.table("train/subject_train.txt")
subjectTest = read.table("test/subject_test.txt")
features = read.table("features.txt")
activity = read.table("activity_labels.txt")

#Merge train and test sets
X = rbind(Xtrain,Xtest)
Y = rbind(Ytrain,Ytest)
subject = rbind(subjectTrain, subjectTest)

#Name data sets X,Y and S (using Week4 concepts)
names(subject) = "subject"

index = grep(".*mean\\(\\)|.*std\\(\\)",features[,2])
X = X[,index]
names(X) = features[index, 2]
names(X) = gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

activity[, 2] = gsub("_", "", as.character(activity[, 2]))
Y[,1] = activity[Y[,1], 2]
names(Y) = "activity"

#Merge and output first dataset
dataset1 = cbind(subject, Y, X)
write.table(dataset1, "dataset1.txt")

#Get the number of iterations needed
nact = length(activity[,1])
ncol = dim(dataset1)[2]
uniquesub = unique(subject)[,1]
nsub = length(unique(subject)[,1])
dataset2 = dataset1[1:(nsub*nact), ]

#Create the second data set with the average of each variable for each activity and each subject.
a = 1
for (i in 1:nsub) {
  for (j in 1:nact) {
    dataset2[a, 1] = uniquesub[i]
    dataset2[a, 2] = activity[j, 2]
    w = dataset1[dataset1$subject==i & dataset1$activity==activity[j, 2], ]
    dataset2[a, 3:ncol] = colMeans(w[, 3:ncol])
    a = a+1
  }
}

#Output the second dataset
write.table(dataset2, "dataset2.txt")