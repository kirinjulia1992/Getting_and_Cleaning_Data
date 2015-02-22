test_data <- read.table("~/Desktop/MOOC/getting and cleaning data/UCI HAR Dataset/test/X_test.txt", quote="\"")
training_data <- read.table("~/Desktop/MOOC/getting and cleaning data/UCI HAR Dataset/train/X_train.txt", quote="\"")
test_label <- read.table("~/Desktop/MOOC/getting and cleaning data/UCI HAR Dataset/test/y_test.txt", quote="\"")
training_label <- read.table("~/Desktop/MOOC/getting and cleaning data/UCI HAR Dataset/train/y_train.txt", quote="\"")
features<-read.table("~/Desktop/MOOC/getting and cleaning data/UCI HAR Dataset/features.txt",quote="\"")



#List 1 is the converted label list for test_label,list 2 is the converted label list for training_label

#test1<-cbind(list1,test_data)
#test2<-cbind(list2,training_data)
test1<-as.matrix(test_data)
test2<-as.matrix(training_data)
rownames(test1)<-NULL
rownames(test2)<-NULL
test1<-t(test1)
test2<-t(test2)
test<-cbind(test1,test2)
rownames(test)<-NULL

#Withouth the feature list, just the raw data and label

#temp1<-features$V2
#temp1<-as.character(temp1)
#temp2<-c('label',temp1)
#test<-data.frame(temp1,test)
#colnames(test)<-c('features',1:10299)

test2<-features$V2
mean<-regexpr('mean',test2,fixed=TRUE)
mean[mean>= 1]=1
mean[mean== -1]=0

subdata1<-NULL
for (i in 1:561)
 {
  if (mean[i] == 1)
    {
    label<-test[i,]
    subdata1<-rbind(subdata1,label)
  }
}
rownames(subdata1)<-NULL

std<-regexpr('std',test2,fixed=TRUE)
std[std>= 1]=1
std[std== -1]=0

label<-NULL
subdata2<-NULL
for (i in 1:561)
{
  if (std[i] == 1)
  {
    label<-test[i,]
    subdata2<-rbind(subdata2,label)
  }
}
rownames(subdata2)<-NULL

# Here we extract only mean and std, step 2
test<-unlist(test_label)
train<-unlist(training_label)
list<-c(test,train)

id6<-NULL
for (i in 1:length(list))
{
  if (list[i] == 6)
  {
    id6<-cbind(id6,i)
  }
}
id6<-as.numeric(id6)
#6 means laying
id5<-NULL
for (i in 1:length(list))
{
  if (list[i] == 5)
  {
    id5<-cbind(id5,i)
  }
}
id5<-as.numeric(id5)
#id == 5 means standing
id4<-NULL
for (i in 1:length(list))
{
  if (list[i] == 4)
  {
    id4<-cbind(id4,i)
  }
}
id4<-as.numeric(id4)
#4 means sitting
id3<-NULL
for (i in 1:length(list))
{
  if (list[i] == 3)
  {
    id3<-cbind(id3,i)
  }
}
id3<-as.numeric(id3)
#3 means walking downstairs
id2<-NULL
for (i in 1:length(list))
{
  if (list[i] == 2)
  {
    id2<-cbind(id2,i)
  }
}
id2<-as.numeric(id2)
#2 means walking upstairs

id1<-NULL
for (i in 1:length(list))
{
  if (list[i] == 1)
  {
    id1<-cbind(id1,i)
  }
}
#1 means walking

length1<-length(subdata1[,1])
length2<-length(subdata2[,1])
id<-length(subdata1)

laying<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id6]
  meanval<-mean(sub)
  laying<-cbind(laying,meanval)
}
laying<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id6]
  meanval<-mean(sub)
  laying<-cbind(laying,meanval)
}


laying<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id6]
  meanval<-mean(sub)
  laying<-cbind(laying,meanval)
}
laying<-as.numeric(laying)
#5,standing
standing<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id5]
  meanval<-mean(sub)
  standing<-cbind(standing,meanval)
}
standing<-as.numeric(standing)
#4,sitting
sitting<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id4]
  meanval<-mean(sub)
  sitting<-cbind(sitting,meanval)
}
sitting<-as.numeric(sitting)
#3,walking down
walking_down<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id3]
  meanval<-mean(sub)
  walking_down<-cbind(walking_down,meanval)
}
walking_down<-as.numeric(walking_down)
#2,walking upstairs
walking_up<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id2]
  meanval<-mean(sub)
  walking_up<-cbind(walking_up,meanval)
}
walking_up<-as.numeric(walking_up)

#1,walking
walking<-NULL
for (i in 1:length1)
{
  test1<-subdata1[i,]
  sub<-test1[id2]
  meanval<-mean(sub)
  walking<-cbind(walking,meanval)
}
walking<-as.numeric(walking)

test<-data.frame(laying,standing,sitting,walking_down,walking_up,walking)


test0<-as.character(features$V2)
sublist1<-NULL
for (i in 1:561)
{
  if (mean[i] == 1)
  {
    label<-test0[i]
    sublist1<-rbind(sublist1,label)
  }
}
rownames(sublist1)<-NULL

mean_sub<-data.frame(sublist1,test)
write.table(mean_sub,file="~/Getting_and_Cleaning_Data/mean_sub.txt",row.name=FALSE)
write.csv(mean_sub,file="~/Getting_and_Cleaning_Data/mean_sub.csv")
laying<-NULL
for (i in 1:length2)
{
  test1<-subdata2[i,]
  sub<-test1[id6]
  meanval<-mean(sub)
  laying<-cbind(laying,meanval)
}
laying<-as.numeric(laying)
#5,standing
standing<-NULL
for (i in 1:length2)
{
  test1<-subdata2[i,]
  sub<-test1[id5]
  meanval<-mean(sub)
  standing<-cbind(standing,meanval)
}
standing<-as.numeric(standing)
#4,sitting
sitting<-NULL
for (i in 1:length2)
{
  test1<-subdata2[i,]
  sub<-test1[id4]
  meanval<-mean(sub)
  sitting<-cbind(sitting,meanval)
}
sitting<-as.numeric(sitting)
#3,walking down
walking_down<-NULL
for (i in 1:length2)
{
  test1<-subdata2[i,]
  sub<-test1[id3]
  meanval<-mean(sub)
  walking_down<-cbind(walking_down,meanval)
}
walking_down<-as.numeric(walking_down)
#2,walking upstairs
walking_up<-NULL
for (i in 1:length2)
{
  test1<-subdata2[i,]
  sub<-test1[id2]
  meanval<-mean(sub)
  walking_up<-cbind(walking_up,meanval)
}
walking_up<-as.numeric(walking_up)

#1,walking
walking<-NULL
for (i in 1:length2)
{
  test1<-subdata2[i,]
  sub<-test1[id2]
  meanval<-mean(sub)
  walking<-cbind(walking,meanval)
}
walking<-as.numeric(walking)

test3<-data.frame(laying,standing,sitting,walking_down,walking_up,walking)


test0<-as.character(features$V2)
sublist2<-NULL
for (i in 1:561)
{
  if (std[i] == 1)
  {
    label<-test0[i]
    sublist2<-rbind(sublist2,label)
  }
}
rownames(sublist2)<-NULL

std_sub<-data.frame(sublist2,test3)
write.table(std_sub,file="~/Getting_and_Cleaning_Data/std_sub.txt",row.name=FALSE)
write.csv(std_sub,file="~/Getting_and_Cleaning_Data/std_sub.csv")
