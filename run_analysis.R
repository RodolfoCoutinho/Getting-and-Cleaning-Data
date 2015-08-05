run_analysis<-function(){

library(dplyr)

test_y<-read.table(file = "UCI HAR Dataset/test/y_test.txt")
test_x<-read.table(file = "UCI HAR Dataset/test/x_test.txt")
test_s<-read.table(file = "UCI HAR Dataset/test/subject_test.txt")
train_y<-read.table(file = "UCI HAR Dataset/train/y_train.txt")
train_x<-read.table(file = "UCI HAR Dataset/train/x_train.txt")
train_s<-read.table(file = "UCI HAR Dataset/train/subject_train.txt")
features<-read.table("UCI HAR Dataset/features.txt")
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt")

#cria o filtro para as colunas de média e desvio padrão
mean_columns<-grep("mean",features$V2)
std_columns<-grep("std",features$V2)
columns<-c(mean_columns,std_columns)

#filtras as colunas
test<-test_x[,columns]
#renomeia buscando na tabela features
column_names<-names(test)
column_names<-as.integer(sub("V","",column_names))
names(test)<-features[column_names,2]
#adiciona as colunas de activity e subject dos outros arquivos
test$Activity<-test_y
test$Subject<-test_s

#análogo
train<-train_x[,columns]
column_names<-names(train)
column_names<-as.integer(sub("V","",column_names))
names(train)<-features[column_names,2]
train$Activity<-train_y
train$Subject<-train_s

#transforma os numeros de activity em nomes da tabela
test$Activity<-inner_join(test$Activity,activity_labels)$V2
train$Activity<-inner_join(train$Activity,activity_labels)$V2

#transforma a ultima coluna em vetor
test$Subject<-test$Subject[, "V1"]
train$Subject<-train$Subject[, "V1"]

#une todos os dados numa tabela só e faz os cálculos das colunas de dados agrupados por sub e act
unified<-rbind(train,test)
final<-aggregate(unified[,1:79],by=list(unified$Activity,unified$Subject),mean)
final<-rename(final,Activity=Group.1,Subject=Group.2)

write.table(final,"dataset.txt",row.names=FALSE)

}