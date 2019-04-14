#0=campur,1=putra,2=putri
train<-read.csv("train.csv")
train<-train[,-1]
test<-read.csv("test_data.csv")
test<-test[,-1]

colnms=c("fac_1", "fac_2", "fac_3", "fac_4", "fac_5","fac_6","fac_7","fac_8")
train$new_col<-rowSums(train[,colnms])
test$new_col<-rowSums(test[,colnms])

#binning
train$price_bin <- NA
train$price_bin[train$price_monthly <=750000] <-1
train$price_bin[train$price_monthly >750000 & train$price_monthly <=900000] <-2
train$price_bin[train$price_monthly >900000] <-3


test$price_bin <- NA
test$price_bin[test$price_monthly <=750000] <-1
test$price_bin[test$price_monthly >750000 & test$price_monthly <=900000] <-2
test$price_bin[test$price_monthly >900000] <-3
#size binning 30

train$size_bin<-NA
test$size_bin<-NA

test$size_bin[test$size<=13]<-1
test$size_bin[test$size>13 &test$size<=30]<-2
test$size_bin[test$size>30]<-3

train$size_bin[train$size<=13]<-1
train$size_bin[train$size>13 &train$size<=30]<-2
train$size_bin[train$size>30]<-3

#room count 5, 12
train$room_bin<-NA
test$room_bin<-NA

test$room_bin[test$room_count<=5]<-1
test$room_bin[test$room_count>5 &test$room_count<=12]<-2
test$room_bin[test$room_count>12]<-3

train$room_bin[train$room_count<=5]<-1
train$room_bin[train$room_count>5 &train$room_count<=12]<-2
train$room_bin[train$room_count>12]<-3

#total call 55,25,250
train$call_bin<-NA
test$call_bin<-NA

test$call_bin[test$total_call<=25]<-1
test$call_bin[test$total_call>25 &test$total_call<=55]<-2
test$call_bin[test$total_call>55 & test$total_call<=250]<-3
test$call_bin[test$total_call>250]<-4

train$call_bin[train$total_call<=25]<-1
train$call_bin[train$total_call>25 &train$total_call<=55]<-2
train$call_bin[train$total_call>55 & train$total_call<=250]<-3
train$call_bin[train$total_call>250]<-4

train$fac_1_campur<-NA
train$fac_1_putra<-NA
train$fac_1_putri<-NA

train$fac_2_campur<-NA
train$fac_2_putra<-NA
train$fac_2_putri<-NA

train$fac_3_campur<-NA
train$fac_3_putra<-NA
train$fac_3_putri<-NA

train$fac_4_campur<-NA
train$fac_4_putra<-NA
train$fac_4_putri<-NA

train$fac_5_campur<-NA
train$fac_5_putra<-NA
train$fac_5_putri<-NA
  
train$fac_6_campur<-NA
train$fac_6_putra<-NA
train$fac_6_putri<-NA

train$fac_7_campur<-NA
train$fac_7_putra<-NA
train$fac_7_putri<-NA

train$fac_8_campur<-NA
train$fac_8_putra<-NA
train$fac_8_putri<-NA

train$poi_cluster_campur<-NA
train$poi_cluster_putra<-NA
train$poi_cluster_putri<-NA

train$price_bin_campur<-NA
train$price_bin_putra<-NA
train$price_bin_putri<-NA

train$size_bin_campur<-NA
train$size_bin_putra<-NA
train$size_bin_putri<-NA

train$room_bin_campur<-NA
train$room_bin_putra<-NA
train$room_bin_putri<-NA

train$call_bin_campur<-NA
train$call_bin_putra<-NA
train$call_bin_putri<-NA

test$fac_1_campur<-NA
test$fac_1_putra<-NA
test$fac_1_putri<-NA

test$fac_2_campur<-NA
test$fac_2_putra<-NA
test$fac_2_putri<-NA

test$fac_3_campur<-NA
test$fac_3_putra<-NA
test$fac_3_putri<-NA

test$fac_4_campur<-NA
test$fac_4_putra<-NA
test$fac_4_putri<-NA

test$fac_5_campur<-NA
test$fac_5_putra<-NA
test$fac_5_putri<-NA

test$fac_6_campur<-NA
test$fac_6_putra<-NA
test$fac_6_putri<-NA

test$fac_7_campur<-NA
test$fac_7_putra<-NA
test$fac_7_putri<-NA

test$fac_8_campur<-NA
test$fac_8_putra<-NA
test$fac_8_putri<-NA

test$poi_cluster_campur<-NA
test$poi_cluster_putra<-NA
test$poi_cluster_putri<-NA

test$price_bin_campur<-NA
test$price_bin_putra<-NA
test$price_bin_putri<-NA

test$size_bin_campur<-NA
test$size_bin_putra<-NA
test$size_bin_putri<-NA

test$room_bin_campur<-NA
test$room_bin_putra<-NA
test$room_bin_putri<-NA

test$call_bin_campur<-NA
test$call_bin_putra<-NA
test$call_bin_putri<-NA

for (i in 1:8){
  k=1
  for (j in (23+3*(i-1)):(25+3*(i-1))){
    A<- as.matrix(table(train[,i],train[,16]))
    A[1,]<- A[1,]/sum(A[1,])
    A[2,]<- A[2,]/sum(A[2,])
    A[3,]<- A[3,]/sum(A[3,])
    
    train[,j][train[,i]==0] <- A[1,k]
    train[,j][train[,i]==0.5] <- A[2,k]
    train[,j][train[,i]==1] <- A[3,k]
    
    test[,j][test[,i+1]==0] <- A[1,k]
    test[,j][test[,i+1]==0.5] <- A[2,k]
    test[,j][test[,i+1]==1] <- A[3,k]
    
    k=k+1
  }}


for (j in 47:49){
  A<- as.matrix(table(train[,17],train[,16]))
  A[1,]<- A[1,]/sum(A[1,])
  A[2,]<- A[2,]/sum(A[2,])
  A[3,]<- A[3,]/sum(A[3,])
  A[4,]<- A[4,]/sum(A[4,])
  
  train[,j][train[,17]==0] <- A[1,j-46]
  train[,j][train[,17]==1] <- A[2,j-46]
  train[,j][train[,17]==2] <- A[3,j-46]
  train[,j][train[,17]==3] <- A[4,j-46]
  
  test[,j][test[,17]==0] <- A[1,j-46]
  test[,j][test[,17]==1] <- A[2,j-46]
  test[,j][test[,17]==2] <- A[3,j-46]
  test[,j][test[,17]==3] <- A[4,j-46]
}


for (j in 60:62){
  A<- as.matrix(table(train[,23],train[,17]))
  A[1,]<- A[1,]/sum(A[1,])
  A[2,]<- A[2,]/sum(A[2,])
  A[3,]<- A[3,]/sum(A[3,])
  A[4,]<- A[4,]/sum(A[4,])
  
  train[,j][train[,23]==1] <- A[1,j-59]
  train[,j][train[,23]==2] <- A[2,j-59]
  train[,j][train[,23]==3] <- A[3,j-59]
  train[,j][train[,23]==4] <- A[4,j-59]
  
  test[,j][test[,23]==1] <- A[1,j-59]
  test[,j][test[,23]==2] <- A[2,j-59]
  test[,j][test[,23]==3] <- A[3,j-59]
  test[,j][test[,23]==4] <- A[4,j-59]
}

for (i in 19:22){
  k=1
  for (j in (50+3*(i-19)):(52+3*(i-19))){
    A<- as.matrix(table(train[,i],train[,16]))
    A[1,]<- A[1,]/sum(A[1,])
    A[2,]<- A[2,]/sum(A[2,])
    A[3,]<- A[3,]/sum(A[3,])
    
    train[,j][train[,i]==1] <- A[1,k]
    train[,j][train[,i]==2] <- A[2,k]
    train[,j][train[,i]==3] <- A[3,k]
    
    test[,j][test[,i]==1] <- A[1,k]
    test[,j][test[,i]==2] <- A[2,k]
    test[,j][test[,i]==3] <- A[3,k]
    
    k=k+1
  }}

#fitur gajadi
train$fac_campur=train$fac_1_campur*
  train$fac_2_campur*
  train$fac_3_campur*
  train$fac_4_campur*
  train$fac_5_campur*
  train$fac_6_campur*
  train$fac_7_campur*
  train$fac_8_campur*
  train$poi_cluster_campur*
  train$price_bin_campur*
  train$room_bin_campur*
  train$size_bin_campur*
  train$call_bin_campur

train$fac_putra=train$fac_1_putra*
  train$fac_2_putra*
  train$fac_3_putra*
  train$fac_4_putra*
  train$fac_5_putra*
  train$fac_6_putra*
  train$fac_7_putra*
  train$fac_8_putra*
  train$poi_cluster_putra*
  train$price_bin_putra*
  train$room_bin_putra*
  train$size_bin_putra*
  train$call_bin_putra

train$fac_putri=train$fac_1_putri*
  train$fac_2_putri*
  train$fac_3_putri*
  train$fac_4_putri*
  train$fac_5_putri*
  train$fac_6_putri*
  train$fac_7_putri*
  train$fac_8_putri*
  train$poi_cluster_putri*
  train$price_bin_putri*
  train$room_bin_putri*
  train$size_bin_putri*
  train$call_bin_putri


test$fac_campur=test$fac_1_campur*
  test$fac_2_campur*
  test$fac_3_campur*
  test$fac_4_campur*
  test$fac_5_campur*
  test$fac_6_campur*
  test$fac_7_campur*
  test$fac_8_campur*
  test$poi_cluster_campur*
  test$price_bin_campur*
  test$room_bin_campur*
  test$size_bin_campur*
  test$call_bin_campur

test$fac_putra=test$fac_1_putra*
  test$fac_2_putra*
  test$fac_3_putra*
  test$fac_4_putra*
  test$fac_5_putra*
  test$fac_6_putra*
  test$fac_7_putra*
  test$fac_8_putra*
  test$poi_cluster_putra*
  test$price_bin_putra*
  test$room_bin_putra*
  test$size_bin_putra*
  test$call_bin_putra

test$fac_putri=test$fac_1_putri*
  test$fac_2_putri*
  test$fac_3_putri*
  test$fac_4_putri*
  test$fac_5_putri*
  test$fac_6_putri*
  test$fac_7_putri*
  test$fac_8_putri*
  test$poi_cluster_putri*
  test$price_bin_putri*
  test$room_bin_putri*
  test$size_bin_putri*
  test$call_bin_putri

train<-write.csv(train,"train.csv")
test<-write.csv(test,"test_data.csv")