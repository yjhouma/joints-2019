train<-read.csv("train.csv")
test<-read.csv("test_data.csv")

test_id<-test[,1]
test<-test[,-1]
train<-train[,-1]

library(mice)
mice_model <- mice(train[,c(9:11)], method='rf')
mice_output <- complete(mice_model)
mice_output

train$poi_1<-mice_output$poi_1
train$poi_2<-mice_output$poi_2
train$poi_3<-mice_output$poi_3

train[,1:8][is.na(train[,1:8])]<-0.5


mice_model2 <- mice(train[,c(1:6,12:13)], method='rf')
mice_output2 <- complete(mice_model2)
mice_output2

train$price_monthly<-mice_output2$price_monthly
train$size<-mice_output2$size

mice_model3 <- mice(train[,-16], method='rf')
mice_output3 <- complete(mice_model3)
mice_output3

train$room_count<-mice_output3$room_count

write.csv(train,"train.csv")
write.csv(test,"test_data.csv")
