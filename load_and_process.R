#load the data table faster
library(data.table)
DT <- fread("train.csv")
DT2 <- fread("test.csv")

DT <- DT[1:1000, ]

#Split training dataset into training/validation & Cross Validation
library(caret)
set.seed(100)
inTraining <- createDataPartition(DT$Ret_121, p = .75, list = FALSE)
inTraining
training <- DT[inTraining[, 1], ]
validation  <- DT[-inTraining[, 1], ]
fitControl <- trainControl(
  method = "oob",
  #number = 2, 
  verboseIter = T
  ## repeated ten times
  #repeats = 10
)

training <- na.omit(training)

pr <- c(grep("Ret_[1-9]", names(DT), value=T)[120:179],grep("Ret_[Plus]", names(DT), value=T)[1:2])

DT2 = na.omit(DT2)
resDT <- data.table(data.frame(remove_it=DT2$Ret_2))

#Loop 
for(n in pr){
  #Random Force model
  cat("Doing model and predition for: ", n)
  fit <- train(training[, .SD, .SDcols=grep("Ret_[1-9]", names(training), value=T)[1:119]], training[, get(n)],
               method = "rf",
               trControl = fitControl,
               ## This last option is actually one
               ## for gbm() that passes through
               verbose = FALSE)
  fit
  plot(fit)
  validation = na.omit(validation)
  
  #Validation of the data
  out <- predict.train(fit, newdata=
                         validation[, .SD, .SDcols=grep("Ret_[1-9]", names(validation), value=T)[1:119]])
  
  plot(out)
  plot(validation$Ret_121)
  
  #Table the sign of corresponding elements
  s_out <- sign(out) == sign(validation$Ret_121)
  table(s_out)
  
  #Correlation 
  cor(out, validation$Ret_121)
  
  #Predict the outcome in testing dataset
  predict.train(fit, newdata=
                  DT2[, .SD, .SDcols=grep("Ret_[1-9]", names(DT2), value=T)[1:119]])
  
  res_temp <- predict.train(fit, newdata=
                              DT2[, .SD, .SDcols=grep("Ret_[1-9]", names(DT2), value=T)[1:119]])
  
  resDT[, eval(n):=res_temp]
}

resDT[, remove_it:=NULL]

write.table(resDT,file="out.csv", quote = F)