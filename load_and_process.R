#load the data table faster
library(data.table)
if(!file.exists("./data/train.csv")) unzip("data/train.csv.zip", exdir = "data")
if(!file.exists("data/test.csv")) unzip("data/test.csv.zip", exdir = "data")

DT <- fread("data/train.csv")
DT2 <- fread("data/test.csv")

## use only part of data, while testing algorithm
DT <- DT[1:1000, ]

## Split training dataset into training/validation & Cross Validation
library(caret)
set.seed(100)
inTraining <- createDataPartition(DT$Ret_121, p = .75, list = FALSE)
inTraining
training <- DT[inTraining[, 1], ]
validation  <- DT[-inTraining[, 1], ]

## defining vector of predictors and outcome variabl
predictors <- names(DT2)
predictors <- predictors[predictors != "Id"]

outcome_var <- names(DT)
outcome_var <- outcome_var[! (outcome_var %in% names(DT2) )]

## Imputing missing values
preProcObj <- preProcess(method="bagImpute", training[, .SD, .SDcols=predictors])
#preProcObj <- preProcess(method="knnImpute", training[, .SD, .SDcols=predictors])

training[, eval(predictors):= predict(preProcObj, 
                                      training[, .SD, .SDcols=predictors])]

validation[, eval(predictors):= predict(preProcObj, 
                                        validation[, .SD, .SDcols=predictors])]

DT2[, eval(predictors):= predict(preProcObj, 
                                        DT2[, .SD, .SDcols=predictors])]


## Training model and making predictions
fitControl <- trainControl(
  method = "oob",
  verboseIter = T
)

## Loop - making model and doing predictions for evey outcome variable independently 

#data table to collect resutls
resDT <- data.table(Id=DT2[, Id])
#file for print intermideate results
if( file.exists("CM.txt") ) file.remove("CM.txt")

for(n in outcome_var[1:2]){ #use [1:2 if only two out needed - for testing]
  #Random Force model
  cat("Doing model and predition for: ", n)
  fit <- train(training[, .SD, .SDcols=predictors], training[, get(n)],
               method = "rf",
               trControl = fitControl,
               verbose = FALSE)
  fit
  #plot(fit)
  
  #predition on validation set
  out <- predict(fit, newdata = validation[, .SD, .SDcols=predictors])
  
  #basic analysis of accuracy
  out_sign <- as.logical( sign(out)+1 )
  real_sign <- validation[, as.logical( sign(get(n)) +1 )]
  CM <- confusionMatrix(out_sign, real_sign)
  capture.output(CM, file = "CM.txt", append = T)
  
  #Predict the outcome in testing dataset
  res_temp <- predict(fit, newdata = DT2[, .SD, .SDcols=predictors])
  #saving resultss of a current loop
  resDT[, eval(n):=res_temp]
}

#writing format should be adjusted
#short resDT format - to long, before writing
write.csv(resDT,file="out.csv", quote = F, row.names = F)
