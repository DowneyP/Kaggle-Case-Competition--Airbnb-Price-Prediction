getwd()
setwd('/Users/pengyuxuan/Desktop/r/pricelala2')
data = read.csv('analysisData.csv')
score = read.csv('scoringData.csv')
#install.packages('caTools')
library(jomo)
library(mice)
library(caret)
library(ISLR)
library(ggplot2)
library(caTools)
library(lattice)
str(score)
head(score)
head(analysisdata)


# Read data and construct a simple model
#data = read.csv('analysisData.csv')
#model = lm(price~minimum_nights+review_scores_rating,data)

# Read scoring data and apply model to generate predictions
#scoringData = read.csv('scoringData.csv')
#pred = predict(model,newdata=scoringData)

# Construct submission from predictions
#submissionFile = data.frame(id = scoringData$id, price = pred)
#write.csv(submissionFile, 'sample_submission.csv',row.names = F)

###########################################################################
#Data loading
###########################################################################
# Remove the data was saved in the database with missing values showing as 'NA' rather blank
names(data)
#给scoredata加一列price 里面全部放1
score$price <- paste0(as.numeric(1))
names(score)
#换好位置
score<-score[,c(1:46,91,47:90)]
#拷贝id以后用
id<-score$id
names(score)
#确定score和data的列名都一样
names(score)==names(data)
#bind
d <- rbind(data,score)


# coerce feature types  to their correct types for analysis
d
names(d)

d$host_name <- as.factor(d$host_name)
d$host_since <-as.factor(d$host_since)
d$host_location <- as.factor(d$host_location)
d$host_response_time <- as.factor(d$host_response_time)
d$host_response_rate <- as.numeric(d$host_response_rate)
d$host_acceptance_rate <- as.numeric(d$host_acceptance_rate)
d$host_is_superhost <- as.factor(d$host_is_superhost)
d$neighbourhood <-as.factor(d$neighbourhood)
d$host_verifications <- as.factor(d$host_verifications)
d$host_has_profile_pic <- as.factor(d$host_has_profile_pic)
d$host_identity_verified <- as.factor(d$host_identity_verified )
d$street <- as.factor(d$street)
d$neighbourhood <- as.factor(d$neighbourhood)
d$neighbourhood_cleansed <- as.factor(d$neighbourhood_cleansed)
d$neighbourhood_group_cleansed <- as.factor(d$neighbourhood_group_cleansed)
d$city <- as.factor(d$city)
d$state <- as.factor(d$state)
d$market <- as.factor(d$market)
d$smart_location <- as.factor(d$smart_location)
d$country_code <- as.factor(d$country_code)
d$is_location_exact <- as.factor(d$is_location_exact)
d$property_type <- as.factor(d$property_type)
d$room_type <- as.factor(d$room_type)
d$bathrooms <- as.numeric(d$bathrooms)
d$bed_type <- as.factor(d$bed_type)
d$amenities <- as.factor(d$amenities)
d$calendar_updated <- as.factor(d$calendar_updated)
d$requires_license <- as.factor(d$requires_license)
d$license <- as.factor(d$license)
d$jurisdiction_names <- as.factor(d$jurisdiction_names)
d$instant_bookable <- as.factor(d$instant_bookable)
d$is_business_travel_ready <- as.factor(d$is_business_travel_ready)
d$cancellation_policy <- as.factor(d$cancellation_policy)
d$require_guest_profile_picture <- as.factor(d$require_guest_profile_picture)
d$require_guest_phone_verification <- as.factor(d$require_guest_phone_verification)
d$reviews_per_month <- as.numeric(d$reviews_per_month)

str(d)

########################################################################
##Handling missing values
########################################################################
# % of rowing having missing values
dim(d[!complete.cases(d),])[[1]]/nrow(d)*100

# missing value report
source('DataQualityReport.R')
DataQualityReport(d)

# We observe there are some features that have many missing values.
#46                                  square_feet numeric         36483            0.97     0     685.87
#48                                 weekly_price numeric         33285            9.65   100     575.99
#49                                monthly_price numeric         36617            0.60   500     851.97
#50                             security_deposit numeric         12864           65.08     0      168.2

d$square_feet <- NULL
d$weekly_price <- NULL
d$monthly_price <- NULL
d$security_deposit <- NULL

# Imputing missing values using predictive modeling (CART (Classificaton & Regression 
# Tree)

# We diss the variables that has too much pressure to memory

d$name <- NULL
d$host_name <- NULL
d$host_since <- NULL
d$host_location <- NULL
d$host_response_time <- NULL
d$host_neighbourhood <- NULL
d$host_verifications <- NULL
d$street <- NULL
d$neighbourhood <- NULL
d$market <- NULL
d$country <- NULL
d$amenities <- NULL
d$calendar_updated <- NULL
d$summary <-NULL
d$space <-NULL
d$description<- NULL
d$neighborhood_overview <- NULL
d$notes <- NULL
d$transit <- NULL
d$access <-NULL
d$interaction <-NULL
d$house_rules <-NULL
d$host_about <-NULL


#indx <- apply(d, 2, function(x) any(is.na(x))) # to check what variables has missing value for decrease pressure on memory
#colnames(d[indx])


DataQualityReport(d)
id<-d$id
d$id<-NULL
DataQualityReport(d)

str(d)
#把不需要impute的放进d1
d1<-d[,c(1:3,6:20,22:23,25:62)]
#需要impute的放进d2
d2<-d[,c("host_listings_count","host_total_listings_count","beds","cleaning_fee","reviews_per_month")]


imputedValues <- mice(data=d2, m=3, method="cart", seed=2019)
d2 <- complete(imputedValues,1)
DataQualityReport(d2)
#把d1,d2合并了
d <- cbind(d1,d2)
#这行有一个missing value， 把整行删了
DataQualityReport(d)
library(dplyr)
d$host_total_listings_count <- NULL
d$price<-as.numeric(d$price)
save(imputedValues, file = 'imputedValues.Rda')
save(d,file = 'd.Rda')

DataQualityReport(d)
# we can now delete some objects in our R environment that are no longer needed
rm(imputedValues,d1,d2)

################################################################################
## Data clean up
################################################################################

names(d)
names(d)[20] <- 'y'
names(d)
d<-d[,c(20,1:19,21:62)]
names(d)

################################################################################
## Creating Dummy Variables
################################################################################

library(caret)

#remove the factors that have only 1 level
str(d)
d$country_code <- NULL
d$has_availability  <- NULL
d$requires_license <- NULL
d$is_business_travel_ready<- NULL
d$first_review<- NULL
d$last_review <-NULL
DataQualityReport(d)
#把不需要dummy的放进d1
t1<-d[,c(2:3,16:18,20:42,49:56)]
#需要dummy的放进d2
t2<-d[,c('y','host_is_superhost','host_has_profile_pic','host_identity_verified','neighbourhood_cleansed',
           'neighbourhood_group_cleansed','city' ,'state','zipcode','smart_location', 'is_location_exact',
           'property_type','room_type' ,'bed_type' ,'license' ,'jurisdiction_names'  ,'instant_bookable',
           'cancellation_policy', 'require_guest_profile_picture','require_guest_phone_verification')]


dummies <- dummyVars(y~host_is_superhost+host_has_profile_pic+host_identity_verified+neighbourhood_cleansed+
                       neighbourhood_group_cleansed+city +state+zipcode+smart_location+ is_location_exact +
                       property_type+room_type +bed_type +license +jurisdiction_names  +instant_bookable+
                       cancellation_policy+ require_guest_profile_picture+require_guest_phone_verification,
                     data = t2)                #create dumyes for Xs

ex <- data.frame(predict(dummies, newdata = t2))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
t2 <- cbind(d$y, ex)                              # combine your target variable with Xs

#把d1,d2合并了
d3 <- cbind(t2,t1)

d <- d3

DataQualityReport(d)
names(d)
str(d)
names(d)[1] <- "y"                               # make target variable called 'y'
names(d)
rm(dummies, ex,d3,t1,t2)                                  # delete temporary things we no longer need


save(d,file = 'd before zero.Rda')
################################################################################
# Remove Zero- and Near Zero-Variance Predictors
################################################################################

#library(caret)
#dim(d) 

#nvz <- nearZeroVar(d[,2:ncol(d)], uniqueCut = 10) # identify columns that are "near zero"
#nvz
#d_filtered <- d[,2:ncol(d)][, -nzv] 
#dim(d_filtered)                                # dimension of your filtered dataset

#nvz

#setdiff(names(d), names(d_filtered))
#d <- cbind(d$y, d_filtered)   # combine y with the Xs
#names(d)[1] <- "y"            # fix the y variable name

#rm(nvz)


################################################################################
# Identify Correlated Predictors and remove them
################################################################################

library(caret)
DataQualityReport(d)
# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])                    # summarize the correlations


highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr)                  # calculate a new correlation matrix

# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update our d dataset by removing those filtered variables that were highly correlated
d <- cbind(d$y, filteredDescr)
names(d)
names(d)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up

################################################################################
# Identifying linear dependencies and remove them
################################################################################

library(caret)
# first save response
y <- d$y

# create a column of 1s. This will help identify all the right linear combos
d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(d)
comboInfo

# remove columns identified that led to linear combos
d <- d[, -comboInfo$remove]

# remove the "ones" column in the first column
d <- d[, c(2:ncol(d))]

# Add the target variable back to our data.frame
d <- cbind(y, d)
names(d)
rm(y, comboInfo)  # clean up
################################################################################
# Standardize (and/ normalize) your input features.
################################################################################

save(d,file = 'd before standardize.Rda')

numcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d)
catcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d)
dNums <- d[,numcols]
dCats <- d[,catcols]

str(d)
# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale"))
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("range","YeoJohnson"))
# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this。
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
d <- cbind(dNums, dCats)

rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up

############复原#############
AnalysisData <- d[1:36839,]
ScoringData<-d[36840:46049,]
ScoringData$Id<-id
ScoringData<-ScoringData[,c(795,2:794)]
str(ScoringData)

################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results
library(caret)
AnalysisData2 <- AnalysisData

inTrain <- createDataPartition(y = AnalysisData2$y,   # outcome variable
                               p = .85,   # % of training data you want
                               list = F)
# create your partitions
train <- AnalysisData2[inTrain,]  # training data set
test <- AnalysisData2[-inTrain,]  # test data set

################################################################################
#  Downey try forward and backward selection
################################################################################


id<-score$id
save(d,file = 'd before back and forward.Rda')
save(AnalysisData, file = 'AnalysisData.Rda')
save(ScoringData, file = 'ScoringData.Rda')

mf <- lm(y ~ ., data=train)
summary(mf)

# automatic backward selection
library(leaps)
mb <- regsubsets(y ~ ., data=train
                 , nbest=1
                 , intercept=T
                 , method='backward'
                 , really.big=T
)


vars2keep <- data.frame(summary(mb)$which[which.max(summary(mb)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]

# here are the final features found to be statistically significant
vars2keep
#[1] "accommodates"                 "bathrooms"                    "bedrooms"                    
#[4] "cleaning_fee"                 "neighbourhood_cleansedHarlem" "cityNewYork.1"               
#[7] "property_typeResort"          "room_typePrivateroom"         "room_typeSharedroom" 

modelFormula <- paste("y ~ accommodates + bathrooms + bedrooms + cleaning_fee + neighbourhood_cleansedHarlem +
                      + cityNewYork.1 + property_typeResort + room_typePrivateroom +
                      room_typeSharedroom") 
mb <- lm(modelFormula, data=train)
summary(mb)
predback = predict(mb,newdata=test)

rmseback = sqrt(mean((predback-test$y)^2)); rmseback
rm(rmsebcak,rmseCV,vars2keep,modelFormula,submissionFile,mf,larsBetas)

predback = predict(RF,newdata=ScoringData)
rmseback = sqrt(mean((predback-ScoringData$y)^2)); rmseback
submissionFile = data.frame(id = ScoringData$Id, price = ScoringData )
write.csv(submissionFile, 'submission xgb.csv',row.names = F)


#try lm with all freatures cause cass said so
# cass is wrong again
#mb <- lm(y~., data=train)

#summary(mb)
#predLm = predict(mb,newdata=test)
#rmseLm = sqrt(mean((predLm-test$y)^2)); rmseLm


################################################################################
# Downey try LASSO
################################################################################

library(caret)
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)
# train a LASSO
lassofit <- train(y ~ .,
                  data = train,
                  method = "lars",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")
lassofit

# optimal s
lassofit$bestTune[[1]]


plot(x=lassofit$results$fraction, y=lassofit$results$RMSE
     , col="blue", pch=19
     , main="RMSE vs s from caret runs", xlab="S", ylab="RMSE")

predLasso = predict(RF,newdata=ScoringData)

submissionFile = data.frame(id = ScoringData$Id, price = predLasso )
write.csv(submissionFile, 'submission Lasso.csv',row.names = F)



################################################################################
# Downey try Random Forests x
################################################################################


library(caret)
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)


# train a random forest.
rf <- train(y ~ .,
            data = AnalysisData,
            method = "rf",
            importance=T,    # we add this in or it varImp cannot be computed
            trControl = ctrl,
            tuneLength = 10,
            metric = "RMSE"
)
rf


library(mgcv) # used to save models

varImp(rf)
plot(varImp(rf))

################################################################################
# Downey try RF 2
################################################################################


install.packages('raondomForest')
library(randomForest)


tuneGrid = expand.grid(mtry=1:5)
set.seed(2019)

RF = train(y~.,data=train,
                 method="rf",ntree=1000,trControl=ctrl,tuneGrid=tuneGrid )

predRF = predict(RF,newdata=test)
rmseRF = sqrt(mean((predRF-test$y)^2)); rmseRF


################################################################################
# Downey try boosting with cv x
################################################################################

library(caret)
library(gbm)

set.seed(2019)
tuneGrid=  expand.grid(n.trees = 1000, interaction.depth = c(1,2),
                       shrinkage = (1:100)*0.001,n.minobsinnode=5)
garbage = capture.output(cvBoost <- train(y~.,data=train,method="gbm", 
                                          trControl=ctrl, tuneGrid=tuneGrid))
boostCV = gbm(y~.,data=train,distribution="gaussian",
              n.trees=cvBoost$bestTune$n.trees,
              interaction.depth=cvBoost$bestTune$interaction.depth,
              shrinkage=cvBoost$bestTune$shrinkage,
              n.minobsinnode = cvBoost$bestTune$n.minobsinnode)
predBoostCV = predict(boostCV,test,n.trees=1000)
rmseBoostCV = sqrt(mean((predBoostCV-test$y)^2)); rmseBoostCV





install.packages('raondomForest')
library(randomForest)


glm <- train(y ~ .,
                  data = train,
                  method = "glm",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")

predGlm = predict(glm,newdata=test)
rmseGlm = sqrt(mean((predGlm-test$y)^2)); rmseGlm

summary(glm)





forest = randomForest(y~.,data=AnalysisData,ntree = 600, metric='RMSE')
predRF = predict(forest,newdata=ScoringData)
submissionFile = data.frame(id = ScoringData$Id, price = predRF)
write.csv(submissionFile, 'submission n600.csv',row.names = F)



################################################################################
# Downey try xgboost
################################################################################

load('AnalysisDataP.Rda')
load('ScoringDataP.Rda')
train = AnalysisData
test = ScoringData[-1]

library(xgboost)
X = data.matrix(train[-1])
library(Matrix)
X = Matrix(X, sparse = TRUE)
Y = data.matrix(train[1])
traindata = list(data = X, label = Y)
train_xgb = xgb.DMatrix(data = traindata$data, label = traindata$label) 
testdata = data.matrix(test)
test_xgb = xgb.DMatrix(data = testdata)

xgb_cv = xgb.cv(data = train_xgb,
                nrounds = 30,
                nthread = 5,
                nfold = 10,
                metrics = 'rmse',
                subsample = 0.8,
                max_depth = 10,
                eta = 0.2,
                seed = 1)

fit_xgb = xgboost(train_xgb,
                  max_depth = 15, 
                  nrounds = 25, 
                  subsample = 0.8,
                  eta = 0.1,
                  seed = 1,
                  nthread = 3)


predxgb = predict(RF,newdata=testdata)
rmsexgb = sqrt(mean((predRF-test$y)^2)); rmseRF
submissionFile = data.frame(id = ScoringData$Id, price = predxgb )
write.csv(submissionFile, 'submission xgb.csv',row.names = F)





