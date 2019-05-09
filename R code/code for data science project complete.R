# Neccesary packages
path<-"C:\\Users\\Sloan Stinnett\\Documents\\Data sets\\files for Data Science Capstone"
setwd(path)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidytext)
library(tidyverse)
library(mlr)
library(rpart)
library(e1071)
library(FSelector)
#reading the data into R

## conversation data
conversations<-read.table("supreme.conversations.revised.txt", header = FALSE, sep = "@", fill=TRUE, quote="")
colnames(conversations)<-c("CASE_ID","UTTERANCE_ID","AFTER_PREVIOUS","SPEAKER","IS_JUSTICE","JUSTICE_VOTE","PRESENTATION_SIDE","UTTERANCE")
## outcome data
outcomes<-read.table("supreme.outcome.revised.txt", header = FALSE, sep = "@", fill=TRUE, quote="")
colnames(outcomes)<-c("CASE_ID","WINNING_SIDE")

#Pre-paring the data for our purposes

## creating a single dataframe
##merging data into a single data frame
mydata<-na.omit(merge(conversations,outcomes,by = "CASE_ID"))
## creating a list of case ids
cases<-as.character(levels(mydata$CASE_ID))
## converting the conversations from factor variables to strings
mydata[,8]<-as.character(mydata[,8])
## converting is justice variable to strings
mydata[,5]<-as.character(mydata[,5])
## converting case id to a string 
mydata[,1]<-as.character(mydata[,1])
## splitting the data up by case 
data.by.case<-split(mydata,mydata$CASE_ID,drop = TRUE)

## merging all the utterances into speachs 
attach(mydata)
attach(data.by.case)
speech.maker<-function(x){
  #finding speaker names
  speaker.names<-mydata[CASE_ID==x & IS_JUSTICE==' NOT JUSTICE ',4]%>%droplevels()%>%levels()
  justice.v.nonjustice<-split(data.by.case[[x]],data.by.case[[x]]$IS_JUSTICE,drop = TRUE)
  speakers.in.court<-split(justice.v.nonjustice$` NOT JUSTICE `,justice.v.nonjustice$` NOT JUSTICE `$SPEAKER,drop = TRUE)
  if(length(speakers.in.court)==1){
    y<-speaker.names[1]
    speech.1<-str_c(speakers.in.court[[1]][,8],collapse = " ")
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == y ,10]<-speech.1 
    assign('mydata',mydata,envir=.GlobalEnv)
  }
  if(length(speakers.in.court)==2){
    y<-speaker.names[1]
    z<-speaker.names[2]
    speech.1<-str_c(speakers.in.court[[1]][,8],collapse = " ")
    speech.2<-str_c(speakers.in.court[[2]][,8],collapse = " ")
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == y ,10]<-speech.1 
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == z ,10]<-speech.2
    assign('mydata',mydata,envir=.GlobalEnv)
  }
  if(length(speakers.in.court)==3){
    y<-speaker.names[1]
    z<-speaker.names[2]
    w<-speaker.names[3]
    speech.1<-str_c(speakers.in.court[[1]][,8],collapse = " ")
    speech.2<-str_c(speakers.in.court[[2]][,8],collapse = " ")
    speech.3<-str_c(speakers.in.court[[3]][,8],collapse = " ")
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == y ,10]<-speech.1 
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == z ,10]<-speech.2
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == w ,10]<-speech.3
    assign('mydata',mydata,envir=.GlobalEnv)
  }
  if(length(speakers.in.court)==4){
    y<-speaker.names[1]
    z<-speaker.names[2]
    w<-speaker.names[3]
    v<-speaker.names[4]
    speech.1<-str_c(speakers.in.court[[1]][,8],collapse = " ")
    speech.2<-str_c(speakers.in.court[[2]][,8],collapse = " ")
    speech.3<-str_c(speakers.in.court[[3]][,8],collapse = " ")
    speech.4<-str_c(speakers.in.court[[4]][,8],collapse = " ")
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == y ,10]<-speech.1 
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == z ,10]<-speech.2
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == w ,10]<-speech.3
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == v ,10]<-speech.4
    assign('mydata',mydata,envir=.GlobalEnv)
  }
  if(length(speakers.in.court)==5){
    y<-speaker.names[1]
    z<-speaker.names[2]
    w<-speaker.names[3]
    v<-speaker.names[4]
    u<-speaker.names[5]
    speech.1<-str_c(speakers.in.court[[1]][,8],collapse = " ")
    speech.2<-str_c(speakers.in.court[[2]][,8],collapse = " ")
    speech.3<-str_c(speakers.in.court[[3]][,8],collapse = " ")
    speech.4<-str_c(speakers.in.court[[4]][,8],collapse = " ")
    speech.5<-str_c(speakers.in.court[[5]][,8],collapse = " ")
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == y ,10]<-speech.1 
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == z ,10]<-speech.2
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == w ,10]<-speech.3
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == v ,10]<-speech.4
    mydata[mydata$CASE_ID == x & mydata$SPEAKER == u ,10]<-speech.5
    assign('mydata',mydata,envir=.GlobalEnv)
  }
  return(paste(x,"speechs complete"))
}
#cases that are working fine
lapply(X=cases[1:56],FUN = speech.maker)
lapply(X=cases[58:67],FUN = speech.maker)
lapply(X=cases[69:91],FUN = speech.maker)
lapply(X=cases[93:105],FUN = speech.maker)
lapply(X=cases[106:139],FUN = speech.maker)
lapply(X=cases[141:163],FUN = speech.maker)
lapply(X=cases[165:182],FUN = speech.maker)
lapply(X=cases[184:202],FUN = speech.maker)
detach(mydata)
detach(data.by.case)
# cases where there is an issue 
# 57,68,92,140,164,183,203,204
### upon further investigation the issue is that we do not have the results for these cases, therefor they will be left out of the analysis.

# removing utterances, utterance ID and after previous
mydata<-mydata[,-c(2,3,6,8)]
## removing justices from data 
mydata.no.justices<-mydata[mydata$IS_JUSTICE ==' NOT JUSTICE ',]
#removing all duplicate lines
Final.data<-distinct(mydata.no.justices)

# some preliminary findings

#ratio of wins between petitioner and Respondent
## Respondents
Win.percent.respondent<-nrow(outcomes[outcomes$WINNING_SIDE==' RESPONDENT',])/nrow(outcomes)
## Petitioners
Win.percent.petitioners<-nrow(outcomes[outcomes$WINNING_SIDE==' PETITIONER',])/nrow(outcomes)
# charting the result
## creating a dataframe
df.win <- data.frame(
  group = c("Respondent", "Petitioner"),
  value = c((Win.percent.respondent*1), (Win.percent.petitioners*1))
)
## creating a bar plot
bp.win <- ggplot(df.win, aes(x=" ", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp.win
## creating the pie chart
pie.win <- bp.win + coord_polar("y", start=0)+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = percent(value)), size=5)
pie.win

# Sentiment Analysis

## naming column 6 in final.data
colnames(Final.data)[6]<- "speech"

#adding columns for sentiment proportions
Final.data$ratio.anger<-rep(NA,each = 559)
Final.data$ratio.anticipation<-rep(NA,each = 559)
Final.data$ratio.disgust<-rep(NA,each = 559)
Final.data$ratio.fear<-rep(NA,each = 559)
Final.data$ratio.joy<-rep(NA,each = 559)
Final.data$ratio.negative<-rep(NA,each = 559)
Final.data$ratio.positive<-rep(NA,each = 559)
Final.data$ratio.sadness<-rep(NA,each = 559)
Final.data$ratio.surprise<-rep(NA,each = 559)
Final.data$ratio.trust<-rep(NA,each = 559)
Final.data$length<-rep(NA,each= 559)
## sentiment finder
attach(Final.data)
number.speech<-1:559

sentiment.finder<- function(x){
  # identifying the speech
  speech <-Final.data[x,6]
  s<- data_frame(txt = speech)
  # Turning the speech into a dataframe of words
  tidy.speech<-s%>%unnest_tokens(word, txt)
  # removing stop words
  data("stop_words")
  tidy.speech.filtered<-tidy.speech %>% anti_join(stop_words)
  # assessing sentiment 
  ## some of the speechs are extremely short and contain no sentiment words, these speeches where causeing the program to hit
  ## an error as tidy.speech.sentiment <- tidy.speech.filtered%>% inner_join(get_sentiments("nrc")) %>% count( sentiment)
  ## would not function so I arbitrarilly set the standard that to be included the speech must be at least 15 words long 
  ## this resolved the error.
  if(nrow(tidy.speech.filtered)<15){print("too short")}else{
    tidy.speech.sentiment <- tidy.speech.filtered%>% inner_join(get_sentiments("nrc")) %>% count( sentiment)
    ## if the sentiment appears in the speech than it will have a value that is not NA and the ratio is assigned 
    ## otherwise the sentiment does not appear and we are given NA this indicates that the ratio is 0 and so a value of 0 is assigned
    if(is.na(tidy.speech.sentiment[1,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,7]<- tidy.speech.sentiment[1,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,7]<-0}
    if(is.na(tidy.speech.sentiment[2,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,8]<- tidy.speech.sentiment[2,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,8]<-0}
    if(is.na(tidy.speech.sentiment[3,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,9]<- tidy.speech.sentiment[3,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,9]<-0}
    if(is.na(tidy.speech.sentiment[4,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,10]<- tidy.speech.sentiment[4,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,10]<-0}
    if(is.na(tidy.speech.sentiment[5,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,11]<- tidy.speech.sentiment[5,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,11]<-0}
    if(is.na(tidy.speech.sentiment[6,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,12]<- tidy.speech.sentiment[6,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,12]<-0}
    if(is.na(tidy.speech.sentiment[7,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,13]<- tidy.speech.sentiment[7,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,13]<-0}
    if(is.na(tidy.speech.sentiment[8,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,14]<- tidy.speech.sentiment[8,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,14]<-0}
    if(is.na(tidy.speech.sentiment[9,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,15]<- tidy.speech.sentiment[9,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,15]<-0}
    if(is.na(tidy.speech.sentiment[10,2]/sum(tidy.speech.sentiment[,2]))==FALSE){Final.data[x,16]<- tidy.speech.sentiment[10,2]/sum(tidy.speech.sentiment[,2])}else{Final.data[x,16]<-0}
    Final.data[x,17]<- nrow(tidy.speech)
  }
  assign("Final.data",Final.data,envir = .GlobalEnv)
  return("sentiments calculated")
}
lapply(X=number.speech[1:559], FUN = sentiment.finder)
# this omits all the speeches that where too short from the data leaving 540 observations
Final.data<- na.omit(Final.data)

# more preliminary findings 

## overall Sentiments 
### overall ratio of sentiments
average.anger<-sum(Final.data$ratio.anger)/540
average.anticipation<-sum(Final.data$ratio.anticipation)/540
average.disgust<-sum(Final.data$ratio.disgust)/540
average.fear<-sum(Final.data$ratio.fear)/540
average.joy<-sum(Final.data$ratio.joy)/540
average.negative<-sum(Final.data$ratio.negative)/540
average.positive<-sum(Final.data$ratio.positive)/540
average.sadness<-sum(Final.data$ratio.sadness)/540
average.surprise<-sum(Final.data$ratio.surprise)/540
average.trust<-sum(Final.data$ratio.trust)/540
### creating a dataframe
df.sentiment1 <- data.frame(
  group = c("Anger", "Anticipation","Disgust","Fear","Joy","Negative","Positive","Sadness","Surprise","Trust"),
  value = c(average.anger,average.anticipation,average.disgust,average.fear,average.joy,average.negative,average.positive,average.sadness,average.surprise,average.trust)
)
df.sentiment<-df.sentiment1[order(df.sentiment1$value, decreasing = TRUE),]
### creating a bar plot
bp.sentiment <- ggplot(df.sentiment, aes(x=group, y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp.sentiment

## average length 
average.length<-sum(Final.data$length)/540
average.length

# Classification Task
detach(Final.data)
set.seed(12345)
## reducing data to just the features of interest
Case.Info<-Final.data[,-c(1,2,3,6)]
# Break up the data:

n <- nrow(Case.Info)

train <- sample(n, size = .8*n)

test  <- setdiff(1:n, train)

Case.Info.train <- Case.Info[train,]

Case.Info.test  <- Case.Info[test, ]

#Defining the Task 

theTask <- makeClassifTask(id = "taskname", data = Case.Info.train, target = 'WINNING_SIDE')
print(theTask)

#resampling strat

resampleStrat<-makeResampleDesc(method="CV", iters =3)

# the tuning strategy

tuneMethod <- makeTuneControlRandom(maxit = 100L)

#prediction algorithms

predAlg.RF<-makeLearner("classif.randomForest",predict.type = "response")

predAlg.SVM <- makeLearner("classif.svm",predict.type = "response")

#model parameters 
modelParams.RF <- makeParamSet(makeIntegerParam("ntree",lower = 50, upper = 500),
                               makeIntegerParam("mtry", lower = 3, upper = 10),
                               makeIntegerParam("nodesize", lower = 10, upper = 50))

modelParams.SVM <- makeParamSet(makeDiscreteParam("kernel", values = c("radial")),
                                makeDiscreteParam("cost", values = c(2^-10,2^-2,2^-1,2^0,2^1,2^2,2^10)),
                                makeDiscreteParam("gamma", values = c(2^-10,2^-2,2^-1,2^0,2^1,2^2,2^10)))

#tuning the model 
tunedModel.RF <- tuneParams(learner = predAlg.RF, 
                            resampling = resampleStrat, 
                            task = theTask, 
                            par.set = modelParams.RF, 
                            control = tuneMethod,
                            measures = list(f1),
                            show.info = TRUE)


tunedModel.SVM <- tuneParams(learner = predAlg.SVM,
                             task = theTask,
                             resampling = resampleStrat,
                             measures = list(f1),       # RMSE performance measure, this can be changed to one or many
                             par.set = modelParams.SVM,
                             control = tuneMethod,
                             show.info = TRUE)
#hyper parameters
predAlg.RF.hyp <- setHyperPars(learner = predAlg.RF, par.vals = tunedModel.RF$x)

predAlg.SVM.hyp <- setHyperPars(learner=predAlg.SVM, par.vals = tunedModel.SVM$x)

#training 
trained.RF<- train(learner = predAlg.RF.hyp,task= theTask)

trained.SVM<- train(learner = predAlg.SVM.hyp,task= theTask)


#predictions
prediction.RF<- predict(trained.RF, newdata = Case.Info.test)

prediction.SVM<- predict(trained.SVM, newdata = Case.Info.test)

#performance 
## In sample performance
tunedModel.RF

tunedModel.SVM
##out of sample peformance
confusionmatrix.RF<-calculateConfusionMatrix(prediction.RF)
confusionmatrix.RF

confusionmatrix.SVM<-calculateConfusionMatrix(prediction.SVM)
confusionmatrix.SVM

precision.RF<- confusionmatrix.RF[1]$result[1]/(confusionmatrix.RF[1]$result[1]+confusionmatrix.RF[1]$result[2])
precision.RF 

Recall.RF<- confusionmatrix.RF[1]$result[1]/(confusionmatrix.RF[1]$result[1]+confusionmatrix.RF[1]$result[4])
Recall.RF

precision.SVM<- confusionmatrix.SVM[1]$result[1]/(confusionmatrix.SVM[1]$result[1]+confusionmatrix.SVM[1]$result[2])
precision.SVM

Recall.SVM<- confusionmatrix.SVM[1]$result[1]/(confusionmatrix.SVM[1]$result[1]+confusionmatrix.RF[1]$result[4])
Recall.SVM

OOS.f1.RF<- 2*((precision.RF*Recall.RF)/(precision.RF+Recall.RF))
OOS.f1.RF

OOS.f1.SVM<- 2*((precision.SVM*Recall.SVM)/(precision.SVM+Recall.SVM))
OOS.f1.SVM

correct.response.rate.RF<-76/108
correct.response.rate.RF

correct.response.rate.SVM<-72/108
correct.response.rate.SVM

# confusion matrixs as LaTeX tables
## random forest
stargazer::stargazer(confusionmatrix.RF[1]$result)
## support vector machine
stargazer::stargazer(confusionmatrix.SVM[1]$result)

