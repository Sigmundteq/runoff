#导入包
library("mlr3")
library("mlr3tuning")
library("mlr3verse")
library("mlr3hyperband")
library("mlr3spatiotempcv")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3extralearners")
library("mlr3viz")

#减少冗长
lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

#导入数据
ecuador=read.csv("e:/JX_alldata/process-data/csv/impact_mean_data.csv")
ecuador=ecuador[,c(-5,-6)]
ecuador$lucc=as.numeric(ecuador$lucc)
ecuador$DEM=as.numeric(ecuador$DEM)

#划分训练集和测试集
#sIndex<-createDataPartition(ecuador$mean,p=0.7,list=FALSE)
setwd("E:/JX_alldata/process-data/Rdata")
load("train.RData")
load("test.RData")
write.csv(outpTest,file="test.csv")
#save(sIndex,file="train_index.RData")
#outpTrain<-ecuador[sIndex,]
#outpTest<-ecuador[-sIndex,]
#save(outpTrain,file="train.RData")
#save(outpTest,file="test.RData")
outpTrain$lucc=as.numeric(outpTrain$lucc)
outpTest$lucc=as.numeric(outpTest$lucc)
#回归模型准备
#rf模型参数设置
learner=lrn("regr.randomForest",mtry = to_tune(1, 7),ntree=to_tune(100,1000))
#模型调参具体思路(先根据param_set查看模型参数，然后设置超参数的调整范围进行网格搜索，查看最优参数设置，并将该参数设置应用于模型进行训练模型，
#将训练好的模型应用于测试集，可以根据learner$impotrance查看变量重要性)
#learner=lrn("classif.rpart",predict_type="prob")
learner$param_set$ids()

#设置超参数的调整范围
search_space <- ps(
  ntree = p_dbl(lower = 100, upper = 1000),
  mtry = p_int(lower = 1, upper = 7)
)

hout <- rsmp("holdout", ratio = 0.7)
measures <- msrs(c("regr.rsq"))

#设定何时停止训练
evals20 <- trm("evals", n_evals = 40) 

task=TaskRegr$new(
  id="mean",backend=as.data.frame(outpTrain),target = "mean"
)

#网格搜索
instance <- tune(
  task = task,
  learner = learner,
  resampling = hout,
  measure = measures,
  search_space = search_space,
  method = "grid_search",
  resolution = 5,
  term_evals = 40
)

#随机搜索
instance = tune(
  method = "random_search",
  task = task,
  learner = learner,
  resampling = hout,
  measure = msr("regr.rsq"),
  term_evals = 1
)


#查看参数设置结果
instance$result_learner_param_vals

instance$result_y

instance$archive

#将调好的超参数重新训练模型
learner$param_set$values <- instance$result_learner_param_vals
learner$param_set
learner$train(task)
learner$importance()
train=learner$predict_newdata(ecuador)
test=learner$predict_newdata(outpTest)
test_pre=as.data.frame(test$response)
write.csv(test_pre,file="catboost_test_pre.csv")

pre=as.data.frame(train$response)
write.csv(pre,file="catboost_pre.csv")
train$score(measure=msr("regr.rsq"))
train$score(measure=msr("regr.rmse"))
train$score(measure=msr("regr.mae"))
train$score(measure=msr("regr.bias"))

