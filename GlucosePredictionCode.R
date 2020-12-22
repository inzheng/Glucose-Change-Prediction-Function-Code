#数据清理部分函数
#介绍：以缺失值50%为评判值去清理，超过50%缺失值则删除
#评判值可以通过数据集的不同进行修改
#OriginalData: 原始数据集
#返回清理好的数据集
columnCleaning<-function(OriginalData){
  library(dplyr)
  dataset<-OriginalData
  returnSet<-OriginalData
  number_column<-ncol(dataset)
  number_row<-nrow(dataset)
  name<-as.vector(colnames(dataset))
  for(i in 1:number_column){
    na<-sum(is.na(dataset[,i]))
    percetage_missing<-na/number_row
    if(percetage_missing>0.5){ #评判值
      dl_variable<-as.character(name[i])
      returnSet<- returnSet %>% select(-contains(dl_variable))
    }
  }
  return(returnSet)
}

#随机森林模型生成函数
#dataset: 生成随机森林模型的数据集 (数据集格式要求：最后一列为因变量)
#return: 返回随机森林模型
RandomforestModel <- function(dataset){
  library(randomForest)
  y.index<-length(dataset)
  set.seed(1)
  train.set <- dataset[sample(1:nrow(dataset), nrow(dataset)%/%3*2, replace = FALSE), ]
  test.set <- setdiff(dataset, train.set)
  model <- randomForest(train.set[, 1:(y.index - 1)], train.set[,y.index], replace=T, keep.forest=T)
  return(model)
}

#生成从大到小顺序特征值排列
#RFModel: 随机森林生成的模型
##return: 返回排列好的特征重要性
ImportanceRank <- function(RFModel){
  library(dplyr)
  importance <- RFModel$importance
  importance_1 <- importance[,1]
  feature <- rownames(importance)
  feature.importance <- data.frame(feature, importance_1)
  colnames(feature.importance) <- c('feature', 'weights')
  index.tmp <- order(feature.importance$weights, decreasing = T)
  feature <- as.character(feature.importance$feature)
  feature.importance <- cbind(feature[index.tmp[1:length(feature.importance$weights)]], 
                              feature.importance$weights[index.tmp[1:length(feature.importance$weights)]])
  feature.importance <- data.frame(feature.importance)
  colnames(feature.importance) <- c('feature', 'weights')
  rm(feature, importance, importance_1, index.tmp)
  return(feature.importance)
}

#通过最低RMSE值找最佳合集
#data: 生成最佳合集的原数据集
#feautre.importance: 原数据集的排列好的特征重要性
GenerateBestData <-function(data, feautre.importance){
  library(caret)
  library(pROC) 
  library(e1071)
  
  n <- length(data) - 1 
  col.number <- n
  rmse.total <- 0
  
  repeat{
    #判断是否删除特征
    if (col.number != n){
      dlv <- feautre.importance[n, 1]
      n <- n - 1        
      drop <- c(dlv)
      data <- data[ , !names(data) %in% drop]
    }else{
      col.number <- col.number - 1
    }
    rf.model <- RandomforestModel(data)
    set.seed(1)
    train.set <- data[sample(1:nrow(data), nrow(data)%/%3*2, replace = FALSE), ]
    test.set <- setdiff(data, train.set)
    result.preditc <- predict(rf.model, test.set[, 1:n])
    truth<-test.set$葡萄糖
    rmse.total<-rmse.total+sqrt(mean((result.preditc - truth)^2))
    if(n == 192){
      compareRmse <- rmse.total
      bestSet <- data
    }else{
      if(rmse.total < compareRmse){
        compareRmse <- rmse.total
        bestSet <- data
      }
    }
    #如果只剩两个合集则循环结束
    if(n == 2){
      Sys.time()
      break
    }
  }  
  return(bestSet)
}

