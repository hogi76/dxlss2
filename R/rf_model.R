
# 3. 모델링 2 - RandomForest  ------

# 3-1. 모델수립

rf_model = function(dataset, y){
  y = deparse(substitute(y)) # 변수를 문자로 변환합니다

  set.seed(0)
  train = sample(nrow(dataset), nrow(dataset)*0.7) #데이터를 분리하기 위하여 행번호를 난수로 결정
  train_df = dataset[train, ] #훈련용 데이터
  test_df = dataset[-train, ] #검증용 데이터


  set.seed(1)
  rf.train_df = eval(parse(text = paste0('randomForest(',y,' ~. , data = train_df, importnace = T)'))) # 모델을 적합합니다
  importance(rf.train_df) # 인자의 중요도를 확인합니다
  varImpPlot(rf.train_df) # 인자의 중요도를 그래프로 확인합니다




  # 3-2. 결과를 예측합니다.
  pred = predict(rf.train_df, newdata = test_df) # 결과를 예측합니다

  B = eval(parse(text = paste0('rpart(',y,' ~. , data = train_df)'))) #그래프분석
  rpart.plot(B)



  if(eval(parse(text = paste0('is.numeric(dataset$',y,')')))){
    rf.MSE = eval(parse( text = paste0('MSE(pred , test_df$',y,')')))  # MSE를 확인합니다
    rf.R_sq = eval(parse( text = paste0('(cor(pred, test_df$',y,'))^2'))) # 설명력을 확인합니다

    return(list(model = rf.train_df,
                predict = pred,
                MSE = rf.MSE,
                R_sq = rf.R_sq))
  } else {
    confusionMatrix = eval(parse( text = paste0('caret::confusionMatrix(pred , test_df$',y,')')))  # 성능을 확인합니다


    return(list(model = rf.train_df,
                predict = pred,
                confusionMatrix = confusionMatrix))
  }
}


