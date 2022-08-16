# 모델링 ----------------------------------------------------------------------



lm_model = function(dataset, y){
  y = deparse(substitute(y)) # 변수를 문자로 변환합니다
  set.seed(0)
  train = sample(nrow(dataset), nrow(dataset)*0.7) #데이터를 분리하기 위하여 행번호를 난수로 결정
  train_df = dataset[train, ] #훈련용 데이터
  test_df = dataset[-train, ] #검증용 데이터



  # 2-1. 모델수립
  lm_best = eval(parse(text = paste0('lm(',y,'~., data = train_df)')))%>%
    step()%>%
    summary() # AIC가 가장 작은 마지막 모델을 확인하여, 아래에 대입합니다

  stepwise.fit = lm(formula = lm_best $ call , train_df) # 모델을 완성합니다.


  # 2-2. 모델의 잔차분석을 실시합니다.

  par(mfrow =c(2,2))
  plot(stepwise.fit)
  par(mfrow =c(1,1))



  # 2-3. 결과를 예측합니다.
  pred = predict(stepwise.fit, newdata = test_df) # 결과를 예측합니다
  lm.MSE = eval(parse( text = paste0('MSE(pred , test_df$',y,')')))  # MSE를 확인합니다
  lm.R_sq = eval(parse( text = paste0('(cor(pred, test_df$',y,'))^2'))) # 설명력을 확인합니다


  return(list(model = stepwise.fit %>% summary(),
              predict = pred,
              MSE = lm.MSE,
              R_sq = lm.R_sq))

}


