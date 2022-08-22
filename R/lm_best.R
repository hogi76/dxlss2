# optimal_value_LM --------------------------------------------------------


lm_best = function(dataset, y){
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

  ########

  data_numeric = dataset %>% select_if((is.numeric)) %>% select(-y) #numeric 열에서 y를 제외한 항목
  data_notnumeric = dataset %>% select_if((negate(is.numeric))) #numeric 열을 제외한 항목

  data_numeric2 = data_numeric[1:3, ] #빈 5 by ncol(dataset)매트릭스를 만듭니다 # 빈벡터 생성
  rownames(data_numeric2) = NULL # 행 이름을 제거합니다

  # 각 숫자 열 데이터의 범위를 5등분 합니다

  for(i in seq_along(data_numeric2)) {
    data_numeric2[,i] = seq(min(data_numeric[,i]),max(data_numeric[,i]),length.out = 3)
  }


  # 각 문자 열을 정의합니다

  rownames(data_notnumeric) = NULL

  new_DF = expand.grid(data_numeric2) # 숫자형 데이터의 그리드를 만듭니다

  for(i in seq_along(data_notnumeric)) {
    new_DF = merge(new_DF, data_notnumeric%>% unique()) # 숫자형과 factor형 전체 그리드를 만들기위하여 그리드를 추가합니다
  }


  ########

  # 3-2. 결과를 예측합니다.

  pred = predict(stepwise.fit, newdata = new_DF) # 결과를 예측합니다
  result = data.frame(new_DF, pred)
  bestcon_up =
    result %>% arrange(desc(pred)) %>% head(10)

  bestcon_down =
    result %>% arrange(pred) %>% head(10)

  return(list( best_condition_up= bestcon_up,
               best_condition_down =bestcon_down))



}

