# grid_data  -----------------------------------------------------------


grid_data = function(dataset, y){

  y = deparse(substitute(y))
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

  x1 = expand.grid(data_numeric2) # 숫자형 데이터의 그리드를 만듭니다

  for(i in seq_along(data_notnumeric)) {
    x1 = merge(x1, data_notnumeric%>% unique()) # 숫자형과 factor형 전체 그리드를 만들기위하여 그리드를 추가합니다
  }



  return(x1)


}


