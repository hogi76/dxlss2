# 2.prework2 : outlier제거, 변수최적화 ------------------------------------------

prework2 = function(dataset, y) {

  # 1. rawdata에서 이상치를 확인합니다

  y = deparse(substitute(y))
  par(mfrow = c(1,2))
  a1 = eval(parse(text = paste0('boxplot(dataset$',y, ',main = "Outlier : Before")')))
  abline(h = (a1$stats[c(1, 5), ]), col = "red", lty = "dotted")
  print(dim(dataset))


  # 2. rawdata에서outlier를 제외합니다.

  repeat{
    dataset = eval(parse(text = paste0('dataset[!dataset$',y, '%in% boxplot.stats(dataset$',y,')$out,  ,drop = FALSE]')))
    if(eval(parse(text = paste0('length(boxplot.stats(dataset$',y,')$out)'))) == 0){
      break
    }
  }


  # 3. rawdata에서outlier가 제거되었는지를 확인합니다.

  a2 = eval(parse(text = paste0('boxplot(dataset$',y, ',main = "Outlier : After")')))
  abline(h = (a2$stats[c(1, 5), ]), col = "red", lty = "dotted")
  print(dim(dataset))


  # 4. 상관관계가 0.1이상인 열로만 분석을 진행, 먼저 문자열과 숫자열을 다시 분리

  RO_char3 =
    dataset %>%
    select_if(is.character)




  RO_num3 =
    dataset %>%
    select_if(is.numeric)



  RO_num3_twin = RO_num3

  # 5. 상관관계가  0.1이상인 값들만 인자로 선정


  k = 0.1
  repeat{
    for(i in seq_along(RO_num3)){
      RO_num3_twin[,i] = eval(parse(text = paste0('ifelse(abs(cor(RO_num3$',y,', RO_num3[,i])) >= k, RO_num3[,i], NA)')))
    }
    RO_num4 = RO_num3_twin %>%
      select_if(is.numeric)   # 상관관계가 0.1이상인 값들만 추출


    if(ncol(RO_num4)<10){ # 상관관계가 높은 10개의 아이템으로 최적화
      break
    }
    k = k + 0.01
  }


  RO_num5 = RO_num3[, colnames(RO_num4)] # 원본데이터 입력







  # 문자열과 숫자열을 통합 ------------------------------------------------------------



  RO_DF2 = cbind(RO_char3, RO_num5) # 문자열과 숫자열을 통합
  par(mfrow = c(1,1))
  print(dim(RO_DF2))
  return (RO_DF2)


}

