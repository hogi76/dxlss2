#' Graph the relationship between column y and the rest of the x columns
#'
#' @param data raw_dataset
#' @param y column you want to improve
#' @return Returns the result of xy_relation
#' @example
#' graph(airquality, Ozone)





# 관계형 그래프 분석 --------------------------------------------------------------

#전처리한 데이터의 관계를 확인합니다

graph = function(dataset, y) {


  y = deparse(substitute(y))

  for(i in seq_along(dataset)){
    print(
      eval(parse(text = paste0('ggplot(dataset, aes( x = dataset[,i], y =', y,')) +
            geom_point()+
            geom_smooth()+
            labs(x = colnames(dataset)[i])')))
    )
  }
}

