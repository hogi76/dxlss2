prework1 = function(raw_dataset){
  mpg_numeric = raw_dataset %>% dplyr::select_if((is.numeric)) #numeric 열을 제외한 항목
  mpg_notnumeric = raw_dataset %>% dplyr::select_if((negate(is.numeric))) #numeric 열을 제외한 항목
  new_name = vector(length = length(mpg_notnumeric)) # 빈벡터 생성


  # ID형태의 문자열을 제외(제외기준 : NA열 또는 value의 category가 10개 이상이면 제외 예 : 날짜, 이름 외)
  for(i in seq_along(mpg_notnumeric)) {
    new_name[i] = ifelse(as.vector(mpg_notnumeric%>% dplyr::distinct(mpg_notnumeric[,i]) %>% count() > 10 |
                                     mpg_notnumeric[,i] %>% is.na() %>% all()), 'useless', colnames(mpg_notnumeric)[i])
  }


  colnames(mpg_notnumeric) = new_name
  if(any(new_name%in%'useless')){
    mpg_notnumeric = mpg_notnumeric %>%
      dplyr::select(-useless)
  }

  dataset = cbind(mpg_numeric,  mpg_notnumeric) #Numeric과 Factor열로 구성
  dataset = centralImputation(dataset) #NA값 제외

  return(dataset)

}


