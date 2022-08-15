oneshot_model = function(raw_dataset, y) {
  y = deparse(substitute(y))


  if(eval(parse(text = paste0('is.numeric(raw_dataset$',y,')')))){
    dataset_no_NA = prework1(raw_dataset)
    dataset_no_outlier = eval(parse(text = paste0('prework2(dataset_no_NA,', y,')')))
    graph = eval(parse(text = paste0('graph(dataset_no_outlier,', y,')')))
    linear_regression = eval(parse(text = paste0('lm_model(dataset_no_outlier,', y,')')))
    randomForest = eval(parse(text = paste0('rf_model(dataset_no_outlier,', y,')')))
    lm_best = eval(parse(text = paste0('lm_best(dataset_no_outlier,', y,')')))
    rf_best = eval(parse(text = paste0('rf_best(dataset_no_outlier,', y,')')))

  } else {
    dataset_no_NA = prework1(raw_dataset)
    graph = eval(parse(text = paste0('graph(dataset_no_NA,', y,')')))
    randomForest = eval(parse(text = paste0('rf_model(dataset_no_NA,', y,')')))

  }


  if(eval(parse(text = paste0('is.numeric(raw_dataset$',y,')')))){
    return (list(clean_dataset = dataset_no_outlier,relation_check = graph,
                 linear_regression = linear_regression,
                 randomForest = randomForest,
                 lm_best = lm_best,
                 rf_best = rf_best
    ))
  } else{
    return (list(clean_dataset = dataset_no_NA,relation_check = graph,
                 randomForest = randomForest

    ))
  }
}
