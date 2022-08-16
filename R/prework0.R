prework0 = function(){
    RequiredPackages = c('tidyverse', 'tree', 'randomForest', 'rpart', 'psych', 'caret', 'rpart.plot', 'DMwR2')
    for (i in RequiredPackages) {
      if (!i %in% row.names(installed.packages())) install.packages(i)
    }


    for (i in seq_along(RequiredPackages)){
      library(RequiredPackages[i])
    }

}


